{-# LANGUAGE LambdaCase, BlockArguments #-}

module Tenpureto.FeatureMerger
    ( MergeRecord(..)
    , PropagatePushMode(..)
    , withMergeCache
    , runMergeGraphPure
    , runMergeGraph
    , listMergeCombinations
    , runPropagateGraph
    ) where

import           Polysemy
import           Polysemy.Output
import           Polysemy.State

import           Algebra.Graph.ToGraph
import           Control.Monad
import           Data.Functor
import           Data.List
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Tenpureto.Effects.Git
import           Tenpureto.Effects.Terminal
import           Tenpureto.Effects.UI
import           Tenpureto.FeatureMerger.Internal
import           Tenpureto.Graph
import           Tenpureto.MergeOptimizer
import           Tenpureto.Messages
import           Tenpureto.OrderedSet           ( OrderedSet )
import qualified Tenpureto.OrderedSet          as OrderedSet
import           Tenpureto.TemplateLoader

data MergeRecord = CheckoutRecord Text
                 | MergeRecord Text Text Text
    deriving (Show, Eq)

mergeCommits
    :: Members '[Git , UI , Terminal] r
    => GitRepository
    -> MergeElement
    -> MergeElement
    -> MergedBranchDescriptor
    -> Sem r MergeElement
mergeCommits repo (MergeElement b1c b1n b1t b1hs) (MergeElement b2c b2n b2t b2hs) d
    = do
        let mergedTree = Node (mergedBranchName d) [b2t, b1t]
        let message =
                commitMergeMessage b2n b1n <> "\n\n" <> showTree mergedTree
        checkoutBranch repo (unCommittish b1c) Nothing
        mergeResult <- mergeBranch repo
                                   MergeAllowFastForward
                                   (unCommittish b2c)
                                   message
        c <- case mergeResult of
            MergeSuccessCommitted   -> getCurrentHead repo
            MergeSuccessUncommitted -> do
                updateTemplateYaml
                commit repo message
            MergeConflicts mergeConflicts -> do
                updateTemplateYaml
                resolve d mergeConflicts
                commit repo message

        return $ MergeElement c
                              (mergedBranchName d)
                              mergedTree
                              (b1hs `OrderedSet.union` b2hs)
  where
    resolve _ [] = return ()
    resolve descriptor mergeConflicts =
        if templateYamlFile `elem` mergeConflicts
            then resolve descriptor (delete templateYamlFile mergeConflicts)
            else
                inputResolutionStrategy (repositoryPath repo) mergeConflicts
                    >>= \case
                            AlreadyResolved -> return ()
                            MergeTool ->
                                runMergeTool repo >> sayLn mergeSuccess
    updateTemplateYaml = writeAddFile
        repo
        templateYamlFile
        (formatTemplateYaml (descriptorToTemplateYaml d))

withMergeCache :: Sem (State MergeCache ': r) a -> Sem r a
withMergeCache = fmap snd . runState mempty

runMergeGraph
    :: Members '[Git , UI , Terminal , State MergeCache] r
    => GitRepository
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> Sem r (Maybe (TemplateYaml, OrderedSet Committish))
runMergeGraph repo graph branches = do
    mbd <- mergeBranchesGraph branchData mergeCommitsCached graph branches
    forM_ mbd $ \case
        MergeBranchesResult (MergeElement c _ _ _) d -> do
            checkoutBranch repo (unCommittish c) Nothing
            writeAddFile repo templateYamlFile (formatTemplateYaml d)
            commit repo commitUpdateTemplateYaml
    return $ fmap runResult mbd
  where
    mergeCommitsCached a b d = do
        cache <- get
        case Map.lookup (a, b) cache of
            Just r  -> return r
            Nothing -> do
                r <- mergeCommits repo a b d
                modify (Map.insert (a, b) r)
                return r
    branchData bi = MergeElement
        { mergeHead  = branchCommit bi
        , mergeName  = branchName bi
        , mergeTree  = Leaf (branchName bi)
        , mergeHeads = OrderedSet.singleton (branchCommit bi)
        }
    runResult mbd =
        ( mergeBranchesResultTemplateYaml mbd
        , (mergeHeads . mergeBranchesResultMeta) mbd
        )

runMergeGraphPure
    :: Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> ([MergeRecord], Maybe (TemplateYaml, OrderedSet Committish))
runMergeGraphPure graph selectedBranches =
    let
        (records, c) = run . runOutputMonoid pure $ mergeBranchesGraph
            branchData
            logMerges
            graph
            selectedBranches
        co = maybeToList
            $ fmap (CheckoutRecord . fst . mergeBranchesResultMeta) c
    in
        (records <> co, fmap runResult c)
  where
    branchData bi = (branchName bi, OrderedSet.singleton (branchCommit bi))
    logMerges (b1, b1hs) (b2, b2hs) d =
        let mc = mergedBranchName d
        in  output (MergeRecord b1 b2 mc) $> (mc, b1hs `OrderedSet.union` b2hs)
    runResult mbd =
        ( mergeBranchesResultTemplateYaml mbd
        , (snd . mergeBranchesResultMeta) mbd
        )

listMergeCombinations
    :: Graph TemplateBranchInformation -> [Set TemplateBranchInformation]
listMergeCombinations graph =
    let
        selectable branch =
            not (isHiddenBranch branch) && isFeatureBranch branch
        nodes        = filter selectable $ vertexList graph
        combinations = subsequences nodes
        addAncestors = filter selectable . graphAncestors graph
        noConflicts selected =
            let
                conflicts :: Set Text
                conflicts =
                    ( maybe mempty yamlConflicts
                        . fmap fst
                        . snd
                        . runMergeGraphPure graph
                        )
                        selected
                selectedNames = Set.map branchName selected
            in
                Set.null (Set.intersection conflicts selectedNames)
    in
        filter noConflicts
        $   Set.toList
        .   Set.fromList
        $   fmap Set.fromList
        $   filter (not . null)
        $   addAncestors
        <$> combinations

data PropagatePushMode = PropagatePushMerged | PropagatePushSeparately
data PropagateData = PropagateData
    { propagateCurrentCommit  :: Committish
    , propagateUpstreamCommit :: Committish
    , propagateBranchName     :: Text
    }
    deriving (Eq, Ord, Show)

runPropagateGraph
    :: Members '[Git , Terminal , State MergeCache] r
    => GitRepository
    -> PropagatePushMode
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> Sem r [PushSpec]
runPropagateGraph repo mode graph branches =
    Set.toList
        <$> propagateBranchesGraph branchData
                                   (propagateOne mode)
                                   (propagateMerge mode)
                                   graph
                                   branches
  where
    branchData bi = PropagateData { propagateCurrentCommit  = branchCommit bi
                                  , propagateUpstreamCommit = branchCommit bi
                                  , propagateBranchName     = branchName bi
                                  }
    mergeOne mid a = do
        needsMerge <- gitDiffHasCommits repo
                                        (propagateCurrentCommit a)
                                        (propagateUpstreamCommit mid)
        if not needsMerge
            then return
                ( propagateCurrentCommit mid
                , Set.singleton $ CloseBranchUpdate
                    { destinationRef = BranchRef $ propagateBranchName mid
                    , pullRequestRef = BranchRef
                                       $  propagateBranchName a
                                       <> "/"
                                       <> propagateBranchName mid
                    }
                )
            else do
                checkoutBranch repo
                               (unCommittish $ propagateCurrentCommit mid)
                               Nothing
                let title = pullRequestBranchIntoBranchTitle
                        (propagateBranchName a)
                        (propagateBranchName mid)
                preMergeResult <- mergeBranch
                    repo
                    MergeAllowFastForward
                    (unCommittish $ propagateCurrentCommit a)
                    title
                let
                    success c =
                        ( c
                        , Set.singleton $ UpdateBranch
                            { sourceCommit     = c
                            , sourceRef = BranchRef $ propagateBranchName a
                            , destinationRef   = BranchRef
                                                     $ propagateBranchName mid
                            , pullRequestRef   = BranchRef
                                                 $  propagateBranchName a
                                                 <> "/"
                                                 <> propagateBranchName mid
                            , pullRequestTitle = title
                            }
                        )
                case preMergeResult of
                    MergeSuccessCommitted   -> success <$> getCurrentHead repo
                    MergeSuccessUncommitted -> success <$> commit repo title
                    MergeConflicts _ ->
                        mergeAbort repo $> success (propagateCurrentCommit a)
    propagateOne PropagatePushSeparately mi a =
        let mid = mergedBranchMeta mi
        in  do
                (_, actions) <- mergeOne mid a
                return
                    ( PropagateData
                        { propagateCurrentCommit  = propagateCurrentCommit mid
                        , propagateUpstreamCommit = propagateUpstreamCommit mid
                        , propagateBranchName     = propagateBranchName mid
                        }
                    , actions
                    )
    propagateOne PropagatePushMerged mi a =
        let mid = mergedBranchMeta mi
        in  do
                (c, _) <- mergeOne mid a
                return
                    ( PropagateData
                        { propagateCurrentCommit  = c
                        , propagateUpstreamCommit = propagateUpstreamCommit mid
                        , propagateBranchName     = propagateBranchName mid
                        }
                    , Set.empty
                    )
    propagateMerge PropagatePushMerged b =
        let
            bd        = mergedBranchMeta b
            needsPush = propagateCurrentCommit bd /= propagateUpstreamCommit bd
        in
            return $ Set.fromList
                [ UpdateBranch
                      { sourceCommit     = propagateCurrentCommit bd
                      , sourceRef        = BranchRef $ propagateBranchName bd
                      , destinationRef   = BranchRef $ propagateBranchName bd
                      , pullRequestRef   = BranchRef
                                           $  propagateBranchName bd
                                           <> "/"
                                           <> propagateBranchName bd
                      , pullRequestTitle = pullRequestBranchUpdateTitle
                                               (propagateBranchName bd)
                      }
                | needsPush
                ]
    propagateMerge PropagatePushSeparately _ = return mempty
