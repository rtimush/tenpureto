{-# LANGUAGE TupleSections #-}

module Tenpureto.Effects.UI.Internal where

import           Polysemy

import           Algebra.Graph.ToGraph
import           Data.Functor
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Terminal
import           Tenpureto.Graph
import           Tenpureto.MergeOptimizer
import           Tenpureto.OrderedMap           ( OrderedMap )
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.TemplateLoader


inputTemplate :: Member TerminalInput r => Sem r Text
inputTemplate = ask "Template URL:" Nothing

inputTarget :: Members '[TerminalInput , FileSystem] r => Sem r (Path Abs Dir)
inputTarget = ask "Target directory:" Nothing <&> T.unpack >>= resolveDir

templateBranchesByNames
    :: TemplateInformation -> Set Text -> [TemplateBranchInformation]
templateBranchesByNames templateInformation =
    filterBranchesByNames (branchesInformation templateInformation)

filterBranchesByNames
    :: [TemplateBranchInformation] -> Set Text -> [TemplateBranchInformation]
filterBranchesByNames availableBranches branches =
    filter (flip Set.member branches . branchName) availableBranches

findBranchByName
    :: [TemplateBranchInformation] -> Text -> Maybe TemplateBranchInformation
findBranchByName availableBranches branch =
    find ((==) branch . branchName) availableBranches

branchSelection :: [TemplateBranchInformation] -> Set Text -> Set Text
branchSelection availableBranches selectedBranches =
    Set.unions
        $   requiredBranches
        <$> filterBranchesByNames availableBranches selectedBranches

preMergeBranches
    :: Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> TemplateYaml
preMergeBranches graph selectedBranches =
    foldr templateYamlUnion emptyTemplateYaml
        $ fmap mergeBranchesResultTemplateYaml
        $ runIdentity
        $ mergeBranchesGraph (const ())
                             (\_ _ _ -> return ())
                             graph
                             selectedBranches

inputBranchList
    :: Graph TemplateBranchInformation
    -> [TemplateBranchInformation]
    -> Set Text
    -> Doc AnsiStyle
inputBranchList graph availableBranches selectedBranches = vsep $ zipWith6
    renderLine
    selectedPrefixes
    branchIndexes
    branchNames
    descriptions
    stabilities
    notes
  where
    green  = annotate (color Green)
    red    = annotate (color Red)
    yellow = annotate (color Yellow)
    renderLine
        :: Doc AnsiStyle
        -> Text
        -> Text
        -> Text
        -> FeatureStability
        -> Text
        -> Doc AnsiStyle
    renderLine selectedPrefix index branch description stability note =
        " " <> selectedPrefix <+> pretty index <> ")" <+> hang
            4
            (   annotate (colorDull White) (pretty branch)
            <>  softline
            <>  softline
            <>  annotate (colorDull Green) (pretty description)
            <+> stabilityNote stability
            <>  annotate (color Black) (pretty note)
            )
    stabilityNote Stable       = ""
    stabilityNote Experimental = yellow "(experimental)"
    stabilityNote Deprecated   = yellow "(deprecated)"
    equalize texts =
        let w = maximum (fmap T.length texts)
        in  fmap (T.justifyLeft w ' ') texts
    branchIndexes =
        equalize $ T.pack . show . fst @Int <$> [1 ..] `zip` availableBranches
    selectedBranchInformations = Set.fromList
        $ filterBranchesByNames availableBranches selectedBranches
    mergedYaml           = preMergeBranches graph selectedBranchInformations
    transitivelySelected = (Set.map yamlFeatureName . yamlFeatures) mergedYaml
    conflictsWithSelected =
        flip Set.member (yamlConflicts mergedYaml) . branchName
    branchLineSelected branch =
        let name        = branchName branch
            selected    = name `Set.member` selectedBranches
            transitive  = name `Set.member` transitivelySelected
            conflicting = conflictsWithSelected branch
        in  case (selected, transitive, conflicting) of
                (True , _    , _   ) -> (green "✓", "")
                (False, True , _   ) -> (green "·", "[transitive dependency]")
                (_    , _    , True) -> (red "𐄂", "[conflict]")
                (False, False, _   ) -> (" ", "")
    (selectedPrefixes, notes) =
        unzip $ fmap branchLineSelected availableBranches
    branchNames  = equalize $ fmap branchName availableBranches
    descriptions = fmap (fromMaybe "" . featureDescription) availableBranches
    stabilities  = fmap featureStability availableBranches

branchByIndex
    :: [TemplateBranchInformation] -> Text -> Maybe TemplateBranchInformation
branchByIndex availableBranches index =
    let isIndex :: (Int, a) -> Bool
        isIndex = (==) index . T.pack . show . fst
    in  find isIndex ([1 ..] `zip` availableBranches) <&> snd

type InputBranchState
    = ([TemplateBranchInformation], Set TemplateBranchInformation, Maybe Text)

inputBranches
    :: Members '[Terminal , TerminalInput] r
    => Graph TemplateBranchInformation
    -> Set Text
    -> Sem r (Set TemplateBranchInformation)
inputBranches graph initialNameSelection = case initial of
    Left  initial' -> askUntil initial' request process
    Right auto     -> return auto
  where
    initialSelection :: Set TemplateBranchInformation
    initialSelection = Set.fromList
        $ filterBranchesByNames (vertexList graph) initialNameSelection

    roots :: [TemplateBranchInformation]
    roots = graphRoots graph

    available :: Set TemplateBranchInformation -> [TemplateBranchInformation]
    available selected =
        let
            fullSelection = filterBranchesByNames (vertexList graph)
                $ Set.unions (Set.map requiredBranches selected)
            reachableFromSelection =
                concatMap (`reachable` graph) fullSelection
        in
            nub $ roots <> reachableFromSelection

    autoselect
        :: InputBranchState
        -> Either InputBranchState (Set TemplateBranchInformation)
    autoselect ([single], selection, badInput)
        | Set.size selection == 0
        = let newSelection = Set.singleton single
          in  autoselect (available newSelection, newSelection, badInput)
        | Set.size selection == 1
        = Right selection
    autoselect other = Left other

    initial :: Either InputBranchState (Set TemplateBranchInformation)
    initial =
        autoselect (available initialSelection, initialSelection, Nothing)

    request :: InputBranchState -> (Doc AnsiStyle, Maybe Text)
    request (availableBranches, selected, badInput) =
        let
            doc =
                inputBranchList graph
                                availableBranches
                                (Set.map branchName selected)
                    <> "\n"
                    <> maybe
                           ""
                           (\x ->
                               dquotes (pretty x)
                                   <+> "is not a valid feature index. "
                           )
                           badInput
                    <> "Add or remove a feature:"
        in  (doc, Nothing)

    process
        :: InputBranchState
        -> Text
        -> Either InputBranchState (Set TemplateBranchInformation)
    process (_, selected, _) "" = Right selected
    process (availableBranches, selected, _) input =
        autoselect $ case branchByIndex availableBranches input of
            Just bi ->
                let newSelected = if bi `Set.member` selected
                        then bi `Set.delete` selected
                        else bi `Set.insert` selected
                in  (available newSelected, newSelected, Nothing)
            Nothing -> (availableBranches, selected, Just input)

withDefaults
    :: (Ord a, Ord b)
    => OrderedMap a b
    -> OrderedMap a b
    -> OrderedMap b b
    -> OrderedMap a b
withDefaults defaults vars replacements = OrderedMap.mapWithKey getDefault
                                                                defaults
  where
    getDefault k v = fromMaybe (replace v) (OrderedMap.lookup k vars)
    replace v = fromMaybe v $ OrderedMap.lookup v replacements

inputVariable :: Member TerminalInput r => (Text, Text) -> Sem r (Text, Text)
inputVariable (desc, name) = (desc, ) <$> ask (pretty desc) (Just name)

inputVariables
    :: Member TerminalInput r
    => OrderedMap Text Text
    -> Sem r (OrderedMap Text Text)
inputVariables vars =
    OrderedMap.fromList <$> traverse inputVariable (OrderedMap.toList vars)
