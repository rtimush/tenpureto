{-# LANGUAGE DeriveFunctor #-}

module Tenpureto.MergeOptimizer
    ( MergedBranchInformation(..)
    , MergedBranchDescriptor(..)
    , MergeBranchesResult(..)
    , descriptorToTemplateYaml
    , mergeBranchesGraph
    , mergeGraph
    , propagateBranchesGraph
    ) where

import           Control.Monad
import           Data.Maybe
import           Data.Semigroup.Foldable
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )

import           Tenpureto.Graph
import           Tenpureto.OrderedMap           ( OrderedMap )
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.TemplateLoader       ( TemplateBranchInformation(..)
                                                , TemplateYaml(..)
                                                , TemplateYamlFeature
                                                , branchVariables
                                                , isFeatureBranch
                                                , isHiddenBranch
                                                )

data MergedBranchInformation a = MergedBranchInformation
    { mergedBranchMeta       :: a
    , mergedBranchDescriptor :: MergedBranchDescriptor
    }
    deriving (Show, Eq, Ord, Functor)

data MergedBranchDescriptor = MergedBranchDescriptor
    { mergedBranchName :: Text
    , mergedVariables  :: OrderedMap Text Text
    , mergedExcludes   :: Set Text
    , mergedConflicts  :: Set Text
    , mergedFeatures   :: Set TemplateYamlFeature
    }
    deriving (Show, Eq, Ord)

data MergeBranchesResult a = MergeBranchesResult
    { mergeBranchesResultMeta         :: a
    , mergeBranchesResultTemplateYaml :: TemplateYaml
    }
    deriving (Show, Eq)

descriptorToTemplateYaml :: MergedBranchDescriptor -> TemplateYaml
descriptorToTemplateYaml d = TemplateYaml { yamlVariables = mergedVariables d
                                          , yamlFeatures  = mergedFeatures d
                                          , yamlExcludes  = mergedExcludes d
                                          , yamlConflicts = mergedConflicts d
                                          }

mergedBranchInformationToResult
    :: MergedBranchInformation a -> MergeBranchesResult a
mergedBranchInformationToResult mbi = MergeBranchesResult
    { mergeBranchesResultMeta         = mergedBranchMeta mbi
    , mergeBranchesResultTemplateYaml = descriptorToTemplateYaml mbd
    }
    where mbd = mergedBranchDescriptor mbi

templateBranchInformationData
    :: (TemplateBranchInformation -> a)
    -> TemplateBranchInformation
    -> MergedBranchInformation a
templateBranchInformationData extract bi = MergedBranchInformation
    { mergedBranchMeta       = extract bi
    , mergedBranchDescriptor = MergedBranchDescriptor
        { mergedBranchName = branchName bi
        , mergedVariables  = branchVariables bi
        , mergedExcludes   = (yamlExcludes . templateYaml) bi
        , mergedConflicts  = (yamlConflicts . templateYaml) bi
        , mergedFeatures   = (yamlFeatures . templateYaml) bi
        }
    }

mergeBranchesGraph
    :: (Ord a, Monad m)
    => (TemplateBranchInformation -> a)
    -> (a -> a -> MergedBranchDescriptor -> m a)
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> m (Maybe (MergeBranchesResult a))
mergeBranchesGraph extract mergeCommits graph selectedBranches =
    fmap (fmap mergedBranchInformationToResult)
        $ mergeGraph mergeCommits
        $ mapVertices (templateBranchInformationData extract)
        $ graphSubset (vertexDecision selectedBranches) graph

propagateBranchesGraph
    :: (Ord a, Monoid b, Monad m)
    => (TemplateBranchInformation -> a)
    -> (MergedBranchInformation a -> a -> m (a, b))
    -> (MergedBranchInformation a -> m b)
    -> Graph TemplateBranchInformation
    -> Set TemplateBranchInformation
    -> m b
propagateBranchesGraph extract propagateOne propagateMerge graph selectedBranches
    = propagateGraph propagateOne' propagateMerge'
        $ mapVertices (templateBranchInformationData extract') graph
  where
    extract' bi = (extract bi, branchName bi)
    branchNames = Set.map branchName selectedBranches
    name        = mergedBranchName . mergedBranchDescriptor
    propagateOne' mi (a, aname)
        | (name mi `Set.member` branchNames) || (aname `Set.member` branchNames) = do
            (a', b) <- propagateOne (fmap fst mi) a
            return ((a', name mi), b)
        | otherwise = return (mergedBranchMeta mi, mempty)
    propagateMerge' mi = propagateMerge (fmap fst mi)

vertexDecision
    :: Set TemplateBranchInformation
    -> TemplateBranchInformation
    -> GraphSubsetDecision
vertexDecision selectedBranches v | v `Set.member` selectedBranches = MustKeep
                                  | isHiddenBranch v                = PreferDrop
                                  | isFeatureBranch v               = MustDrop
                                  | otherwise                       = PreferKeep

mergeGraph
    :: (Ord a, Monad m)
    => (a -> a -> MergedBranchDescriptor -> m a)
    -> Graph (MergedBranchInformation a)
    -> m (Maybe (MergedBranchInformation a))
mergeGraph mergeCommits = foldTopologically vcombine hcombine
  where
    hcombineD d1 d2 = MergedBranchDescriptor
        { mergedBranchName = mergedBranchName d1 <> "+" <> mergedBranchName d2
        , mergedVariables  = mergedVariables d1
                                 `OrderedMap.union` mergedVariables d2
        , mergedExcludes   = mergedExcludes d1 <> mergedExcludes d2
        , mergedConflicts  = mergedConflicts d1 <> mergedConflicts d2
        , mergedFeatures   = mergedFeatures d1 <> mergedFeatures d2
        }
    combine combineD b1 b2 =
        let d = combineD (mergedBranchDescriptor b1)
                         (mergedBranchDescriptor b2)
        in  do
                c <- mergeCommits (mergedBranchMeta b1) (mergedBranchMeta b2) d
                return $ MergedBranchInformation { mergedBranchMeta       = c
                                                 , mergedBranchDescriptor = d
                                                 }
    vcombine v _ = return v
    hcombine = foldlM1 (combine hcombineD)

propagateGraph
    :: (Ord a, Monoid b, Monad m)
    => (MergedBranchInformation a -> a -> m (a, b))
    -> (MergedBranchInformation a -> m b)
    -> Graph (MergedBranchInformation a)
    -> m b
propagateGraph propagateOne propagateMerge graph =
    fromMaybe mempty <$> foldTopologically vcombine hcombine graph
  where
    vcombine v ps = do
        (combined, acc) <- foldM vcombineOne (v, mempty) ps
        acc'            <- propagateMerge combined
        return (combined, acc' <> acc)
    vcombineOne (v, vacc) (p, pacc) =
        let md = vcombineD (mergedBranchDescriptor v)
                           (mergedBranchDescriptor p)
        in  do
                (v', acc') <- propagateOne
                    (MergedBranchInformation (mergedBranchMeta v) md)
                    (mergedBranchMeta p)
                return (MergedBranchInformation v' md, acc' <> vacc <> pacc)
    hcombine = return . foldMap snd

vcombineD
    :: MergedBranchDescriptor
    -> MergedBranchDescriptor
    -> MergedBranchDescriptor
vcombineD d1 d2 = MergedBranchDescriptor
    { mergedBranchName = mergedBranchName d1
    , mergedVariables = mergedVariables d1 `OrderedMap.union` mergedVariables d2
    , mergedExcludes   = mergedExcludes d1 <> mergedExcludes d2
    , mergedConflicts  = mergedConflicts d1
    , mergedFeatures   = mergedFeatures d1 <> mergedFeatures d2
    }
