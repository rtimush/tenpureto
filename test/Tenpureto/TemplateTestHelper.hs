module Tenpureto.TemplateTestHelper where

import qualified Data.Set                      as Set
import           Data.Text                      ( Text )

import           Tenpureto.Effects.Git          ( Committish(..) )
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.TemplateLoader       ( FeatureStability(..)
                                                , TemplateBranchInformation(..)
                                                , requiredBranches
                                                )
import           Tenpureto.TemplateLoader.Internal
                                                ( TemplateYaml(..)
                                                , TemplateYamlFeature(..)
                                                )

yamlFeature :: Text -> TemplateYamlFeature
yamlFeature name = TemplateYamlFeature { yamlFeatureName        = name
                                       , yamlFeatureDescription = Nothing
                                       , yamlFeatureHidden      = False
                                       , yamlFeatureStability   = Stable
                                       }

anonymousBranch :: Text -> [Text] -> TemplateBranchInformation
anonymousBranch name deps = TemplateBranchInformation
    { branchName   = name
    , branchCommit = Committish "undefined"
    , templateYaml = TemplateYaml
                         { yamlVariables = OrderedMap.empty
                         , yamlFeatures  = Set.fromList $ fmap yamlFeature deps
                         , yamlExcludes  = mempty
                         , yamlConflicts = mempty
                         }
    }

branch :: Text -> [Text] -> TemplateBranchInformation
branch name deps = anonymousBranch name (name : deps)

baseBranch :: Text -> TemplateBranchInformation
baseBranch name = branch name []

childBranch :: Text -> [TemplateBranchInformation] -> TemplateBranchInformation
childBranch name parents =
    branch name (Set.toList $ mconcat $ fmap requiredBranches parents)

renamedBranch :: Text -> TemplateBranchInformation -> TemplateBranchInformation
renamedBranch name parent =
    anonymousBranch name (Set.toList $ requiredBranches parent)

mergeBranch :: Text -> [TemplateBranchInformation] -> TemplateBranchInformation
mergeBranch name branches =
    anonymousBranch name (Set.toList $ mconcat $ fmap requiredBranches branches)
