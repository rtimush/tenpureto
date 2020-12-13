module Tenpureto.Data where

import           Data.Set                       ( Set )

import           Control.Applicative

import           Tenpureto.Effects.Git
import           Tenpureto.Effects.Logging
import           Tenpureto.OrderedMap           ( OrderedMap )
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.Orphanage            ( )
import           Tenpureto.TemplateLoader

data PreliminaryProjectConfiguration = PreliminaryProjectConfiguration
    { preSelectedTemplate            :: Maybe Text
    , preTargetDirectory             :: Maybe (Path Abs Dir)
    , prePreviousTemplateCommit      :: Maybe ParentCommit
    , preSelectedBranches            :: Maybe (Set Text)
    , preVariableValues              :: Maybe (OrderedMap Text Text)
    , preVariableDefaultReplacements :: OrderedMap Text Text
    , prePreviousMergedHeads         :: Maybe [Committish]
    }
    deriving Show

data FinalTemplateConfiguration = FinalTemplateConfiguration
    { selectedTemplate :: Text
    , targetDirectory  :: Path Abs Dir
    }
    deriving Show

newtype FinalUpdateConfiguration = FinalUpdateConfiguration
        { previousTemplateCommit :: ParentCommit
        }
        deriving (Show)

data FinalProjectConfiguration = FinalProjectConfiguration
    { projectBranches :: [TemplateBranchInformation]
    , variableValues  :: OrderedMap Text Text
    }
    deriving Show


instance Semigroup PreliminaryProjectConfiguration where
    (<>) a b = PreliminaryProjectConfiguration
        { preSelectedTemplate = preSelectedTemplate a <|> preSelectedTemplate b
        , preTargetDirectory = preTargetDirectory a <|> preTargetDirectory b
        , prePreviousTemplateCommit = prePreviousTemplateCommit a
                                          <|> prePreviousTemplateCommit b
        , preSelectedBranches = preSelectedBranches a <|> preSelectedBranches b
        , preVariableValues = preVariableValues a <|> preVariableValues b
        , preVariableDefaultReplacements = preVariableDefaultReplacements a
            `OrderedMap.union` preVariableDefaultReplacements b
        , prePreviousMergedHeads = prePreviousMergedHeads a
                                       <|> prePreviousMergedHeads b
        }

instance Pretty PreliminaryProjectConfiguration where
    pretty cfg = (align . vsep)
        [ "Template:                      "
            <+> (align . pretty) (preSelectedTemplate cfg)
        , "Target directory:              "
            <+> (align . pretty) (preTargetDirectory cfg)
        , "Previous template commit:      "
            <+> (align . pretty . show) (prePreviousTemplateCommit cfg)
        , "Selected branches:             "
            <+> (align . pretty) (preSelectedBranches cfg)
        , "Variable values:               "
            <+> (align . pretty) (preVariableValues cfg)
        , "Variable default replacements: "
            <+> (align . pretty) (preVariableDefaultReplacements cfg)
        , "Previous merged heads: "
            <+> (align . pretty) (prePreviousMergedHeads cfg)
        ]

instance Pretty FinalProjectConfiguration where
    pretty cfg = (align . vsep)
        [ "Branches:       " <+> (align . pretty) (projectBranches cfg)
        , "Variable values:" <+> (align . pretty) (variableValues cfg)
        ]

instance Pretty FinalUpdateConfiguration where
    pretty cfg = (align . vsep)
        [ "Previous template commit:"
              <+> (align . pretty) (previousTemplateCommit cfg)
        ]
