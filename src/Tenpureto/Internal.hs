{-# LANGUAGE TupleSections #-}

module Tenpureto.Internal where

import           Data.Text.Prettyprint.Doc
import           Algebra.Graph.Export.Dot

import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI
import           Tenpureto.TemplateLoader

data TenpuretoException = TemplateBranchNotFoundException Text
                        | TenpuretoBranchNotCreated Text
                        | TenpuretoBranchNotDeleted Text
                        | TenpuretoEmptySelection
                        | TenpuretoEmptyChangeset
                        | TenpuretoNoPreviousMergedHeads
                        | TenpuretoGitException GitException
                        | TenpuretoUIException UIException
                        | TenpuretoTemplateLoaderException TemplateLoaderException
                        | MultipleExceptions [TenpuretoException]
                        | CancelledException

instance Pretty TenpuretoException where
    pretty (TemplateBranchNotFoundException branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not found"
    pretty (TenpuretoBranchNotCreated branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not created"
    pretty (TenpuretoBranchNotDeleted branch) =
        "Branch" <+> dquotes (pretty branch) <+> "not deleted"
    pretty TenpuretoEmptySelection =
        "Cannot create a project from an empty selection"
    pretty TenpuretoEmptyChangeset =
        "Cannot create a commit because the changeset is empty"
    pretty TenpuretoNoPreviousMergedHeads =
        "Cannot find information about previously merged template commits"
    pretty (TenpuretoGitException            e) = pretty e
    pretty (TenpuretoUIException             e) = pretty e
    pretty (TenpuretoTemplateLoaderException e) = pretty e
    pretty (MultipleExceptions ee) =
        "Multiple failures:\n" <> (align . vsep) (fmap pretty ee)
    pretty CancelledException = "Cancelled by a user"

exportDotStyle :: TemplateInformation -> Style TemplateBranchInformation Text
exportDotStyle templateInformation = Style
    { graphName               = ""
    , preamble                = mempty
    , graphAttributes         = ["rankdir" := "LR"]
    , defaultVertexAttributes = mempty
    , defaultEdgeAttributes   = mempty
    , vertexName              = branchName
    , vertexAttributes        = vertexAttributes'
    , edgeAttributes          = edgeAttributes'
    }
  where
    vertexAttributes' branch
        | isMergeBranch templateInformation branch
        = ["label" := "", circle, dotted]
        | isHiddenBranch branch
        = ["label" := branchName branch, box, dashed]
        | otherwise
        = ["label" := branchName branch, box]
    edgeAttributes' _ dst
        | isMergeBranch templateInformation dst = ["style" := "dotted"]
        | otherwise                             = []
    dotted = "style" := "dotted"
    dashed = "style" := "dashed"
    box    = "shape" := "box"
    circle = "shape" := "circle"
