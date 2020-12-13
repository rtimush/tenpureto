{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.UI where

import           Polysemy
import           Polysemy.Error

import           Data.Bool
import           Data.Foldable
import           Data.Maybe
import qualified Data.Set                      as Set

import           Tenpureto.Data
import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Git          ( BranchRef )
import           Tenpureto.Effects.Terminal
import           Tenpureto.Effects.UI.Internal
import           Tenpureto.Messages
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.TemplateLoader

data UIException = UnattendedNotPossible Text
                 | NoBaseBranchesException
                 | NoPreviousCommitException

instance Pretty UIException where
    pretty (UnattendedNotPossible missing) =
        "Input required when running in an unattended mode:" <+> pretty missing
    pretty NoBaseBranchesException =
        "Repository does not contain template branches"
    pretty NoPreviousCommitException
        = "Cannot find a previous template commit, either the repository was not created with tenpureto or the commit history has been amended in an incompatible way"

data ConflictResolutionStrategy = AlreadyResolved | MergeTool

data UI m a where
    InputTemplateConfiguration ::PreliminaryProjectConfiguration -> UI m FinalTemplateConfiguration
    InputUpdateConfiguration ::PreliminaryProjectConfiguration -> UI m FinalUpdateConfiguration
    InputProjectConfiguration ::TemplateInformation -> PreliminaryProjectConfiguration -> UI m FinalProjectConfiguration
    InputResolutionStrategy ::Path Abs Dir -> [Path Rel File] -> UI m ConflictResolutionStrategy
    ConfirmShellToAmend ::UI m Bool
    ConfirmPush ::[BranchRef] -> [BranchRef] -> [BranchRef] -> UI m Bool
    ConfirmPullRequest ::[BranchRef] -> Int -> UI m Bool

makeSem ''UI

runUIInTerminal
    :: Members '[FileSystem , Error UIException , Terminal , TerminalInput] r
    => Sem (UI ': r) a
    -> Sem r a
runUIInTerminal = interpret $ \case

    InputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt, preTargetDirectory = mbtd }
        -> FinalTemplateConfiguration
            <$> maybe inputTemplate return mbt
            <*> maybe inputTarget   return mbtd

    InputUpdateConfiguration PreliminaryProjectConfiguration { prePreviousTemplateCommit = mbc }
        -> FinalUpdateConfiguration
            <$> maybe (throw NoPreviousCommitException) return mbc

    InputProjectConfiguration templateInformation providedConfiguration -> do
        let psb = fromMaybe mempty (preSelectedBranches providedConfiguration)
        let branches = filterTemplateBranches BranchFilterIsFeatureBranch
                                              templateInformation
        sbi <- inputBranches branches psb
        let sbvars = foldr OrderedMap.union OrderedMap.empty
                $ fmap branchVariables (Set.toList sbi)
        let
            cvars = fromMaybe OrderedMap.empty
                              (preVariableValues providedConfiguration)
        let vars = withDefaults
                sbvars
                cvars
                (preVariableDefaultReplacements providedConfiguration)
        varVals <- inputVariables vars
        return FinalProjectConfiguration { projectBranches = Set.toList sbi
                                         , variableValues  = varVals
                                         }

    InputResolutionStrategy repo mergeConflicts -> do
        sayLn "The following files have merge conflicts:"
        traverse_ (\c -> sayLn ("  " <> pretty c)) mergeConflicts
        sayLn $ "Repository path: " <> pretty repo
        result <- confirm "Run \"git mergetool\"" (Just True)
        return $ bool AlreadyResolved MergeTool result

    ConfirmShellToAmend -> confirm confirmShellToAmendMessage (Just False)

    ConfirmPush deletes creates updates ->
        confirm (confirmPushMessage deletes creates updates) (Just False)

    ConfirmPullRequest updates cleanups ->
        confirm (confirmPullRequestMessage updates cleanups) (Just False)

runUIUnattended :: Member (Error UIException) r => Sem (UI ': r) a -> Sem r a
runUIUnattended = interpret $ \case

    InputTemplateConfiguration PreliminaryProjectConfiguration { preSelectedTemplate = mbt, preTargetDirectory = mbtd }
        -> FinalTemplateConfiguration
            <$> maybe (notPossible "template name")    return mbt
            <*> maybe (notPossible "target directory") return mbtd

    InputUpdateConfiguration PreliminaryProjectConfiguration { prePreviousTemplateCommit = mbc }
        -> FinalUpdateConfiguration
            <$> maybe (notPossible "previous template commit") return mbc

    InputProjectConfiguration templateInformation providedConfiguration ->
        FinalProjectConfiguration
            <$> maybe (notPossible "selected branches")
                      (return . templateBranchesByNames templateInformation)
                      (preSelectedBranches providedConfiguration)
            <*> maybe (notPossible "variable values")
                      return
                      (preVariableValues providedConfiguration)

    InputResolutionStrategy _ _ -> notPossible "merge conflicts"

    ConfirmShellToAmend         -> return False

    ConfirmPush{}               -> return True

    ConfirmPullRequest{}        -> return True

  where
    notPossible :: Member (Error UIException) r => Text -> Sem r a
    notPossible msg = throw $ UnattendedNotPossible msg
