{-# LANGUAGE LambdaCase #-}

module Main where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Resource

import           Data.Foldable
import           Data.Functor
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Version
import           Options.Applicative

import           System.Exit

import           Tenpureto
import           Tenpureto.Data
import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Git
import           Tenpureto.Effects.Logging
import           Tenpureto.Effects.Process
import           Tenpureto.Effects.Terminal
import           Tenpureto.Effects.UI
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.TemplateLoader

import           Paths_tenpureto                ( version )


data Command
    = Create
            { maybeTemplateName :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , maybeProjectConfiguration :: Maybe FilePath
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | Update
            { maybeTemplateName :: Maybe Text
            , maybePreviousCommit :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , maybeProjectConfiguration :: Maybe FilePath
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | Adopt
            { maybeTemplateName :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , maybeProjectConfiguration :: Maybe FilePath
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | Log
            { maybeTemplateName :: Maybe Text
            , maybeTargetDirectory :: Maybe FilePath
            , maybeProjectConfiguration :: Maybe FilePath
            , showLogMode :: ShowLogMode
            , enableDebugLogging :: Bool
            }
    | TemplateGraph
            { templateName :: Text
            , branchFilter :: BranchFilter
            , enableDebugLogging :: Bool
            }
    | TemplateListBranches
            { templateName :: Text
            , branchFilter :: BranchFilter
            , enableDebugLogging :: Bool
            }
    | TemplateRenameBranch
            { templateName :: Text
            , oldBranchName :: Text
            , newBranchName :: Text
            , enableInteractivity :: Bool
            , enableDebugLogging :: Bool
            }
    | TemplatePropagateChanges
            { templateName :: Text
            , branchFilter :: BranchFilter
            , remoteChangeMode :: RemoteChangeMode
            , runUnattended :: Bool
            , enableDebugLogging :: Bool
            }
    | TemplateListConflicts
            { templateName :: Text
            , enableDebugLogging :: Bool
            }
    | TemplateChangeVariable
            { templateName :: Text
            , oldVariableValue :: Text
            , newVariableValue :: Text
            , enableInteractivity :: Bool
            , enableDebugLogging :: Bool
            }

templateNameOption :: Parser Text
templateNameOption = strOption
    (long "template" <> metavar "<repository>" <> help
        "Template repository name or URL"
    )

previousCommitOption :: Parser Text
previousCommitOption = strOption
    (long "previous-commit" <> metavar "<sha1>" <> help
        "Override the commit in which the template was previously generated"
    )

targetArgument :: Parser FilePath
targetArgument = strArgument
    (metavar "<dir>" <> help "Target directory" <> action "directory")

projectConfiguratonOption :: Parser FilePath
projectConfiguratonOption = strOption
    (  long "configuration"
    <> metavar "<file>"
    <> help "Project configuration"
    <> action "file"
    )

unattendedSwitch :: Parser Bool
unattendedSwitch = switch (long "unattended" <> help "Do not ask anything")

interactiveSwitch :: Parser Bool
interactiveSwitch = switch
    (  long "interactive"
    <> help "Stop before committing to allow manual intervention"
    )

debugSwitch :: Parser Bool
debugSwitch = switch (long "debug" <> help "Print debug information")

versionOption :: Parser (a -> a)
versionOption = infoOption
    ("tenpureto " ++ showVersion version)
    (short 'v' <> long "version" <> help "Show the program version" <> hidden)

oldBranchNameOption :: Parser Text
oldBranchNameOption =
    strOption (long "old-name" <> metavar "<branch>" <> help "Old branch name")

newBranchNameOption :: Parser Text
newBranchNameOption =
    strOption (long "new-name" <> metavar "<branch>" <> help "New branch name")

branchFilterNameOption :: Parser BranchFilter
branchFilterNameOption = BranchFilterEqualTo <$> strOption
    (long "branch" <> metavar "<branch>" <> help "Branch name to filter against"
    )

branchFilterNamesOption :: Parser BranchFilter
branchFilterNamesOption = many branchFilterNameOption <&> \case
    [] -> BranchFilterAny
    bs -> BranchFilterOr bs

branchFilterChildOfOption :: Parser BranchFilter
branchFilterChildOfOption = BranchFilterChildOf <$> strOption
    (long "children-of" <> metavar "<branch>" <> help
        "Child branches of a given branch"
    )

branchFilterParentOfOption :: Parser BranchFilter
branchFilterParentOfOption = BranchFilterParentOf <$> strOption
    (long "parents-of" <> metavar "<branch>" <> help
        "Parent branches of a given branch"
    )

branchFilterOptions :: Parser BranchFilter
branchFilterOptions = BranchFilterAnd <$> (fmap catMaybes . traverse optional)
    [branchFilterChildOfOption, branchFilterParentOfOption]

branchTypeIncludeHiddenFlag :: Parser BranchFilter
branchTypeIncludeHiddenFlag = flag
    BranchFilterNone
    BranchFilterIsHiddenBranch
    (long "include-hidden" <> help "Include hidden branches")

branchTypeIncludeMergesFlag :: Parser BranchFilter
branchTypeIncludeMergesFlag = flag
    BranchFilterNone
    BranchFilterIsMergeBranch
    (long "include-merges" <> help "Include merge branches")

branchTypeFilterOption :: Parser BranchFilter
branchTypeFilterOption = BranchFilterOr <$> sequenceA
    [ pure BranchFilterIsFeatureBranch
    , branchTypeIncludeHiddenFlag
    , branchTypeIncludeMergesFlag
    ]

oldVariableValueOption :: Parser Text
oldVariableValueOption = strOption
    (long "old-value" <> metavar "<value>" <> help "Old variable value")

newVariableValueOption :: Parser Text
newVariableValueOption = strOption
    (long "new-value" <> metavar "<value>" <> help "New variable value")

pullRequestFlag :: Parser ()
pullRequestFlag = flag'
    ()
    (  long "pull-request"
    <> help "Create a pull request instead of merging directly"
    )

pullRequestMergeFlag :: Parser Bool
pullRequestMergeFlag = switch
    (  long "pull-request-merge-commit"
    <> help "Create a pull request from a merge commit"
    )

pullRequestLabelOption :: Parser Text
pullRequestLabelOption = strOption
    (long "pull-request-label" <> metavar "<name>" <> help
        "Assign a label to a pull request (requires --pull-request)"
    )

pullRequestAssignOption :: Parser Text
pullRequestAssignOption = strOption
    (long "pull-request-assignee" <> metavar "<user>" <> help
        "Assign a pull request to a user (requires --pull-request)"
    )

pullRequestSettingsOptionSet :: Parser PullRequestSettings
pullRequestSettingsOptionSet =
    PullRequestSettings
        <$> many pullRequestLabelOption
        <*> many pullRequestAssignOption
        <*> pullRequestMergeFlag

remoteChangeModeOptionSet :: Parser RemoteChangeMode
remoteChangeModeOptionSet = asum
    [ UpstreamPullRequest <$> (pullRequestFlag *> pullRequestSettingsOptionSet)
    , pure PushDirectly
    ]

showLogModeOption :: Parser ShowLogMode
showLogModeOption = incoming <|> included <|> pure ShowLogAll
  where
    incoming = flag' ShowLogIncoming
                     (long "incoming" <> help "Show only incoming changes")
    included = flag'
        ShowLogIncluded
        (long "included" <> help "Show only already included changes")

createCommand :: Parser Command
createCommand =
    Create
        <$> optional templateNameOption
        <*> optional targetArgument
        <*> optional projectConfiguratonOption
        <*> unattendedSwitch
        <*> debugSwitch

updateCommand :: Parser Command
updateCommand =
    Update
        <$> optional templateNameOption
        <*> optional previousCommitOption
        <*> optional targetArgument
        <*> optional projectConfiguratonOption
        <*> unattendedSwitch
        <*> debugSwitch

adoptCommand :: Parser Command
adoptCommand =
    Adopt
        <$> optional templateNameOption
        <*> optional targetArgument
        <*> optional projectConfiguratonOption
        <*> unattendedSwitch
        <*> debugSwitch

logCommand :: Parser Command
logCommand =
    Log
        <$> optional templateNameOption
        <*> optional targetArgument
        <*> optional projectConfiguratonOption
        <*> showLogModeOption
        <*> debugSwitch

templateGraphCommand :: Parser Command
templateGraphCommand =
    TemplateGraph
        <$> templateNameOption
        <*> branchTypeFilterOption
        <*> debugSwitch

templateListBranchesCommand :: Parser Command
templateListBranchesCommand =
    TemplateListBranches
        <$> templateNameOption
        <*> branchFilterOptions
        <*> debugSwitch

renameBranchCommand :: Parser Command
renameBranchCommand =
    TemplateRenameBranch
        <$> templateNameOption
        <*> oldBranchNameOption
        <*> newBranchNameOption
        <*> interactiveSwitch
        <*> debugSwitch

propagateChanges :: Parser Command
propagateChanges =
    TemplatePropagateChanges
        <$> templateNameOption
        <*> branchFilterNamesOption
        <*> remoteChangeModeOptionSet
        <*> unattendedSwitch
        <*> debugSwitch

listConflicts :: Parser Command
listConflicts = TemplateListConflicts <$> templateNameOption <*> debugSwitch

changeVariableCommand :: Parser Command
changeVariableCommand =
    TemplateChangeVariable
        <$> templateNameOption
        <*> oldVariableValueOption
        <*> newVariableValueOption
        <*> interactiveSwitch
        <*> debugSwitch

templateCommands :: Parser Command
templateCommands = hsubparser
    (  command
          "graph"
          (info templateGraphCommand
                (progDesc "Generate a graph of template branches")
          )
    <> command
           "list-branches"
           (info templateListBranchesCommand
                 (progDesc "List template branches")
           )
    <> command
           "change-variable"
           (info changeVariableCommand
                 (progDesc "Change a template variable value")
           )
    <> command
           "rename-branch"
           (info renameBranchCommand (progDesc "Rename a template branch"))
    <> command
           "propagate-changes"
           (info propagateChanges
                 (progDesc "Merge a template branch into child branches")
           )
    <> command
           "list-conflicts"
           (info listConflicts
                 (progDesc "List conflicting branch combinations")
           )
    )


-- brittany-disable-next-binding
runAppM
    :: Members '[Terminal, Resource, Embed IO] r
    => Bool
    -> Bool
    -> Sem
           (  GitServer
           ': Git
           ': Process
           ': Logging
           ': UI
           ': FileSystem
           ': TerminalInput
           ': Error TemplateLoaderException
           ': Error UIException
           ': Error GitException
           ': Error TenpuretoException
           ': r
           )
           a
    -> Sem r a
runAppM withDebug unattended =
    runTenpuretoException
        . runError @TenpuretoException
        . mapError TenpuretoGitException
        . mapError TenpuretoUIException
        . mapError TenpuretoTemplateLoaderException
        . runTerminalIOInput
        . runFileSystemIO
        . runUI unattended
        . runLoggingTerminal (if withDebug then Debug else Silent)
        . runProcessIO
        . withProcessLogging
        . runGit
        . runGitHub

runTenpuretoException
    :: Members '[Embed IO , Terminal] r
    => Sem r (Either TenpuretoException a)
    -> Sem r a
runTenpuretoException = (=<<) handleResult
  where
    handleResult (Right a) = return a
    handleResult (Left  e) = do
        sayLn $ pretty e
        embed $ exitWith (ExitFailure 1)

runUI
    :: Members '[FileSystem , Terminal , TerminalInput , Error UIException] r
    => Bool
    -> Sem (UI ': r) a
    -> Sem r a
runUI False = runUIInTerminal
runUI True  = runUIUnattended

runCommand
    :: Members '[Terminal , Resource , Embed IO] r => Command -> Sem r ()
runCommand Create { maybeTemplateName = t, maybeTargetDirectory = td, maybeProjectConfiguration = c, runUnattended = u, enableDebugLogging = d }
    = runAppM d u $ do
        resolvedTd <- traverse resolveDir td
        resolvedC  <- traverse resolveFile c
        maybeYaml  <- traverse loadTemplateYaml resolvedC
        let
            inputConfig = PreliminaryProjectConfiguration
                { preSelectedTemplate            = t
                , preTargetDirectory             = resolvedTd
                , prePreviousTemplateCommit      = Nothing
                , preSelectedBranches            = Set.map yamlFeatureName
                                                   .   yamlFeatures
                                                   <$> maybeYaml
                , preVariableValues              = yamlVariables <$> maybeYaml
                , preVariableDefaultReplacements =
                    fromMaybe OrderedMap.empty
                        $ liftA2 templateNameDefaultReplacement t td
                , prePreviousMergedHeads         = Nothing
                }
        createProject inputConfig
runCommand Update { maybeTemplateName = t, maybeTargetDirectory = td, maybeProjectConfiguration = c, maybePreviousCommit = pc, runUnattended = u, enableDebugLogging = d }
    = runAppM d u $ do
        resolvedTd    <- resolveDir (fromMaybe "." td)
        resolvedC     <- traverse resolveFile c
        maybeYaml     <- traverse loadTemplateYaml resolvedC
        currentConfig <- loadExistingProjectConfiguration resolvedTd
        let
            inputConfig = PreliminaryProjectConfiguration
                { preSelectedTemplate            = t
                , preTargetDirectory             = Just resolvedTd
                , prePreviousTemplateCommit      = fmap
                                                       ( ExistingParentCommit
                                                       . Committish
                                                       )
                                                       pc
                , preSelectedBranches            = Set.map yamlFeatureName
                                                   .   yamlFeatures
                                                   <$> maybeYaml
                , preVariableValues              = yamlVariables <$> maybeYaml
                , preVariableDefaultReplacements = OrderedMap.empty
                , prePreviousMergedHeads         = Nothing
                }
        updateProject (inputConfig <> currentConfig)
runCommand Adopt { maybeTemplateName = t, maybeTargetDirectory = td, maybeProjectConfiguration = c, runUnattended = u, enableDebugLogging = d }
    = runAppM d u $ do
        resolvedTd    <- resolveDir (fromMaybe "." td)
        resolvedC     <- traverse resolveFile c
        maybeYaml     <- traverse loadTemplateYaml resolvedC
        currentConfig <- loadExistingProjectConfiguration resolvedTd
        let
            inputConfig = PreliminaryProjectConfiguration
                { preSelectedTemplate            = t
                , preTargetDirectory             = Just resolvedTd
                , prePreviousTemplateCommit      = Just OrphanCommit
                , preSelectedBranches            = Set.map yamlFeatureName
                                                   .   yamlFeatures
                                                   <$> maybeYaml
                , preVariableValues              = yamlVariables <$> maybeYaml
                , preVariableDefaultReplacements = OrderedMap.empty
                , prePreviousMergedHeads         = Nothing
                }
        updateProject (inputConfig <> currentConfig)
runCommand Log { maybeTemplateName = t, maybeTargetDirectory = td, maybeProjectConfiguration = c, showLogMode = m, enableDebugLogging = d }
    = runAppM d True $ do
        resolvedTd    <- resolveDir (fromMaybe "." td)
        resolvedC     <- traverse resolveFile c
        maybeYaml     <- traverse loadTemplateYaml resolvedC
        currentConfig <- loadExistingProjectConfiguration resolvedTd
        let
            inputConfig = PreliminaryProjectConfiguration
                { preSelectedTemplate            = t
                , preTargetDirectory             = Just resolvedTd
                , prePreviousTemplateCommit      = Nothing
                , preSelectedBranches            = Set.map yamlFeatureName
                                                   .   yamlFeatures
                                                   <$> maybeYaml
                , preVariableValues              = yamlVariables <$> maybeYaml
                , preVariableDefaultReplacements = OrderedMap.empty
                , prePreviousMergedHeads         = Nothing
                }
        showLog (inputConfig <> currentConfig) m
runCommand TemplateGraph { templateName = t, branchFilter = bf, enableDebugLogging = d }
    = runAppM d True $ generateTemplateGraph t bf
runCommand TemplateListBranches { templateName = t, branchFilter = bf, enableDebugLogging = d }
    = runAppM d True $ listTemplateBranches t bf
runCommand TemplateRenameBranch { templateName = t, oldBranchName = on, newBranchName = nn, enableInteractivity = i, enableDebugLogging = d }
    = runAppM d False $ renameTemplateBranch t on nn i
runCommand TemplatePropagateChanges { templateName = t, branchFilter = bf, remoteChangeMode = cm, runUnattended = u, enableDebugLogging = d }
    = runAppM d u $ propagateTemplateBranchChanges t bf cm
runCommand TemplateListConflicts { templateName = t, enableDebugLogging = d } =
    runAppM d True $ listTemplateConflicts t
runCommand TemplateChangeVariable { templateName = t, oldVariableValue = ov, newVariableValue = nv, enableInteractivity = i, enableDebugLogging = d }
    = runAppM d False $ changeTemplateVariableValue t ov nv i

runOptParser
    :: Members '[Terminal , Resource , Embed IO , Final IO] r => Sem r ()
runOptParser = do
    w <- fromMaybe 80 <$> terminalWidth
    let p = prefs (showHelpOnEmpty <> columns w)
    c <- embed (customExecParser p opts)
    runCommand c
  where
    commands = hsubparser
        (  command
              "create"
              (info createCommand
                    (progDesc "Create a new project from a template")
              )
        <> command
               "update"
               (info updateCommand
                     (progDesc "Update a project from a template")
               )
        <> command
               "adopt"
               (info
                   adoptCommand
                   (progDesc "Adopt an existing project to use a template")
               )
        <> command
               "log"
               (info logCommand (progDesc "Show log of template changes"))
        <> command "template"
                   (info templateCommands (progDesc "Manage a template"))
        )
    opts = info (commands <**> versionOption <**> helper) fullDesc

main :: IO ()
main = do
    runTerminalIOOutput' <- runTerminalIOOutput
    runFinal
        . embedToFinal @IO
        . resourceToIOFinal
        . runTerminalIOOutput'
        $ runOptParser
