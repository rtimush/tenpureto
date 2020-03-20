{-# LANGUAGE TupleSections #-}

module Tenpureto
    ( module Tenpureto
    , TenpuretoException(..)
    )
where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Resource

import           Data.Maybe
import           Data.Either
import           Data.Either.Combinators        ( rightToMaybe )
import qualified Data.ByteString.Lazy          as BS
import qualified Data.Text                     as T
import qualified Data.Set                      as Set
import           Data.Foldable
import           Data.Functor
import           Control.Applicative
import           Data.Attoparsec.Text    hiding ( try )
import qualified Algebra.Graph.Export.Dot      as Dot

import           Tenpureto.Data
import           Tenpureto.Messages
import           Tenpureto.Graph
import           Tenpureto.Effects.Logging
import           Tenpureto.Effects.Terminal
import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Process
import           Tenpureto.Effects.Git
import           Tenpureto.Effects.UI
import           Tenpureto.FeatureMerger
import           Tenpureto.TemplateLoader
import           Tenpureto.Templater
import           Tenpureto.Internal
import           Tenpureto.OrderedSet           ( OrderedSet )
import qualified Tenpureto.OrderedSet          as OrderedSet
import           Tenpureto.OrderedMap           ( OrderedMap )
import qualified Tenpureto.OrderedMap          as OrderedMap

data RemoteChangeMode = PushDirectly
                      | UpstreamPullRequest { pullRequestSettings :: PullRequestSettings }

data ChangesNature = ExistingChanges | NewChanges

data ShowLogMode = ShowLogAll | ShowLogIncoming | ShowLogIncluded

buildTemplaterSettings
    :: TemplateYaml -> FinalProjectConfiguration -> TemplaterSettings
buildTemplaterSettings yaml cfg = TemplaterSettings
    { templaterFromVariables = yamlVariables yaml
    , templaterToVariables   = variableValues cfg
    , templaterExcludes      = yamlExcludes yaml
    }

createProject
    :: Members
           '[Git, UI, FileSystem, Terminal, Logging, Resource, Error
               TenpuretoException]
           r
    => PreliminaryProjectConfiguration
    -> Sem r ()
createProject projectConfiguration =
    withPreparedTemplate projectConfiguration
        $ \template finalTemplateConfiguration finalProjectConfiguration templaterSettings mergedTemplateYaml mergedHeads ->
              let dst = targetDirectory finalTemplateConfiguration
                  translatedTemplateYaml = translateTemplateYaml
                      finalProjectConfiguration
                      mergedTemplateYaml
              in  do
                      project          <- initRepository dst
                      compiledSettings <- compileSettings templaterSettings
                      files            <- copyTemplate compiledSettings
                                                       translatedTemplateYaml
                                                       template
                                                       (repositoryPath project)
                      addFiles project files
                      _ <- commit
                          project
                          (commitCreateMessage
                              (selectedTemplate finalTemplateConfiguration)
                              (fmap unCommittish (OrderedSet.toList mergedHeads)
                              )
                          )
                      sayLn $ projectCreated dst

updateProject
    :: Members
           '[Git, UI, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => PreliminaryProjectConfiguration
    -> Sem r ()
updateProject projectConfiguration = do
    finalUpdateConfiguration <- inputUpdateConfiguration projectConfiguration
    logDebug
        $  "Final update configuration"
        <> line
        <> (indent 4 . pretty) finalUpdateConfiguration
    withPreparedTemplate projectConfiguration
        $ \template finalTemplateConfiguration finalProjectConfiguration templaterSettings mergedTemplateYaml mergedHeads ->
              let translatedTemplateYaml = translateTemplateYaml
                      finalProjectConfiguration
                      mergedTemplateYaml
              in
                  withRepository (targetDirectory finalTemplateConfiguration)
                      $ \project ->
                            withNewWorktree
                                    project
                                    (previousTemplateCommit
                                        finalUpdateConfiguration
                                    )
                                $ \staging -> do
                                      compiledSettings <- compileSettings
                                          templaterSettings
                                      files <- copyTemplate
                                          compiledSettings
                                          translatedTemplateYaml
                                          template
                                          (repositoryPath staging)
                                      addFiles staging files
                                      hasChanges <- hasChangedFiles staging
                                      if hasChanges
                                          then do
                                              updatedTemplateCommit <- commit
                                                  staging
                                                  (commitUpdateMessage
                                                      (selectedTemplate
                                                          finalTemplateConfiguration
                                                      )
                                                      (fmap
                                                          unCommittish
                                                          (OrderedSet.toList
                                                              mergedHeads
                                                          )
                                                      )
                                                  )
                                              logInfo
                                                  $   "Updated template commit:"
                                                  <+> pretty
                                                          updatedTemplateCommit
                                              let
                                                  commitMessage =
                                                      commitUpdateMergeMessage
                                                          (selectedTemplate
                                                              finalTemplateConfiguration
                                                          )
                                              mergeResult <- mergeBranch
                                                  project
                                                  MergeAllowFastForward
                                                  (unCommittish
                                                      updatedTemplateCommit
                                                  )
                                                  commitMessage
                                              case mergeResult of
                                                  MergeSuccessCommitted ->
                                                      sayLn
                                                          $ projectUpdated
                                                                (repositoryPath
                                                                    project
                                                                )
                                                  MergeSuccessUncommitted ->
                                                      sayLn
                                                          $ projectUpdatedWithoutConflicts
                                                                (repositoryPath
                                                                    project
                                                                )
                                                  MergeConflicts _ ->
                                                      sayLn
                                                          $ projectUpdatedWithConflicts
                                                                (repositoryPath
                                                                    project
                                                                )
                                          else sayLn noRelevantTemplateChanges

showLog
    :: Members
           '[Git, UI, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => PreliminaryProjectConfiguration
    -> ShowLogMode
    -> Sem r ()
showLog projectConfiguration logMode = do
    finalUpdateConfiguration <- inputUpdateConfiguration projectConfiguration
    logDebug
        $  "Final update configuration"
        <> line
        <> (indent 4 . pretty) finalUpdateConfiguration
    withPreparedTemplate projectConfiguration
        $ \template _ _ _ _ mergedHeads ->
              let previousHeads =
                          case fold (prePreviousMergedHeads projectConfiguration) of
                              [] -> throw TenpuretoNoPreviousMergedHeads
                              hs -> return hs
                  currentHeads = OrderedSet.toList mergedHeads
              in  do
                      (include, exclude) <- case logMode of
                          ShowLogAll      -> return (currentHeads, [])
                          ShowLogIncluded -> (, []) <$> previousHeads
                          ShowLogIncoming -> (currentHeads, ) <$> previousHeads
                      gitLogInteractive template include exclude

translateTemplateYaml
    :: FinalProjectConfiguration -> TemplateYaml -> TemplateYaml
translateTemplateYaml cfg yaml = TemplateYaml
    { yamlVariables = variableValues cfg
    , yamlFeatures  = yamlFeatures yaml
    , yamlExcludes  = mempty
    , yamlConflicts = mempty
    }

copyTemplate
    :: (Member FileSystem r, Member Logging r, Member Git r)
    => CompiledTemplaterSettings
    -> TemplateYaml
    -> GitRepository
    -> Path Abs Dir
    -> Sem r [Path Rel File]
copyTemplate settings yaml repo dst = do
    files <- copy settings repo dst
    writeFileAsByteString (dst </> templateYamlFile)
                          (BS.toStrict . formatTemplateYaml $ yaml)
    return $ templateYamlFile : files

withPreparedTemplate
    :: Members
           '[Git, UI, Terminal, Logging, FileSystem, Resource, Error
               TenpuretoException]
           r
    => PreliminaryProjectConfiguration
    -> (  GitRepository
       -> FinalTemplateConfiguration
       -> FinalProjectConfiguration
       -> TemplaterSettings
       -> TemplateYaml
       -> OrderedSet Committish
       -> Sem r ()
       )
    -> Sem r ()
withPreparedTemplate projectConfiguration block = do
    logDebug
        $  "Preliminary project configuration:"
        <> line
        <> (indent 4 . pretty) projectConfiguration
    finalTemplateConfiguration <- inputTemplateConfiguration
        projectConfiguration
    withClonedRepository
            (parseRepositoryUri $ selectedTemplate finalTemplateConfiguration)
        $ \repository -> do
              templateInformation <- loadTemplateInformation repository
              logDebug
                  $  "Template information"
                  <> line
                  <> (indent 4 . pretty) templateInformation
              finalProjectConfiguration <- inputProjectConfiguration
                  templateInformation
                  projectConfiguration
              logDebug
                  $  "Final project configuration"
                  <> line
                  <> (indent 4 . pretty) finalProjectConfiguration
              (mergedTemplateYaml, mergedHeads) <- prepareTemplate
                  repository
                  templateInformation
                  finalProjectConfiguration
              logDebug
                  $  "Merged template YAML"
                  <> line
                  <> (indent 4 . pretty) mergedTemplateYaml
              let templaterSettings = buildTemplaterSettings
                      mergedTemplateYaml
                      finalProjectConfiguration
              block repository
                    finalTemplateConfiguration
                    finalProjectConfiguration
                    templaterSettings
                    mergedTemplateYaml
                    mergedHeads

loadExistingProjectConfiguration
    :: Members '[Git, Logging] r
    => Path Abs Dir
    -> Sem r PreliminaryProjectConfiguration
loadExistingProjectConfiguration projectPath =
    withRepository projectPath $ \project -> do
        logInfo
            $   "Loading template configuration from"
            <+> pretty templateYamlFile
        templateYamlContent <- getWorkingCopyFile project templateYamlFile
        previousCommit <- findCommitByMessage project commitMessagePattern
        previousCommitMessage <- traverse (getCommitMessage project)
                                          previousCommit
        let yaml = templateYamlContent >>= (rightToMaybe . parseTemplateYaml)
        return PreliminaryProjectConfiguration
            { preSelectedTemplate            = extractTemplateName
                                                   =<< previousCommitMessage
            , preTargetDirectory             = Just projectPath
            , prePreviousTemplateCommit      = ExistingParentCommit
                                                   <$> previousCommit
            , preSelectedBranches            = fmap
                                                   (Set.map yamlFeatureName . yamlFeatures)
                                                   yaml
            , preVariableValues              = fmap yamlVariables yaml
            , preVariableDefaultReplacements = OrderedMap.empty
            , prePreviousMergedHeads         = extractTemplateCommits
                                                   <$> previousCommitMessage
            }

prepareTemplate
    :: Members '[Git, UI, Terminal, Logging, Error TenpuretoException] r
    => GitRepository
    -> TemplateInformation
    -> FinalProjectConfiguration
    -> Sem r (TemplateYaml, OrderedSet Committish)
prepareTemplate repository template configuration =
    let
        graph            = branchesGraph template
        selectedBranches = projectBranches configuration
        fullSelectedBranches =
            Set.fromList $ graphAncestors graph selectedBranches
        (mergeLog, _) = runMergeGraphPure graph fullSelectedBranches
        mergeRecord (MergeRecord a b c) =
            "Merge" <+> pretty b <+> "into" <+> pretty a <+> "as" <+> pretty c
        mergeRecord (CheckoutRecord a) = "Checkout" <+> pretty a
    in
        withMergeCache $ do
            logInfo $ "Full branch selection:" <> line <> (indent 4 . pretty)
                (branchName <$> Set.toList fullSelectedBranches)
            logInfo $ "Merge plan:" <> line <> (indent 4 . vsep)
                (fmap mergeRecord mergeLog)
            runMergeGraph repository graph fullSelectedBranches
                >>= maybe (throw TenpuretoEmptySelection) return

generateTemplateGraph
    :: Members
           '[Git, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> BranchFilter
    -> Sem r ()
generateTemplateGraph template branchFilter =
    withClonedRepository (parseRepositoryUri template) $ \repo -> do
        templateInformation <- loadTemplateInformation repo
        let graph = filterVertices
                (applyBranchFilter branchFilter templateInformation)
                (templateBranchesGraph templateInformation)
        let style = exportDotStyle templateInformation
        sayLn $ pretty (Dot.export style graph)

listTemplateBranches
    :: Members
           '[Git, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> BranchFilter
    -> Sem r ()
listTemplateBranches template branchFilter =
    withClonedRepository (parseRepositoryUri template) $ \repo -> do
        templateInformation <- loadTemplateInformation repo
        let branches = getTemplateBranches branchFilter templateInformation
        traverse_ (sayLn . pretty . branchName) branches

renameTemplateBranch
    :: Members
           '[Git, GitServer, UI, Terminal, FileSystem, Process, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Text
    -> Text
    -> Bool
    -> Sem r ()
renameTemplateBranch template oldBranch newBranch interactive =
    runTemplateChange template interactive PushDirectly NewChanges $ \repo -> do
        templateInformation <- loadTemplateInformation repo
        mainBranch          <- getTemplateBranch templateInformation oldBranch
        let bis = branchesInformation templateInformation
        let
            renameOnBranch bi = do
                checkoutBranch repo (branchName bi) Nothing
                let descriptor = templateYaml bi
                    newDescriptor =
                        renameBranchInYaml oldBranch newBranch descriptor
                writeAddFile repo
                             templateYamlFile
                             (formatTemplateYaml newDescriptor)
                commit repo (commitRenameBranchMessage oldBranch newBranch)
        let childBranches = filter (isChildBranch oldBranch) bis
        let deleteOld = DeleteBranch { destinationRef = BranchRef oldBranch }
        pushNew <-
            renameOnBranch mainBranch
                <&> \c -> CreateBranch { sourceCommit   = c
                                       , destinationRef = BranchRef newBranch
                                       }
        pushChildren <- traverse
            (\bi -> renameOnBranch bi <&> \c -> UpdateBranch
                { sourceCommit     = c
                , sourceRef        = BranchRef $ branchName bi
                , destinationRef   = BranchRef $ branchName bi
                , pullRequestRef   = BranchRef $ branchName bi
                , pullRequestTitle = pullRequestRenameBranchTitle
                                         (branchName bi)
                                         oldBranch
                                         newBranch
                }
            )
            childBranches
        return $ deleteOld : pushNew : pushChildren

propagateTemplateBranchChanges
    :: Members
           '[Git, GitServer, UI, Terminal, FileSystem, Process, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> BranchFilter
    -> RemoteChangeMode
    -> Sem r ()
propagateTemplateBranchChanges template branchFilter pushMode =
    runTemplateChange template False pushMode ExistingChanges $ \repo ->
        withMergeCache $ do
            templateInformation <- loadTemplateInformation repo
            let branches = getTemplateBranches branchFilter templateInformation
            let mode = case pushMode of
                    PushDirectly          -> PropagatePushMerged
                    UpstreamPullRequest{} -> PropagatePushSeparately
            logInfo $ "Propagating changes for" <+> pretty
                (fmap branchName branches)
            runPropagateGraph repo
                              mode
                              (branchesGraph templateInformation)
                              (Set.fromList branches)

listTemplateConflicts
    :: Members
           '[Git, UI, Terminal, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Sem r ()
listTemplateConflicts template =
    withClonedRepository (parseRepositoryUri template) $ \repo ->
        withMergeCache $ do
            templateInformation <- loadTemplateInformation repo
            let graph        = branchesGraph templateInformation
            let combinations = listMergeCombinations graph
            let
                keepConflicts combination =
                    either (const $ Just (Set.map branchName combination))
                           (const Nothing)
                        <$> try
                                (  resetWorktree repo
                                >> runMergeGraph repo graph combination
                                )
            let info = pretty . Set.map branchName
            conflicting <-
                catMaybes
                    <$> traverseWithProgressBar info keepConflicts combinations
            case conflicting of
                [] -> sayLn noConflictingCombinations
                names ->
                    sayLn $ conflictingCombinations <> line <> (indent 4 . vsep)
                        (fmap pretty names)
            return ()

changeTemplateVariableValue
    :: Members
           '[Git, GitServer, UI, Terminal, FileSystem, Process, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Text
    -> Text
    -> Bool
    -> Sem r ()
changeTemplateVariableValue template oldValue newValue interactive =
    runTemplateChange template interactive PushDirectly NewChanges $ \repo -> do
        bis <- branchesInformation <$> loadTemplateInformation repo
        templaterSettings <- compileSettings $ TemplaterSettings
            { templaterFromVariables = OrderedMap.singleton "Variable" oldValue
            , templaterToVariables   = OrderedMap.singleton "Variable" newValue
            , templaterExcludes      = Set.empty
            }
        let branches = filter (hasVariableValue oldValue) bis
            changeOnBranch bi = do
                checkoutBranch repo (branchName bi) Nothing
                files <- move templaterSettings repo
                addFiles repo files
                let descriptor = templateYaml bi
                    newDescriptor =
                        replaceVariableInYaml oldValue newValue descriptor
                writeAddFile repo
                             templateYamlFile
                             (formatTemplateYaml newDescriptor)
                c <- commit repo (commitChangeVariableMessage oldValue newValue)
                return UpdateBranch
                    { sourceCommit     = c
                    , sourceRef        = BranchRef $ branchName bi
                    , destinationRef   = BranchRef $ branchName bi
                    , pullRequestRef   = BranchRef $ branchName bi
                    , pullRequestTitle = pullRequestChangeVariableTitle
                                             (branchName bi)
                                             oldValue
                                             newValue
                    }
        traverse changeOnBranch branches

runTemplateChange
    :: Members
           '[Git, GitServer, UI, Terminal, Process, FileSystem, Logging, Resource, Error
               TenpuretoException]
           r
    => Text
    -> Bool
    -> RemoteChangeMode
    -> ChangesNature
    -> (GitRepository -> Sem r [PushSpec])
    -> Sem r ()
runTemplateChange template interactive changeMode changesNature f =
    withClonedRepository (parseRepositoryUri template) $ \repo -> do
        let diff = case changesNature of
                ExistingChanges -> gitLog
                NewChanges      -> gitLogDiff
        let confirmUpdate src (BranchRef ref) = do
                msg <- changesForBranchMessage ref
                    <$> diff repo src (Committish $ "remotes/origin/" <> ref)
                sayLn msg
                if not interactive
                    then return src
                    else do
                        shouldRunShell <- confirmShellToAmend
                        if shouldRunShell
                            then do
                                runShell (repositoryPath repo)
                                getCurrentHead repo
                            else return src
            confirmCommit CreateBranch { sourceCommit = src, destinationRef = dst }
                = confirmUpdate src dst <&> \c ->
                    CreateBranch { sourceCommit = c, destinationRef = dst }

            confirmCommit UpdateBranch { sourceCommit = srcCommit, sourceRef = src, destinationRef = dst, pullRequestRef = prRef, pullRequestTitle = title }
                = confirmUpdate srcCommit dst <&> \c -> UpdateBranch
                    { sourceCommit     = c
                    , sourceRef        = src
                    , destinationRef   = dst
                    , pullRequestRef   = prRef
                    , pullRequestTitle = title
                    }
            confirmCommit refspec@DeleteBranch{}      = return refspec
            confirmCommit refspec@CloseBranchUpdate{} = return refspec

        let pullRequest _ (DeleteBranch (BranchRef dst)) = do
                sayLn $ deleteBranchManually dst
                return $ Left $ TenpuretoBranchNotDeleted dst
            pullRequest _ (CreateBranch _ (BranchRef dst)) = do
                sayLn $ createBranchManually dst
                return $ Left $ TenpuretoBranchNotCreated dst
            pullRequest settings UpdateBranch { sourceCommit = c, destinationRef = BranchRef dst, pullRequestRef = BranchRef pr, pullRequestTitle = title }
                = try $ createOrUpdatePullRequest repo settings c title pr dst
            pullRequest settings CloseBranchUpdate { destinationRef = BranchRef dst, pullRequestRef = BranchRef pr }
                = try $ closePullRequest repo settings pr dst
        let pushRefsToServer PushDirectly changes = pushRefs repo changes
            pushRefsToServer UpstreamPullRequest { pullRequestSettings = settings } changes
                = do
                    errors <- lefts <$> traverse (pullRequest settings) changes
                    case errors of
                        [] -> return ()
                        e  -> throw $ MultipleExceptions e
        changes <- f repo >>= traverse confirmCommit
        let cuIncrement = case changeMode of
                PushDirectly          -> 0
                UpstreamPullRequest{} -> 1
        let collectPushRefs DeleteBranch { destinationRef = r } (d, c, u, cu) =
                (r : d, c, u, cu)
            collectPushRefs CreateBranch { destinationRef = r } (d, c, u, cu) =
                (d, r : c, u, cu)
            collectPushRefs UpdateBranch { destinationRef = r } (d, c, u, cu) =
                (d, c, r : u, cu)
            collectPushRefs CloseBranchUpdate{} (d, c, u, cu) =
                (d, c, u, cu + cuIncrement)
        let (deletes, creates, updates, closes) =
                foldr collectPushRefs ([], [], [], 0) changes
        if null deletes && null creates && null updates && closes == 0
            then sayLn noRelevantTemplateChanges
            else do
                shouldPush <- case changeMode of
                    PushDirectly          -> confirmPush deletes creates updates
                    UpstreamPullRequest{} -> confirmPullRequest updates closes
                if shouldPush
                    then pushRefsToServer changeMode changes
                    else throw CancelledException

commitMessagePattern :: Text
commitMessagePattern = "^Template: .*$"

extractTemplateNameRegex :: Parser Text
extractTemplateNameRegex = matching <|> nonMatching *> extractTemplateNameRegex
  where
    matching    = string "Template: " *> takeWhile1 (not . isEndOfLine)
    nonMatching = skipWhile (not . isEndOfLine) *> endOfLine

extractTemplateName :: Text -> Maybe Text
extractTemplateName msg = rightToMaybe $ parseOnly extractTemplateNameRegex msg

extractTemplateCommitsRegex :: Parser [Text]
extractTemplateCommitsRegex = catMaybes
    <$> many' (Just <$> matching <|> Nothing <$ nonMatching)
  where
    matching    = string "Template commit: " *> takeWhile1 (not . isEndOfLine)
    nonMatching = skipWhile (not . isEndOfLine) *> endOfLine


extractTemplateCommits :: Text -> [Committish]
extractTemplateCommits msg = either (const []) (fmap Committish)
    $ parseOnly extractTemplateCommitsRegex msg

isChildBranch :: Text -> TemplateBranchInformation -> Bool
isChildBranch branch bi =
    branch /= branchName bi && Set.member branch (requiredBranches bi)

hasVariableValue :: Text -> TemplateBranchInformation -> Bool
hasVariableValue value bi = value `elem` OrderedMap.elems (branchVariables bi)

getTemplateBranch
    :: Member (Error TenpuretoException) r
    => TemplateInformation
    -> Text
    -> Sem r TemplateBranchInformation
getTemplateBranch templateInformation branch = maybe
    (throw $ TemplateBranchNotFoundException branch)
    return
    (findTemplateBranch templateInformation branch)

templateNameDefaultReplacement :: Text -> FilePath -> OrderedMap Text Text
templateNameDefaultReplacement template target = OrderedMap.singleton
    (keepRepoName $ dropGitSuffix template)
    (keepRepoName $ T.pack target)
  where
    dropGitSuffix url = fromMaybe url (T.stripSuffix ".git" url)
    keepRepoName url = last $ T.split (== '/') url
