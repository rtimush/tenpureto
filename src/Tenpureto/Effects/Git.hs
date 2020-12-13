{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.Git
    ( module Tenpureto.Effects.Git
    , Text
    , Committish(..)
    , ParentCommit(..)
    , GitRepository(..)
    , GitException
    , RepositoryLocation(..)
    , Path
    , Abs
    , Rel
    , Dir
    , File
    , parseRepositoryUri
    ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Resource

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           System.Random

import           Path

import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Git.Internal
import           Tenpureto.Effects.Process

newtype BranchRef = BranchRef { reference :: Text } deriving (Eq, Ord, Show)

data PushSpec = CreateBranch { sourceCommit :: Committish, destinationRef :: BranchRef }
              | UpdateBranch { sourceCommit :: Committish, sourceRef :: BranchRef, destinationRef :: BranchRef, pullRequestRef :: BranchRef, pullRequestTitle :: Text }
              | DeleteBranch { destinationRef :: BranchRef }
              | CloseBranchUpdate { pullRequestRef :: BranchRef, destinationRef :: BranchRef }
              deriving (Eq, Ord, Show)

data PullRequestSettings = PullRequestSettings
    { pullRequestAddLabels :: [Text]
    , pullRequestAssignTo  :: [Text]
    , pullRequestPreMerge  :: Bool
    }

data MergeStrategy = MergeNoFastForward
                   | MergeAllowFastForward

data MergeResult = MergeSuccessCommitted
                 | MergeSuccessUncommitted
                 | MergeConflicts [Path Rel File]
                 deriving (Eq, Show)

data Git m a where
    InitRepository ::Path Abs Dir -> Git m GitRepository
    AddRepositoryOrigin ::GitRepository -> RepositoryLocation -> Git m ()
    InitWorktree ::GitRepository -> ParentCommit -> Path Abs Dir -> Git m (GitRepository, Maybe Text)
    DeleteWorktree ::GitRepository -> Path Abs Dir -> Maybe Text -> Git m ()
    ListBranches ::GitRepository -> Git m [Text]
    CheckoutBranch ::GitRepository -> Text -> Maybe Text -> Git m ()
    MergeBranch ::GitRepository -> MergeStrategy -> Text -> Text -> Git m MergeResult
    MergeAbort ::GitRepository -> Git m ()
    RunMergeTool ::GitRepository -> Git m ()
    ResetWorktree ::GitRepository -> Git m ()
    GetRepositoryFile ::GitRepository -> Committish -> Path Rel File -> Git m (Maybe ByteString)
    GetWorkingCopyFile ::GitRepository -> Path Rel File -> Git m (Maybe ByteString)
    WriteRepoFile ::GitRepository -> Path Rel File -> ByteString -> Git m ()
    AddFiles ::GitRepository -> [Path Rel File] -> Git m ()
    Commit ::GitRepository -> Text -> Git m Committish
    FindCommitByRef ::GitRepository -> BranchRef -> Git m (Maybe Committish)
    FindCommitByMessage ::GitRepository -> Text -> Git m (Maybe Committish)
    GetCommitMessage ::GitRepository -> Committish -> Git m Text
    GitDiffHasCommits ::GitRepository -> Committish -> Committish -> Git m Bool
    GitLog ::GitRepository -> Committish -> Committish -> Git m Text
    GitLogDiff ::GitRepository -> Committish -> Committish -> Git m Text
    GitLogInteractive ::GitRepository -> [Committish] -> [Committish] -> Git m ()
    ListFiles ::GitRepository -> Git m [Path Rel File]
    HasChangedFiles ::GitRepository -> Git m Bool
    GetCurrentBranch ::GitRepository -> Git m Text
    GetCurrentHead ::GitRepository -> Git m Committish
    RenameCurrentBranch ::GitRepository -> Text -> Git m ()
    PushRefs ::GitRepository -> [PushSpec] -> Git m ()

makeSem ''Git

data GitServer m a where
    CreateOrUpdatePullRequest ::GitRepository -> PullRequestSettings -> Committish -> Text -> Text -> Text -> GitServer m ()
    ClosePullRequest ::GitRepository -> PullRequestSettings -> Text -> Text -> GitServer m ()

makeSem ''GitServer


internalBranchPrefix :: Text
internalBranchPrefix = "tenpureto/"


writeAddFile
    :: Member Git r => GitRepository -> Path Rel File -> ByteString -> Sem r ()
writeAddFile repo file content = do
    writeRepoFile repo file content
    addFiles repo [file]

withRepository
    :: Member Git r => Path Abs Dir -> (GitRepository -> Sem r a) -> Sem r a
withRepository path f = f (GitRepository path)

withClonedRepository
    :: Members '[Resource , FileSystem , Git] r
    => RepositoryLocation
    -> (GitRepository -> Sem r a)
    -> Sem r a
withClonedRepository location f = withSystemTempDir "tenpureto" $ \dir -> do
    repo <- initRepository dir
    addRepositoryOrigin repo location
    f repo

withNewWorktree
    :: Members '[Resource , FileSystem , Git] r
    => GitRepository
    -> ParentCommit
    -> (GitRepository -> Sem r a)
    -> Sem r a
withNewWorktree repo c f = do
    bracket
        (createSystemTempDir "tenpureto" >>= initWorktree repo c)
        (\(GitRepository dir, tempBranch) -> deleteWorktree repo dir tempBranch)
        (\(worktree, _) -> f worktree)

runGit
    :: Members '[FileSystem , Process , Error GitException , Embed IO] r
    => Sem (Git ': r) a
    -> Sem r a
runGit = interpret $ \case

    InitRepository dir -> do
        ensureEmptyDir dir
        gitCmd ["init", T.pack (toFilePath dir)] >>= asUnit
        return $ GitRepository dir

    AddRepositoryOrigin repo location -> do
        gitRepoCmd repo ["remote", "add", "origin", repositoryUrl location]
            >>= asUnit
        gitRepoCmd repo ["fetch", "origin"] >>= asUnit

    InitWorktree repo (ExistingParentCommit (Committish c)) dir -> do
        gitRepoCmd
                repo
                ["worktree", "add", "--no-checkout", T.pack (toFilePath dir), c]
            >>= asUnit
        return $ (GitRepository dir, Nothing)

    InitWorktree repo OrphanCommit dir -> do
        gitRepoCmd
                repo
                [ "worktree"
                , "add"
                , "--no-checkout"
                , "--detach"
                , T.pack (toFilePath dir)
                ]
            >>= asUnit
        branchSuffix <- embed randomIO
        let branch =
                "tenpureto-temp-" <> (T.pack . show) (branchSuffix :: Word)
        gitRepoCmd (GitRepository dir) ["checkout", "--orphan", branch]
            >>= asUnit
        return $ (GitRepository dir, Just branch)

    DeleteWorktree repo dir tempBranch -> do
        -- "git worktree remove" required too modern git
        removeDirRecur dir
        gitRepoCmd repo ["worktree", "prune"] >>= asUnit
        case tempBranch of
            Just branch ->
                gitRepoCmd repo ["branch", "--delete", "--force", branch]
                    >>= asUnit
            Nothing -> return ()

    ListBranches repo ->
        gitRepoCmd
                repo
                [ "for-each-ref"
                , "--format=%(refname:strip=3)"
                , "refs/remotes/origin/"
                ]
            >>= asLines

    CheckoutBranch repo branch name -> do
        gitRepoCmd repo ["checkout", branch] >>= asUnit
        gitRepoCmd repo (asName name) >>= asUnit
      where
        asName Nothing  = ["checkout", "--detach"]
        asName (Just n) = ["checkout", "-b", n]

    MergeBranch repo strategy branch message -> do
        (mergeResult, _, _) <- gitRepoCmd
            repo
            (  [ "merge"
               , "--no-commit"
               , "--allow-unrelated-histories"
               , "--message"
               , message
               ]
            <> options strategy
            <> [branch]
            )
        case mergeResult of
            ExitSuccess ->
                gitRepoCmd repo ["rev-parse", "--verify", "MERGE_HEAD"]
                    <&> \case
                            (ExitFailure _, _, _) -> MergeSuccessCommitted
                            (ExitSuccess  , _, _) -> MergeSuccessUncommitted
            ExitFailure _ ->
                gitRepoCmd repo ["diff", "--name-only", "--diff-filter=U"]
                    >>= asFiles
                    <&> MergeConflicts
      where
        options MergeAllowFastForward = []
        options MergeNoFastForward    = ["--no-ff"]

    MergeAbort    repo -> gitRepoCmd repo ["merge", "--abort"] >>= asUnit

    RunMergeTool  repo -> gitInteractiveRepoCmd repo ["mergetool"]

    ResetWorktree repo -> gitRepoCmd repo ["reset", "--hard"] >>= asUnit

    GetRepositoryFile repo (Committish c) file ->
        gitRepoCmd repo ["show", c <> ":" <> T.pack (toFilePath file)]
            <&> maybeByteString

    GetWorkingCopyFile repo file ->
        readFileAsMaybeByteString (repositoryPath repo </> file)
            <&> fmap BS.fromStrict

    WriteRepoFile repo file content -> writeFileAsByteString
        (repositoryPath repo </> file)
        (BS.toStrict content)

    AddFiles repo files ->
        gitRepoCmd
                repo
                (["add", "--force", "--"] ++ map (T.pack . toFilePath) files)
            >>= asUnit

    Commit repo message -> do
        gitRepoCmd repo ["commit", "--allow-empty", "--message", message]
            >>= asUnit
        gitRepoCmd repo ["rev-parse", "--verify", "HEAD"] >>= asCommittish

    FindCommitByRef repo (BranchRef branch) ->
        gitRepoCmd repo ["rev-parse", "--verify", branch] >>= asMaybeCommittish

    FindCommitByMessage repo pat ->
        gitRepoCmd
                repo
                [ "rev-list"
                , "--max-count=1"
                , "--date-order"
                , "--grep"
                , pat
                , "HEAD"
                ]
            >>= asMaybeCommittish

    GetCommitMessage repo (Committish c) ->
        gitRepoCmd repo ["rev-list", "--max-count=1", "--format=%B", c]
            >>= asText

    GitDiffHasCommits repo (Committish c) (Committish base) ->
        gitRepoCmd repo ["rev-list", "--count", c, "^" <> base]
            >>= asText
            <&> T.strip
            <&> ("0" /=)

    GitLog repo (Committish c) (Committish base) ->
        gitRepoCmd repo ["log", "--color", "--oneline", c, "^" <> base]
            >>= asText

    GitLogDiff repo (Committish c) (Committish base) ->
        gitRepoCmd repo ["log", "--patch", "--color", c, "^" <> base] >>= asText

    GitLogInteractive repo include exclude -> gitInteractiveRepoCmd
        repo
        (  ["log", "--color", "--oneline", "--no-merges", "--no-decorate"]
        ++ fmap unCommittish              include
        ++ fmap ((<>) "^" . unCommittish) exclude
        )

    ListFiles repo -> gitRepoCmd repo ["ls-files"] >>= asFiles

    HasChangedFiles repo ->
        gitRepoCmd repo ["status", "--porcelain"] >>= asText <&> (not . T.null)

    GetCurrentBranch repo ->
        gitRepoCmd repo ["rev-parse", "--abbrev-ref", "HEAD"]
            >>= asText
            <&> T.strip

    GetCurrentHead repo ->
        gitRepoCmd repo ["rev-parse", "--verify", "HEAD"] >>= asCommittish

    RenameCurrentBranch repo name ->
        gitRepoCmd repo ["branch", "--move", name] >>= asUnit

    PushRefs repo refs ->
        gitRepoCmd repo
                   (["push", "--atomic", "origin"] ++ mapMaybe refspec refs)
            >>= asUnit
      where
        ref (BranchRef dst) = "refs/heads/" <> dst
        refspec CreateBranch { sourceCommit = (Committish c), destinationRef = dst }
            = Just $ c <> ":" <> ref dst
        refspec UpdateBranch { sourceCommit = (Committish c), destinationRef = dst }
            = Just $ c <> ":" <> ref dst
        refspec DeleteBranch { destinationRef = dst } = Just $ ":" <> ref dst
        refspec CloseBranchUpdate{}                   = Nothing


runGitHub
    :: Members '[Git , Process , Error GitException] r
    => Sem (GitServer ': r) a
    -> Sem r a
runGitHub = interpret $ \case

    CreateOrUpdatePullRequest repo settings commitish title localSource target
        -> let source = internalBranchPrefix <> localSource
           in
               do
                   gitRepoCmd
                           repo
                           [ "push"
                           , "--force"
                           , "origin"
                           , unCommittish commitish
                           <> ":"
                           <> "refs/heads/"
                           <> source
                           ]
                       >>= asUnit
                   exitingPullRequests <- hubApiFindPullRequest repo
                                                                source
                                                                target
                   pullRequest <- case exitingPullRequests of
                       Just pullRequest -> return pullRequest
                       Nothing ->
                           hubApiCmd
                                   repo
                                   ApiPost
                                   "/repos/{owner}/{repo}/pulls"
                                   PullRequestInputPayload
                                       { pullRequestHead     = source
                                       , pullRequestBase     = target
                                       , setPullRequestTitle = Just title
                                       , setPullRequestState = Nothing
                                       }
                               >>= asApiResponse
                   hubApiCmd
                           repo
                           ApiPatch
                           (  "/repos/{owner}/{repo}/issues/"
                           <> (T.pack . show . pullRequestNumber)
                                  pullRequest
                           )
                           IssueInputPayload
                               { setIssueAssignees = nub
                                                     $  fmap
                                                            assigneeLogin
                                                            (pullRequestAssignees
                                                                pullRequest
                                                            )
                                                     ++ pullRequestAssignTo
                                                            settings
                               , setIssueLabels    = nub
                                                     $  fmap
                                                            labelName
                                                            (pullRequestLabels
                                                                pullRequest
                                                            )
                                                     ++ pullRequestAddLabels
                                                            settings
                               }
                       >>= asUnit

    ClosePullRequest repo _ localSource _ ->
        let source = internalBranchPrefix <> localSource
        in  gitRepoCmd repo ["push", "origin", ":" <> "refs/heads/" <> source]
                $> ()


instance Pretty BranchRef where
    pretty ref = pretty $ reference ref
