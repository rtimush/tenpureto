{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.Process
    ( module Tenpureto.Effects.Process
    , NonEmpty(..)
    , ExitCode(..)
    ) where

import           Polysemy

import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.ByteString.Lazy          as BS
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E

import           Path
import           System.Exit                    ( ExitCode(..) )
import           System.Process.Typed           ( byteStringInput
                                                , closed
                                                , inherit
                                                , proc
                                                , readProcess
                                                , runProcess
                                                , runProcess_
                                                , setStderr
                                                , setStdin
                                                , setStdout
                                                , setWorkingDir
                                                , shell
                                                )

import           Tenpureto.Effects.Logging
import           Tenpureto.Orphanage            ( )


data Process m a where
    RunCmd::NonEmpty Text -> Process m (ExitCode, ByteString, ByteString)
    RunInputCmd::NonEmpty Text -> ByteString -> Process m (ExitCode, ByteString, ByteString)
    RunInteractiveCmd::NonEmpty Text -> Process m ExitCode
    RunShell ::Path Abs Dir -> Process m ()

makeSem ''Process

runProcessIO :: Member (Embed IO) r => Sem (Process ': r) a -> Sem r a
runProcessIO = interpret $ \case

    RunCmd (cmd :| args) -> embed $ readProcess $ setStdin closed $ proc
        (T.unpack cmd)
        (map T.unpack args)

    RunInputCmd (cmd :| args) input ->
        embed $ readProcess $ setStdin (byteStringInput input) $ proc
            (T.unpack cmd)
            (map T.unpack args)

    RunInteractiveCmd (cmd :| args) ->
        embed
            $ runProcess
            $ setStdin inherit
            $ setStdout inherit
            $ setStderr inherit
            $ proc (T.unpack cmd) (map T.unpack args)

    RunShell dir ->
        embed $ runProcess_ $ setWorkingDir (toFilePath dir) $ shell "$SHELL"


withProcessLogging
    :: (Member Logging r, Member Process r) => Sem r a -> Sem r a
withProcessLogging = intercept $ \case

    RunCmd (cmd :| args) -> do
        logInfo $ "Running" <+> pretty cmd <+> fillSep (map pretty args)
        (code, out, err) <- runCmd (cmd :| args)
        logStream "err" err
        logStream "out" out
        return (code, out, err)

    RunInputCmd (cmd :| args) input -> do
        logInfo $ "Running" <+> pretty cmd <+> fillSep (map pretty args)
        logStream "in" input
        (code, out, err) <- runInputCmd (cmd :| args) input
        logStream "err" err
        logStream "out" out
        return (code, out, err)

    RunInteractiveCmd (cmd :| args) -> do
        logInfo $ "Running interactively" <+> pretty cmd <+> fillSep
            (map pretty args)
        runInteractiveCmd (cmd :| args)

    RunShell dir -> do
        logInfo $ "Running shell in " <+> pretty dir
        runShell dir

  where

    logStream :: (Member Logging r) => Doc () -> ByteString -> Sem r ()
    logStream name d = logDebug $ prefixed (name <> " > ") (decodeUtf8 d)

    decodeUtf8 :: ByteString -> Text
    decodeUtf8 = E.decodeUtf8 . BS.toStrict

    prefixed :: Doc () -> Text -> Doc ()
    prefixed prefix =
        concatWith (\a b -> a <> hardline <> b)
            . map ((<>) prefix . pretty)
            . T.lines
