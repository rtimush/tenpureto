{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Tenpureto.Effects.Terminal
    ( module Tenpureto.Effects.Terminal
    , module Data.Text.Prettyprint.Doc
    , Text
    , AnsiStyle
    ) where

import           Polysemy
import           Polysemy.Resource
import           Polysemy.State

import           Data.Bool
import           Data.IORef
import           Data.Text                      ( Text )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           System.Console.ANSI            ( hSupportsANSI )
import           System.IO                      ( stdout )

import           Tenpureto.Effects.Terminal.Internal

data Terminal m a where
    TerminalWidth ::Terminal m (Maybe Int)
    SayLn ::Doc AnsiStyle -> Terminal m ()
    SayLnTemporary ::Doc AnsiStyle -> Terminal m ()

data TerminalInput m a where
    Ask ::Doc AnsiStyle -> Maybe Text -> TerminalInput m Text
    AskUntil ::s -> (s -> (Doc AnsiStyle, Maybe Text)) -> (s -> Text -> Either s a) -> TerminalInput m a

makeSem ''Terminal
makeSem ''TerminalInput

confirm :: Member TerminalInput r => Doc AnsiStyle -> Maybe Bool -> Sem r Bool
confirm msg def = askUntil Nothing request process
  where
    defAns = fmap (bool "n" "y") def
    request Nothing = (msg <+> "(y/n)?", defAns)
    request (Just _) =
        ("Please answer \"y\" or \"n\"." <+> msg <+> "(y/n)?", defAns)
    process = const mapAnswer
    mapAnswer "y" = Right True
    mapAnswer "n" = Right False
    mapAnswer p   = Left (Just p)

traverseWithProgressBar
    :: (Members '[Terminal , Resource] r, Traversable t)
    => (a -> Doc AnsiStyle)
    -> (a -> Sem r b)
    -> t a
    -> Sem r (t b)
traverseWithProgressBar info action tasks =
    let total = length tasks
        percentage idx = case idx * 100 `div` total of
            x | x < 10 -> space <> pretty x
            x          -> pretty x
        fullInfo idx a = brackets (percentage idx <> "%") <+> info a
        showInfo idx a = sayLnTemporary $ fullInfo idx a
        fullAction idx a = showInfo idx a >> action a
    in  traverseWithIndex fullAction tasks

runTerminalIOOutput
    :: Member (Embed IO) r => IO (Sem (Terminal ': r) a -> Sem r a)
runTerminalIOOutput = do
    ioRef        <- newIORef (TemporaryHeight 0)
    supportsANSI <- hSupportsANSI stdout
    let fmt :: Doc AnsiStyle -> Doc AnsiStyle
        fmt = if supportsANSI then id else unAnnotate
    return $ runStateIORef ioRef . reinterpret \case
        TerminalWidth -> embed getTerminalWidth
        SayLn msg     -> do
            TemporaryHeight ph <- get
            embed $ clearLastLinesTerminal ph
            put $ TemporaryHeight 0
            embed $ sayLnTerminal (fmt msg)
        SayLnTemporary msg -> do
            TemporaryHeight ph <- get
            embed $ clearLastLinesTerminal ph
            h <- embed $ sayLnTerminal' (fmt msg)
            put $ TemporaryHeight h

runTerminalIOInput
    :: Member (Embed IO) r => Sem (TerminalInput ': r) a -> Sem r a
runTerminalIOInput = interpret $ \case
    Ask msg defans -> embed $ askTerminal msg defans
    AskUntil state request process ->
        embed $ askTerminalUntil state request process
