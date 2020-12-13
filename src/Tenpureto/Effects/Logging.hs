{-# LANGUAGE TemplateHaskell #-}

module Tenpureto.Effects.Logging
    ( module Tenpureto.Effects.Logging
    , module Data.Text.Prettyprint.Doc
    , Severity(..)
    ) where

import           Polysemy

import           Data.Text.Prettyprint.Doc

import           Tenpureto.Effects.Logging.Internal
import           Tenpureto.Effects.Terminal

data Logging m a where
    LogInfo ::Doc () -> Logging m ()
    LogDebug ::Doc () -> Logging m ()

makeSem ''Logging

runLoggingTerminal
    :: Member Terminal r => Severity -> Sem (Logging ': r) a -> Sem r a
runLoggingTerminal Debug = interpret $ \case
    LogInfo  msg -> sayLn (formatTerminalMessage Info msg)
    LogDebug msg -> sayLn (formatTerminalMessage Debug msg)
runLoggingTerminal Info = interpret $ \case
    LogInfo  _   -> return ()
    LogDebug msg -> sayLn (formatTerminalMessage Debug msg)
runLoggingTerminal Silent = interpret $ \case
    LogInfo  _ -> return ()
    LogDebug _ -> return ()

runNoLogging :: Sem (Logging ': r) a -> Sem r a
runNoLogging = interpret $ \case
    LogInfo  _ -> return ()
    LogDebug _ -> return ()
