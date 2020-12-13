{-# LANGUAGE TupleSections #-}

module Tenpureto.Effects.Terminal.Internal where

import           Control.Monad
import           Data.Functor
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Traversable

import           System.Console.ANSI            ( clearFromCursorToScreenEnd
                                                , cursorUp
                                                , setCursorColumn
                                                )
import qualified System.Console.Terminal.Size  as TS
import           System.IO                      ( hFlush
                                                , stdout
                                                )

newtype TemporaryHeight = TemporaryHeight Int

layoutOptions :: Maybe Int -> LayoutOptions
layoutOptions w = LayoutOptions $ maybe Unbounded (flip AvailablePerLine 1.0) w

getTerminalWidth :: IO (Maybe Int)
getTerminalWidth = fmap TS.width <$> TS.size

sayLnTerminal :: Doc AnsiStyle -> IO ()
sayLnTerminal msg = do
    options <- layoutOptions <$> getTerminalWidth
    renderIO stdout (layoutPretty options (msg <> "\n"))
    hFlush stdout

sayTerminal' :: Doc AnsiStyle -> IO Int
sayTerminal' msg = do
    options <- layoutOptions <$> getTerminalWidth
    let text = renderStrict (layoutPretty options msg)
    let h    = length (T.lines text)
    putStr (T.unpack text)
    hFlush stdout
    return h

sayLnTerminal' :: Doc AnsiStyle -> IO Int
sayLnTerminal' msg = sayTerminal' (msg <> "\n")

clearLastLinesTerminal :: Int -> IO ()
clearLastLinesTerminal n = when (n > 0) $ do
    cursorUp n
    setCursorColumn 0
    clearFromCursorToScreenEnd

askTerminalH :: Doc AnsiStyle -> Maybe Text -> IO (Text, Int)
askTerminalH prompt (Just defans) = do
    h <-
        sayTerminal'
        $   prompt
        <+> annotate (color Black) ((brackets . pretty) defans)
        <>  " "
    input <- T.pack <$> getLine
    return (if T.null input then defans else input) <&> (, h)
askTerminalH prompt Nothing = do
    h <- sayTerminal' $ prompt <> " "
    T.pack <$> getLine <&> (, h)

askTerminal :: Doc AnsiStyle -> Maybe Text -> IO Text
askTerminal prompt defans = askTerminalH prompt defans <&> fst

askTerminalUntil
    :: s
    -> (s -> (Doc AnsiStyle, Maybe Text))
    -> (s -> Text -> Either s a)
    -> IO a
askTerminalUntil state request process = do
    (ans, h) <- uncurry askTerminalH (request state)
    reprompt h (process state ans)
  where
    reprompt h (Left s) = do
        clearLastLinesTerminal h
        askTerminalUntil s request process
    reprompt _ (Right r) = return r

traverseWithIndex
    :: (Monad f, Traversable t) => (Int -> a -> f b) -> t a -> f (t b)
traverseWithIndex f =
    let f' i a = (i + 1, f i a) in sequence . snd . mapAccumL f' 0
