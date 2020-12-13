module Tenpureto.Yaml
    ( fromByteString
    , toByteString
    , prettyPrintYamlParseException
    ) where

import           Control.Exception              ( try )
import           Data.ByteString                ( ByteString )
import           Data.Conduit                   ( (.|)
                                                , runConduitRes
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Yaml.Builder
import           Data.Yaml.Parser
import           System.IO.Unsafe               ( unsafePerformIO )
import qualified Text.Libyaml                  as LY

fromByteString :: FromYaml a => ByteString -> Either YamlParseException a
fromByteString bs = unsafePerformIO $ try (readByteString bs)
  where
    readByteString :: FromYaml a => ByteString -> IO a
    readByteString bs' =
        runConduitRes (LY.decode bs' .| sinkRawDoc) >>= parseRawDoc

prettyPrintYamlParseException :: YamlParseException -> Text
prettyPrintYamlParseException UnexpectedEndOfEvents =
    "Unexpected end of YAML stream"
prettyPrintYamlParseException (UnexpectedEvent e) =
    "Unexpected YAML event " <> (T.pack . show) e
prettyPrintYamlParseException (FromYamlException e) =
    "Cannot parse YAML: " <> e
