module Tenpureto.Templater where

import           Polysemy
import           Polysemy.Resource

import           Control.Monad
import           Data.Attoparsec.Text
import           Data.ByteString                ( ByteString )
import           Data.Foldable
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Replace.Attoparsec.Text
import           System.FilePattern

import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Git
import           Tenpureto.Effects.Logging
import           Tenpureto.OrderedMap           ( OrderedMap )
import qualified Tenpureto.OrderedMap          as OrderedMap
import           Tenpureto.Orphanage            ( )
import           Tenpureto.Templater.CaseConversion


data TemplaterSettings = TemplaterSettings
    { templaterFromVariables :: OrderedMap Text Text
    , templaterToVariables   :: OrderedMap Text Text
    , templaterExcludes      :: Set Text
    }

data CompiledTemplaterSettings = CompiledTemplaterSettings
    { translate     :: Text -> Text
    , shouldExclude :: Path Rel File -> Bool
    }

data TemplaterException = TemplaterException
    deriving Show

expandReplacement :: (Text, Text) -> [(Text, Text)]
expandReplacement (a, b) = removeDuplicated
    [ (templateValueText av, templateValueText bv)
    | av <- variations a
    , bv <- variations b
    , sameStyle av bv
    ]
  where
    variations text = Set.toList . Set.fromList $ concatMap
        styleVariations
        (textToTemplateValues text)
    removeDuplicated = nubBy (\(t1, _) (t2, _) -> t1 == t2)

expandReplacements
    :: OrderedMap Text Text -> OrderedMap Text Text -> [(Text, Text)]
expandReplacements fv tv = concatMap expandReplacement
    $ OrderedMap.elems (OrderedMap.intersectionWith (,) fv tv)

compileSettings
    :: Member Logging r => TemplaterSettings -> Sem r CompiledTemplaterSettings
compileSettings TemplaterSettings { templaterFromVariables = tfv, templaterToVariables = ttv, templaterExcludes = ex }
    = let
          replacements = expandReplacements tfv ttv
          excludes     = compileExcludes ex
      in
          do
              logDebug
                  $   "From variables:"
                  <+> align
                          (sep
                              [ pretty f <+> "->" <+> pretty t
                              | (f, t) <- OrderedMap.toList tfv
                              ]
                          )
              logDebug
                  $   "To variables:"
                  <+> align
                          (sep
                              [ pretty f <+> "->" <+> pretty t
                              | (f, t) <- OrderedMap.toList ttv
                              ]
                          )
              logDebug
                  $   "Replacements:"
                  <+> align
                          (sep
                              [ pretty f <+> "->" <+> pretty t
                              | (f, t) <- replacements
                              ]
                          )
              logDebug $ "Excludes:" <+> align
                  (sep (fmap pretty (Set.toList ex)))
              return CompiledTemplaterSettings
                  { translate     = replaceVariables replacements
                  , shouldExclude = excludes
                  }

replaceVariables :: [(Text, Text)] -> Text -> Text
replaceVariables []    = id
replaceVariables rules = runReplace
  where
    replaceOne (from, to) = string from $> to
    replaceAll = choice $ fmap replaceOne rules
    runReplace = streamEdit replaceAll id

data FileContent = BinaryContent ByteString
                 | TextContent Text (Text -> ByteString)

mapContent :: (Text -> Text) -> FileContent -> FileContent
mapContent f (TextContent t enc) = TextContent (f t) enc
mapContent _ (BinaryContent b  ) = BinaryContent b

detectEncoding :: ByteString -> FileContent
detectEncoding bs = case decodeUtf8' bs of
    Right t -> TextContent t encodeUtf8
    Left  _ -> BinaryContent bs

contentToByteString :: FileContent -> ByteString
contentToByteString (TextContent t enc) = enc t
contentToByteString (BinaryContent bs ) = bs

translateFile
    :: Member FileSystem r
    => CompiledTemplaterSettings
    -> Path Rel File
    -> Sem r (Path Rel File)
translateFile settings =
    parseRelFile . T.unpack . translate settings . T.pack . fromRelFile

compileExcludes :: Set Text -> Path Rel File -> Bool
compileExcludes excludes =
    let handleLeadingSlash p = fromMaybe ("**/" <> p) (T.stripPrefix "/" p)
        handleTrailingSlash p =
            maybe (p <> "/**") (<> "/**/*") (T.stripSuffix "/" p)
        matches =
            (?==)
                .   T.unpack
                .   handleLeadingSlash
                .   handleTrailingSlash
                <$> Set.toList excludes
        fab ?? a = fmap ($ a) fab
    in  or . (??) matches . fromRelFile

copyAbsFile
    :: (Member FileSystem r, Member Logging r)
    => CompiledTemplaterSettings
    -> Path Abs File
    -> Path Abs File
    -> (ByteString -> Sem r ())
    -> Sem r ()
copyAbsFile settings src dst write = do
    logDebug $ "Copying" <+> pretty src <+> "to" <+> pretty dst
    ensureDir (parent dst)
    symlink <- isSymlink src
    if symlink
        then getSymbolicLinkDirTarget src >>= flip createDirectoryLink dst
        else do
            byteContent <- readFileAsByteString src
            let content           = detectEncoding byteContent
                translatedContent = mapContent (translate settings) content
                translated        = contentToByteString translatedContent
            write translated
            copyPermissions src dst

copyRelFile
    :: (Member FileSystem r, Member Logging r)
    => CompiledTemplaterSettings
    -> Path Abs Dir
    -> Path Abs Dir
    -> Path Rel File
    -> Sem r (Path Rel File)
copyRelFile settings src dst srcFile = do
    dstFile <- translateFile settings srcFile
    let srcAbsFile = src </> srcFile
        dstAbsFile = dst </> dstFile
    copyAbsFile settings
                srcAbsFile
                dstAbsFile
                (writeFileAsByteString dstAbsFile)
    return dstFile

moveRelFile
    :: (Member Resource r, Member FileSystem r, Member Logging r)
    => CompiledTemplaterSettings
    -> Path Abs Dir
    -> Path Abs Dir
    -> Path Rel File
    -> Sem r (Path Rel File, (Path Abs File, Path Rel File))
moveRelFile settings src dst srcFile = do
    dstFile <- translateFile settings srcFile
    let srcAbsFile = src </> srcFile
        dstAbsFile = dst </> dstFile
        dstFileDir = parent dstAbsFile
    ensureDir dstFileDir
    bracket (openBinaryTempFile dstFileDir (toFilePath (filename dstFile)))
            (hClose . snd)
        $ \(tmpFile, tmpHandle) -> do
              copyAbsFile settings srcAbsFile tmpFile (hPutByteString tmpHandle)
              copyPermissions srcAbsFile tmpFile
              logDebug $ "Removing" <+> pretty srcAbsFile
              removeFile srcAbsFile
              return (srcFile, (tmpFile, dstFile))

copy
    :: (Member FileSystem r, Member Logging r, Member Git r)
    => CompiledTemplaterSettings
    -> GitRepository
    -> Path Abs Dir
    -> Sem r [Path Rel File]
copy settings repo dst =
    let src = repositoryPath repo
    in  do
            files <- listFiles repo
            let includedFiles = mfilter (not . shouldExclude settings) files
            traverse (copyRelFile settings src dst) includedFiles

move
    :: (Member Resource r, Member FileSystem r, Member Logging r, Member Git r)
    => CompiledTemplaterSettings
    -> GitRepository
    -> Sem r [Path Rel File]
move settings repo =
    let dir = repositoryPath repo
        renameFile' (a, b) = do
            logDebug $ "Moving" <+> pretty a <+> "to" <+> pretty (dir </> b)
            renameFile a (dir </> b)
    in  do
            files <- listFiles repo
            let includedFiles = mfilter (not . shouldExclude settings) files
            moveResults <- traverse (moveRelFile settings dir dir) includedFiles
            let oldFiles = fmap fst moveResults
                actions  = fmap snd moveResults
            traverse_ renameFile' actions
            let newFiles = fmap snd actions
                changes  = Set.fromList oldFiles <> Set.fromList newFiles
            return $ Set.toList changes
