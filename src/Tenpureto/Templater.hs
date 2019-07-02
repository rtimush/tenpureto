module Tenpureto.Templater where

import           Polysemy
import           Polysemy.Resource

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.HashMap.Strict.InsOrd     ( InsOrdHashMap )
import qualified Data.HashMap.Strict.InsOrd    as InsOrdHashMap
import           Data.Maybe
import           Data.Foldable
import           Control.Monad
import qualified Data.Text.ICU                 as ICU
import           Data.Text.Encoding
import           Data.Text.ICU.Replace
import           System.FilePath.Glob

import           Tenpureto.Effects.Logging
import           Tenpureto.Effects.FileSystem
import           Tenpureto.Effects.Git
import           Tenpureto.Orphanage            ( )

import           Templater.CaseConversion

data TemplaterSettings = TemplaterSettings
    { templaterFromVariables :: InsOrdHashMap Text Text
    , templaterToVariables :: InsOrdHashMap Text Text
    , templaterExcludes :: Set Text
    }

data CompiledTemplaterSettings = CompiledTemplaterSettings
    { translate :: Text -> Text
    , shouldExclude :: Path Rel File -> Bool
    }

data TemplaterException = TemplaterException
    deriving Show

expandReplacement :: (Text, Text) -> [(Text, Text)]
expandReplacement (a, b) =
    [ (templateValueText av, templateValueText bv)
    | av <- variations a
    , bv <- variations b
    , sameStyle av bv
    ]
  where
    variations text = Set.toList . Set.fromList $ concatMap
        styleVariations
        (textToTemplateValues text)

expandReplacements
    :: InsOrdHashMap Text Text -> InsOrdHashMap Text Text -> [(Text, Text)]
expandReplacements fv tv = concatMap expandReplacement
    $ InsOrdHashMap.elems (InsOrdHashMap.intersectionWith (,) fv tv)

compileSettings
    :: Member Logging r => TemplaterSettings -> Sem r CompiledTemplaterSettings
compileSettings TemplaterSettings { templaterFromVariables = tfv, templaterToVariables = ttv, templaterExcludes = ex }
    = let
          replacements = expandReplacements tfv ttv
          excludes     = compileExcludes ex
      in
          do
              logDebug $ "From variables:" <+> align
                  (sep
                      [ pretty f <+> "->" <+> pretty t
                      | (f, t) <- InsOrdHashMap.toList tfv
                      ]
                  )
              logDebug $ "To variables:" <+> align
                  (sep
                      [ pretty f <+> "->" <+> pretty t
                      | (f, t) <- InsOrdHashMap.toList ttv
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
replaceVariables rules = replaceAll regex replacement
  where
    rulesMap = Map.fromList rules
    quote text = "\\Q" <> T.replace "\\E" "\\\\E" text <> "\\E"
    regex = ICU.regex [] $ T.intercalate "|" $ fmap (quote . fst) rules
    findReplacement mtch = fromMaybe matchText (Map.lookup matchText rulesMap)
        where matchText = fold $ ICU.group 0 mtch
    replacement = rtfn findReplacement

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
    let
        handleLeadingSlash p = maybe [p, "**/" <> p] pure (T.stripPrefix "/" p)
        handleTrailingSlash p =
            maybe [p, p <> "/**"] (pure . (<> "/**")) (T.stripSuffix "/" p)
        transformPattern = handleLeadingSlash >=> handleTrailingSlash
        patterns =
            fmap (simplify . compile . T.unpack)
                $   transformPattern
                =<< Set.toList excludes
        matches :: [FilePath -> Bool]
        matches = fmap match patterns
        fab ?? a = fmap ($ a) fab
    in
        or . (??) matches . fromRelFile

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