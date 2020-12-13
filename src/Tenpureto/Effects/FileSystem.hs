{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tenpureto.Effects.FileSystem
    ( module Tenpureto.Effects.FileSystem
    , FilePath
    , Path
    , Rel
    , Abs
    , File
    , Dir
    , (</>)
    , fromRelFile
    , toFilePath
    , parent
    , filename
    ) where

import           Polysemy
import           Polysemy.Resource

import           Data.Bool
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS

import qualified Control.Exception             as E

import           Path
import qualified Path.IO
import qualified System.Directory
import           System.IO                      ( Handle )
import qualified System.IO

data FileSystem m a where
    ParseRelFile ::FilePath -> FileSystem m (Path Rel File)
    EnsureDir ::Path b Dir -> FileSystem m ()
    EnsureEmptyDir ::Path b Dir -> FileSystem m ()
    ResolveDir ::FilePath -> FileSystem m (Path Abs Dir)
    ResolveFile ::FilePath -> FileSystem m (Path Abs File)
    IsSymlink ::Path Abs File -> FileSystem m Bool
    GetSymbolicLinkDirTarget::Path b t -> FileSystem m FilePath
    CreateDirectoryLink ::FilePath -> Path Abs File -> FileSystem m ()
    CopyPermissions ::Path Abs File -> Path Abs File -> FileSystem m ()
    RenameFile ::Path Abs File -> Path Abs File -> FileSystem m ()
    RemoveFile ::Path Abs File -> FileSystem m ()
    ReadFileAsByteString ::Path b File -> FileSystem m ByteString
    ReadFileAsMaybeByteString ::Path b File -> FileSystem m (Maybe ByteString)
    WriteFileAsByteString ::Path b File -> ByteString -> FileSystem m ()
    OpenBinaryTempFile ::Path Abs Dir -> FilePath -> FileSystem m (Path Abs File, Handle)
    CreateSystemTempDir ::FilePath -> FileSystem m (Path Abs Dir)
    RemoveDirRecur ::Path Abs Dir -> FileSystem m ()
    HPutByteString::Handle -> ByteString -> FileSystem m ()
    HClose ::Handle -> FileSystem m ()

makeSem ''FileSystem


withSystemTempDir
    :: Members '[FileSystem , Resource] r
    => FilePath
    -> (Path Abs Dir -> Sem r a)
    -> Sem r a
withSystemTempDir template =
    bracket (createSystemTempDir template) removeDirRecur


runFileSystemIO :: Member (Embed IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret $ \case
    ParseRelFile   filePath -> embed $ Path.parseRelFile filePath
    EnsureDir      dir      -> embed $ Path.IO.ensureDir dir
    EnsureEmptyDir dir      -> embed $ do
        Path.IO.createDirIfMissing False dir
        Path.IO.listDir dir >>= \(dirs, files) -> bool
            (E.throwIO $ userError "Directory is not empty")
            (return ())
            (null dirs && null files)
    ResolveDir path -> embed $ E.catch
        (Path.IO.resolveDir' path)
        (\e -> let _ = (e :: PathException) in Path.parseAbsDir path)
    ResolveFile path -> embed $ E.catch
        (Path.IO.resolveFile' path)
        (\e -> let _ = (e :: PathException) in Path.parseAbsFile path)
    IsSymlink file -> embed $ Path.IO.isSymlink file
    GetSymbolicLinkDirTarget path ->
        embed $ System.Directory.getSymbolicLinkTarget (toFilePath path)
    CreateDirectoryLink dst file ->
        embed $ System.Directory.createDirectoryLink dst (toFilePath file)
    CopyPermissions src  dst  -> embed $ Path.IO.copyPermissions src dst
    RenameFile      from to   -> embed $ Path.IO.renameFile from to
    RemoveFile           file -> embed $ Path.IO.removeFile file
    ReadFileAsByteString file -> embed $ BS.readFile (toFilePath file)
    ReadFileAsMaybeByteString file ->
        embed $ Path.IO.forgivingAbsence $ BS.readFile (toFilePath file)
    WriteFileAsByteString file content ->
        embed $ BS.writeFile (toFilePath file) content
    OpenBinaryTempFile dir template ->
        embed $ Path.IO.openBinaryTempFile dir template
    CreateSystemTempDir template ->
        embed $ Path.IO.getTempDir >>= flip Path.IO.createTempDir template
    RemoveDirRecur dir            -> embed $ Path.IO.removeDirRecur dir
    HPutByteString handle content -> embed $ BS.hPut handle content
    HClose handle                 -> embed $ System.IO.hClose handle
