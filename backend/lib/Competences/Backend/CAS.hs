-- | Content-Addressable Store (CAS) for binary files.
--
-- Files are stored by their SHA-256 hash in a flat directory structure
-- using the first two hex characters as a subdirectory prefix:
--
-- @
--   \<root\>/ab/abcdef0123456789...  (full 64-char hex hash as filename)
-- @
--
-- Writes use write-to-temp-then-atomic-rename for safety.
-- Content-addressed writes are naturally idempotent.
module Competences.Backend.CAS
  ( CAS (..)
  , InstanceId
  , newCAS
  , storeFile
  , fetchFile
  , fileExists
  , casFilePath
  , registerOwner
  , isHashRegistered
  , storeAndRegister
  , computeSHA256
  )
where

import Competences.Document.FileRef (SHA256Hash (..), sha256HashFromText)
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

-- | A content-addressable store backed by a filesystem directory.
data CAS = CAS
  { rootDir :: FilePath
  , fileMode :: FileMode
  -- ^ Mode applied to stored blobs after the atomic rename, since
  -- 'openBinaryTempFile' hardcodes @0600@ and ignores the process
  -- umask. Use this to opt blobs into the group sharing the CAS
  -- directory (sgid'd in the systemd unit); typical value @0o640@.
  }

-- | Create a new CAS handle, ensuring the root directory exists.
newCAS :: FilePath -> FileMode -> IO CAS
newCAS root mode = do
  createDirectoryIfMissing True root
  pure (CAS root mode)

-- | Store file contents in the CAS.
--
-- Returns the SHA-256 hash of the contents. If a file with the same hash
-- already exists, the write is a no-op (content-addressed idempotency).
--
-- Uses write-to-temp-then-atomic-rename:
--   1. Write contents to a temp file in the CAS root
--   2. Compute SHA-256 hash
--   3. Rename temp file to final path (atomic on POSIX)
storeFile :: CAS -> BL.ByteString -> IO (SHA256Hash, Int64)
storeFile cas contents = do
  let hashText = computeSHA256 contents
      size = fromIntegral (BL.length contents) :: Int64
  case sha256HashFromText hashText of
    Nothing -> error $ "CAS.storeFile: impossible - computed invalid hash: " ++ T.unpack hashText
    Just sha -> do
      let finalPath = casFilePath cas sha
          subdir = casSubdir cas sha
      -- Check if already stored (idempotent)
      exists <- doesFileExist finalPath
      if exists
        then pure (sha, size)
        else do
          -- Ensure subdirectory exists
          createDirectoryIfMissing True subdir
          -- Write to temp file in CAS root, then atomic rename.
          -- 'openBinaryTempFile' opens at mode 0600 regardless of the
          -- process umask, so we explicitly reapply the configured
          -- mode after the rename.
          (tmpPath, tmpHandle) <- openBinaryTempFile cas.rootDir "cas-upload"
          BL.hPut tmpHandle contents
          hClose tmpHandle
          renameFile tmpPath finalPath
          setFileMode finalPath cas.fileMode
          pure (sha, size)

-- | Fetch file contents from the CAS.
-- Returns Nothing if the file doesn't exist.
fetchFile :: CAS -> SHA256Hash -> IO (Maybe BL.ByteString)
fetchFile cas sha = do
  let path = casFilePath cas sha
  exists <- doesFileExist path
  if exists
    then Just <$> BL.readFile path
    else pure Nothing

-- | Check if a file exists in the CAS.
fileExists :: CAS -> SHA256Hash -> IO Bool
fileExists cas sha = doesFileExist (casFilePath cas sha)

-- | Compute the filesystem path for a given hash within the CAS.
casFilePath :: CAS -> SHA256Hash -> FilePath
casFilePath cas sha =
  let hex = T.unpack sha.unSHA256Hash
   in cas.rootDir </> take 2 hex </> hex

-- | Compute the subdirectory path for a given hash.
casSubdir :: CAS -> SHA256Hash -> FilePath
casSubdir cas sha =
  let hex = T.unpack sha.unSHA256Hash
   in cas.rootDir </> take 2 hex

-- | An instance identifier, typically the PostgreSQL database name.
-- Unique per backend instance, human-readable.
type InstanceId = Text

-- | Register this instance as an owner of a hash.
-- Creates @\<cas-root\>/refs/\<prefix\>/\<hash\>/\<instance-id\>@ (empty file).
-- Idempotent: re-registering is a no-op.
registerOwner :: CAS -> InstanceId -> SHA256Hash -> IO ()
registerOwner cas instId sha = do
  let dir = refsDir cas sha
  createDirectoryIfMissing True dir
  let path = dir </> T.unpack instId
  exists <- doesFileExist path
  unless exists $ writeFile path ""

-- | Check if this instance has registered ownership of a hash.
isHashRegistered :: CAS -> InstanceId -> SHA256Hash -> IO Bool
isHashRegistered cas instId sha = do
  let path = refsDir cas sha </> T.unpack instId
  doesFileExist path

-- | Store file contents in the CAS and register ownership in one call.
storeAndRegister :: CAS -> InstanceId -> BL.ByteString -> IO (SHA256Hash, Int64)
storeAndRegister cas instId contents = do
  result@(sha, _) <- storeFile cas contents
  registerOwner cas instId sha
  pure result

-- | Compute the refs directory for a given hash.
-- @\<cas-root\>/refs/\<first-2-hex\>/\<hash\>/@
refsDir :: CAS -> SHA256Hash -> FilePath
refsDir cas sha =
  let hex = T.unpack sha.unSHA256Hash
   in cas.rootDir </> "refs" </> take 2 hex </> hex

-- | Compute SHA-256 hash of lazy ByteString contents, returning lowercase hex text.
computeSHA256 :: BL.ByteString -> Text
computeSHA256 contents =
  let digest :: Digest SHA256 = hashlazy contents
   in T.toLower $ decodeUtf8 $ convertToBase Base16 digest
