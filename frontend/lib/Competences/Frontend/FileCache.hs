-- | Client-side file cache for CAS files received over WebSocket.
--
-- Follows the same IORef-based pattern as 'Competences.Frontend.SvgEmbed.Manager.FormulaCache'.
-- Files are cached by their SHA-256 hash and can be converted to data URLs
-- for embedding in @\<img\>@ elements.
module Competences.Frontend.FileCache
  ( FileCache (..)
  , newFileCache
  , lookupFile
  , insertFile
  , fileToDataUrl
  )
where

import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Competences.Document.FileRef (FileData (..), SHA256Hash)

-- | Cache of files received from the server, keyed by content hash.
newtype FileCache = FileCache (IORef (Map SHA256Hash BL.ByteString))

-- | Create a new, empty file cache.
newFileCache :: IO FileCache
newFileCache = FileCache <$> newIORef Map.empty

-- | Look up a file in the cache by its content hash.
lookupFile :: FileCache -> SHA256Hash -> IO (Maybe BL.ByteString)
lookupFile (FileCache ref) hash = do
  cache <- readIORef ref
  pure $ Map.lookup hash cache

-- | Insert a file into the cache.
insertFile :: FileCache -> SHA256Hash -> FileData -> IO ()
insertFile (FileCache ref) hash (FileData bs) =
  atomicModifyIORef' ref $ \cache ->
    (Map.insert hash bs cache, ())

-- | Convert file contents to a data URL for @\<img\>@ embedding.
--
-- Produces: @data:\<mimeType\>;base64,\<base64-encoded-contents\>@
--
-- Uses the same base64 encoding pattern as 'svgToDataUrl' in SvgEmbed.Manager.
fileToDataUrl :: Text -> BL.ByteString -> Text
fileToDataUrl mimeType bs =
  T.concat
    [ "data:"
    , mimeType
    , ";base64,"
    , TE.decodeLatin1 $ Base64.encode $ BL.toStrict bs
    ]
