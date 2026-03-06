{-# LANGUAGE CPP #-}

module Competences.Document.FileRef
  ( -- * SHA-256 Hash
    SHA256Hash (..)
  , sha256HashToText
  , sha256HashFromText
    -- * File Reference
  , FileRef (..)
    -- * File Data (ByteString wrapper with JSON support)
  , FileData (..)
  )
where

#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.ByteString.Base64 qualified as Base64
import Data.Text.Encoding qualified as TE
#endif
import Data.Binary (Binary (..))
import Data.ByteString.Lazy qualified as BL
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | SHA-256 hash of file contents, stored as lowercase hex text.
-- Invariant: always 64 lowercase hex characters.
newtype SHA256Hash = SHA256Hash {unSHA256Hash :: Text}
  deriving (Eq, Generic, Ord, Show)

-- | Convert a SHA256Hash to its text representation.
sha256HashToText :: SHA256Hash -> Text
sha256HashToText = (.unSHA256Hash)

-- | Parse a SHA256Hash from text. Validates length (64) and hex characters.
sha256HashFromText :: Text -> Maybe SHA256Hash
sha256HashFromText t
  | T.length t == 64 && T.all isHexChar t = Just (SHA256Hash (T.toLower t))
  | otherwise = Nothing
  where
    isHexChar c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

instance Binary SHA256Hash where
  put = put . (.unSHA256Hash)
  get = SHA256Hash <$> get

#ifdef WITH_AESON
instance FromJSON SHA256Hash where
  parseJSON = withText "SHA256Hash" $ \t ->
    case sha256HashFromText t of
      Nothing -> fail $ "Invalid SHA256 hash: " ++ show t
      Just h -> pure h

instance ToJSON SHA256Hash where
  toJSON = toJSON . (.unSHA256Hash)
#endif

-- | Reference to a file stored in the content-addressable store.
-- The hash uniquely identifies the file contents; fileName and mimeType
-- are metadata for serving the file to clients.
data FileRef = FileRef
  { hash :: !SHA256Hash
  -- ^ SHA-256 hash of the file contents (the CAS key)
  , fileName :: !Text
  -- ^ Original filename for display and Content-Disposition header
  , mimeType :: !Text
  -- ^ MIME type for Content-Type header (e.g., "application/pdf")
  , fileSize :: !Int64
  -- ^ File size in bytes
  }
  deriving (Eq, Generic, Ord, Show)

instance Binary FileRef

#ifdef WITH_AESON
instance FromJSON FileRef where
  parseJSON = withObject "FileRef" $ \v ->
    FileRef
      <$> v .: "hash"
      <*> v .: "fileName"
      <*> v .: "mimeType"
      <*> v .: "fileSize"

instance ToJSON FileRef where
  toJSON ref =
    object
      [ "hash" .= ref.hash
      , "fileName" .= ref.fileName
      , "mimeType" .= ref.mimeType
      , "fileSize" .= ref.fileSize
      ]
#endif

-- | Wrapper around lazy ByteString for file contents.
-- Provides Binary instance (length-prefixed) and JSON instances (base64-encoded).
newtype FileData = FileData {unFileData :: BL.ByteString}
  deriving (Eq, Ord, Show)

instance Binary FileData where
  put = put . (.unFileData)
  get = FileData <$> get

#ifdef WITH_AESON
instance ToJSON FileData where
  toJSON (FileData bs) =
    toJSON $ TE.decodeLatin1 $ Base64.encode $ BL.toStrict bs

instance FromJSON FileData where
  parseJSON = withText "FileData" $ \t ->
    case Base64.decode (TE.encodeUtf8 t) of
      Left err -> fail $ "Invalid base64: " ++ err
      Right bs -> pure $ FileData $ BL.fromStrict bs
#endif
