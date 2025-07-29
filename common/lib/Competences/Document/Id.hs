module Competences.Document.Id
  ( Id (..)
  , mkId
  , nilId
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Binary (Binary (..))
import Data.Text (Text)
import Data.UUID (UUID, fromText, nil, toText)
import System.Random (Random)

newtype Id a = Id {unId :: UUID}
  deriving (Eq, Show, Ord, Random)

nilId :: Id a
nilId = Id $ nil

mkId :: Text -> Maybe (Id a)
mkId t = Id <$> fromText t

instance FromJSON (Id a) where
  parseJSON = withText "Id" $ \t -> case fromText t of
    Nothing -> fail $ "Invalid UUID: " ++ show t
    Just uuid -> return $ Id uuid

instance ToJSON (Id a) where
  toJSON = toJSON . toText . (.unId)

instance Binary (Id a) where
  put = put . (.unId)
  get = do
    uuid <- get @UUID
    pure $ Id uuid
