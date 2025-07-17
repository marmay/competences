module Competences.Model.Id
  ( Id(..)
  , nilId
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.UUID (UUID, fromText, toText, nil)

newtype Id a = Id {unId :: UUID}
  deriving (Eq, Show, Ord)

nilId :: Id a
nilId = Id $ nil

instance FromJSON (Id a) where
  parseJSON = withText "Id" $ \t -> case fromText t of
    Nothing -> fail $ "Invalid UUID: " ++ show t
    Just uuid -> return $ Id uuid

instance ToJSON (Id a) where
  toJSON = toJSON . toText . (.unId)
