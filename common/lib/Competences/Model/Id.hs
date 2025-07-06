module Competences.Model.Id
  ( Id(..)
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.UUID (UUID, fromText, toText)

newtype Id a = Id {unId :: UUID}
  deriving (Eq, Show, Ord)

instance FromJSON (Id a) where
  parseJSON = withText "Id" $ \t -> case fromText t of
    Nothing -> fail $ "Invalid UUID: " ++ show t
    Just uuid -> return $ Id uuid

instance ToJSON (Id a) where
  toJSON = toJSON . toText . (.unId)
