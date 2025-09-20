module Competences.Document.ChangableField
  ( ChangableField (..)
  )
where

import Competences.Document.Competence (CompetenceId, CompetenceLevelId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Binary (Binary (..))
import GHC.Generics (Generic)

-- | A ChangableField is part of an existing entity that
-- can be changed in isolation by providing a new value
-- as a Text.
data ChangableField
  = CompetenceGridTitle
  | CompetenceGridDescription
  | CompetenceDescription !CompetenceId
  | CompetenceLevelDescription !CompetenceLevelId
  | UserName !UserId
  | UserRole !UserId
  deriving (Eq, Generic, Ord, Show)

instance FromJSON ChangableField

instance ToJSON ChangableField

instance Binary ChangableField
