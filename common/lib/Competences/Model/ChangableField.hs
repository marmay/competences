module Competences.Model.ChangableField
  ( ChangableField (..)
  )
where

import Competences.Model.Competence (CompetenceId, CompetenceLevelId)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | A ChangableField is part of an existing entity that
-- can be changed in isolation by providing a new value
-- as a Text.
data ChangableField
  = CompetenceGridTitle
  | CompetenceGridDescription
  | CompetenceDescription !CompetenceId
  | CompetenceLevelDescription !CompetenceLevelId
  deriving (Eq, Generic, Ord, Show)

instance FromJSON ChangableField

instance ToJSON ChangableField
