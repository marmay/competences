module Competences.Event
  ( ChangableField (..)
  , Event (..)
  )
where

import Competences.Model.Competence (Competence, CompetenceId, CompetenceLevelId)
import Competences.Model.Evidence (Evidence, EvidenceId)
import Competences.Model.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A ChangableField is part of an existing entity that
-- can be changed in isolation by providing a new value
-- as a Text.
data ChangableField
  = CompetenceGridTitle
  | CompetenceGridDescription
  | CompetenceDescription !CompetenceId
  | CompetenceLevelDescription !CompetenceLevelId
  deriving (Eq, Generic, Ord, Show)

data Event
  = FieldLocked !ChangableField !UserId
  | FieldReleased !ChangableField !(Maybe Text)
  | CompetenceAdded !Competence
  | CompetenceRemoved !CompetenceId
  | EvidenceAdded !Evidence
  | EvidenceRemoved !EvidenceId
  deriving (Eq, Generic, Show)

instance FromJSON ChangableField

instance ToJSON ChangableField

instance FromJSON Event

instance ToJSON Event

-- data AffectedUsers
--   = AllUsers
--   | AllTeachers
--   | AllTeachersAndSpecificStudents ![UserId]
--
-- affectedUsersOf :: Event -> AffectedUsers
-- affectedUsersOf (FieldLocked _ _) = AllTeachers
