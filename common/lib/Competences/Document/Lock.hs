module Competences.Document.Lock
  ( Lock (..)
  )
where

import Competences.Document.Competence (CompetenceId)
import Competences.Document.Evidence (EvidenceId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Binary (Binary (..))
import GHC.Generics (Generic)

-- | A ChangableField is part of an existing entity that
-- can be changed in isolation by providing a new value
-- as a Text.
data Lock
  = CompetenceGridTitleLock
  | CompetenceGridDescriptionLock
  | CompetenceLock !CompetenceId
  | UserLock !UserId
  | EvidenceLock !EvidenceId
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Lock

instance ToJSON Lock

instance Binary Lock
