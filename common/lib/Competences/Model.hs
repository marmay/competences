module Competences.Model
  (
  ) where

import Competences.Model.Competence (Competence, CompetenceIxs)
import Competences.Model.CompetenceGrid (CompetenceGrid)
import Competences.Model.Evidence (Evidence, EvidenceIxs)
import Competences.Model.User (User, UserIxs)
import qualified Data.IxSet.Typed as Ix

data Model = Model
  { competenceGrid :: !CompetenceGrid
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  , evidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , users :: !(Ix.IxSet UserIxs User)
  } deriving (Eq, Show)

