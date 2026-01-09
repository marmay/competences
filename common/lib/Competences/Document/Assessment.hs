module Competences.Document.Assessment
  ( CompetenceAssessment (..)
  , CompetenceAssessmentId
  , CompetenceAssessmentIxs
  , nilCompetenceAssessment
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Document.Competence (CompetenceId, Level (..))
import Competences.Document.Id (Id, nilId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

type CompetenceAssessmentId = Id CompetenceAssessment

-- | A competence assessment is a teacher's judgment of what level a student
-- has demonstrated for a specific competence.
--
-- Assessments become "stale" when new evidence is added after the assessment date.
-- Staleness is computed, not stored (see 'isCompetenceAssessmentStale').
--
-- Multiple assessments can exist for the same (userId, competenceId) pair;
-- the most recent by date is considered "active".
data CompetenceAssessment = CompetenceAssessment
  { id :: !CompetenceAssessmentId
  , userId :: !UserId
  -- ^ The student being assessed
  , competenceId :: !CompetenceId
  -- ^ The competence being assessed
  , level :: !Level
  -- ^ The level the student has demonstrated (Basic/Intermediate/Advanced)
  , date :: !Day
  -- ^ When this assessment was created
  , comment :: !(Maybe Text)
  -- ^ Optional teacher notes
  }
  deriving (Eq, Generic, Ord, Show)

-- | Indexes for efficient querying:
-- - By ID (primary key)
-- - By UserId (for projection and per-student queries)
-- - By CompetenceId (for per-competence queries)
-- - By Day (for date-based queries)
type CompetenceAssessmentIxs = '[CompetenceAssessmentId, UserId, CompetenceId, Day]

instance Ix.Indexable CompetenceAssessmentIxs CompetenceAssessment where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.userId))
      (Ix.ixFun $ singleton . (.competenceId))
      (Ix.ixFun $ singleton . (.date))

-- | Default/nil assessment for initialization
nilCompetenceAssessment :: CompetenceAssessment
nilCompetenceAssessment =
  CompetenceAssessment
    { id = nilId
    , userId = nilId
    , competenceId = nilId
    , level = BasicLevel
    , date = fromGregorian 2025 1 1
    , comment = Nothing
    }

instance FromJSON CompetenceAssessment

instance ToJSON CompetenceAssessment

instance Binary CompetenceAssessment
