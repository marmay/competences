module Competences.Document.CompetenceGridGrade
  ( CompetenceGridGrade (..)
  , CompetenceGridGradeId
  , CompetenceGridGradeIxs
  , nilCompetenceGridGrade
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Grade (Grade (..))
import Competences.Document.Id (Id, nilId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

type CompetenceGridGradeId = Id CompetenceGridGrade

-- | A competence grid grade is a teacher's overall grade for a student
-- on a specific competence grid (e.g., "Mathematics" or "English").
--
-- This aggregates the individual competence assessments within that grid
-- into a single Austrian school grade.
--
-- Grid grades become "stale" when:
-- - Any competence assessment within the grid is updated after this grade's date
-- - Any competence assessment within the grid is itself stale
--
-- Multiple grades can exist for the same (userId, competenceGridId) pair;
-- the most recent by date is considered "active".
data CompetenceGridGrade = CompetenceGridGrade
  { id :: !CompetenceGridGradeId
  , userId :: !UserId
  -- ^ The student being graded
  , competenceGridId :: !CompetenceGridId
  -- ^ The competence grid being graded
  , grade :: !Grade
  -- ^ Austrian school grade (1, 1-2, 2, 2-3, 3, 3-4, 4, 4-5, 5)
  , date :: !Day
  -- ^ When this grade was given
  , comment :: !(Maybe Text)
  -- ^ Optional teacher notes
  }
  deriving (Eq, Generic, Ord, Show)

-- | Indexes for efficient querying:
-- - By ID (primary key)
-- - By UserId (for projection and per-student queries)
-- - By CompetenceGridId (for per-grid queries)
-- - By Day (for date-based queries)
type CompetenceGridGradeIxs = '[CompetenceGridGradeId, UserId, CompetenceGridId, Day]

instance Ix.Indexable CompetenceGridGradeIxs CompetenceGridGrade where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.userId))
      (Ix.ixFun $ singleton . (.competenceGridId))
      (Ix.ixFun $ singleton . (.date))

-- | Default/nil grade for initialization
nilCompetenceGridGrade :: CompetenceGridGrade
nilCompetenceGridGrade =
  CompetenceGridGrade
    { id = nilId
    , userId = nilId
    , competenceGridId = nilId
    , grade = Grade3
    , date = fromGregorian 2025 1 1
    , comment = Nothing
    }

instance FromJSON CompetenceGridGrade

instance ToJSON CompetenceGridGrade

instance Binary CompetenceGridGrade
