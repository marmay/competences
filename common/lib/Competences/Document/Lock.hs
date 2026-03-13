{-# LANGUAGE CPP #-}

module Competences.Document.Lock
  ( Lock (..)
  )
where

import Competences.Document.Absence (AbsenceId)
import Competences.Document.Assessment (CompetenceAssessmentId)
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Competence (CompetenceId)
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.CompetenceLevelExample (CompetenceLevelExampleId)
import Competences.Document.CompetenceGridGrade (CompetenceGridGradeId)
import Competences.Document.Evidence (EvidenceId)
import Competences.Document.Lesson (LessonId)
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Document.MesoPlan (MesoPlanId)
import Competences.Document.ParticipationRecord (ParticipationRecordId)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Solution (SolutionId)
import Competences.Document.Submission (SubmissionId)
import Competences.Document.Task (TaskGroupId, TaskId)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..))
#endif
import Data.Binary (Binary (..))
import GHC.Generics (Generic)

-- | A ChangableField is part of an existing entity that
-- can be changed in isolation by providing a new value
-- as a Text.
data Lock
  = CompetenceGridLock !CompetenceGridId
  | CompetenceLock !CompetenceId
  | UserLock !UserId
  | EvidenceLock !EvidenceId
  | TaskLock !TaskId
  | TaskGroupLock !TaskGroupId
  | AssignmentLock !AssignmentId
  | CompetenceAssessmentLock !CompetenceAssessmentId
  | CompetenceGridGradeLock !CompetenceGridGradeId
  | SolutionLock !SolutionId
  | ResourceLock !ResourceId
  | MesoPlanLock !MesoPlanId
  | LessonLock !LessonId
  | ParticipationRecordLock !ParticipationRecordId
  | LessonNotesLock !LessonNotesId
  | AbsenceLock !AbsenceId
  | SubmissionLock !SubmissionId
  | CompetenceLevelExampleLock !CompetenceLevelExampleId
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON Lock

instance ToJSON Lock
#endif

instance Binary Lock
