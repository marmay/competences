{-# LANGUAGE CPP #-}

module Competences.Command.Audience
  ( CommandAudience (..)
  , commandAudience
  , audienceToText
  , audienceFromText
  , audienceRecipients
  )
where

import Competences.Command
  ( Command (..)
  , AbsencesCommand (..)
  , AssignmentsCommand (..)
  , CompetenceAssessmentsCommand (..)
  , CompetenceGridGradesCommand (..)
  , EvidencesCommand (..)
  , ParticipationRecordsCommand (..)
  , SubmissionsCommand (..)
  , EntityCommand (..)
  )
import Competences.Document.Absence (Absence (..))
import Competences.Document.Assignment (Assignment (..))
import Competences.Document.Assessment (CompetenceAssessment (..))
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..))
import Competences.Document.Evidence (Evidence (..))
import Competences.Document.ParticipationRecord (ParticipationRecord (..))
import Competences.Document.Submission (Submission (..), ownerIds)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Audience classification for a command.
--
-- Used to efficiently filter commands for incremental sync:
-- when syncing for a specific user, only commands whose audience
-- includes that user need to be replayed.
data CommandAudience
  = -- | Affects all users (structural changes: competences, grids, tasks, users, etc.)
    AudienceAll
  | -- | Affects only teachers (meso plans, lessons)
    AudienceTeachers
  | -- | Affects all teachers plus specific students
    AudienceTeachersAnd ![UserId]
  | -- | Affects only specific users
    AudienceOnly ![UserId]
  deriving (Eq, Generic, Show)

instance Binary CommandAudience

#ifdef WITH_AESON
instance FromJSON CommandAudience
instance ToJSON CommandAudience
#endif

-- | Convert audience to the text representation stored in the database.
audienceToText :: CommandAudience -> Text
audienceToText AudienceAll = "all"
audienceToText AudienceTeachers = "teachers"
audienceToText (AudienceTeachersAnd _) = "teachers_and_recipients"
audienceToText (AudienceOnly _) = "recipients"

-- | Parse audience from the text representation stored in the database.
-- Recipients must be supplied separately (from the command_recipients table).
audienceFromText :: Text -> [UserId] -> CommandAudience
audienceFromText "all" _ = AudienceAll
audienceFromText "teachers" _ = AudienceTeachers
audienceFromText "teachers_and_recipients" rs = AudienceTeachersAnd rs
audienceFromText "recipients" rs = AudienceOnly rs
audienceFromText _ _ = AudienceAll -- fallback

-- | Extract the specific recipient user IDs from an audience (if any).
-- Returns the list of user IDs that need entries in the command_recipients table.
audienceRecipients :: CommandAudience -> [UserId]
audienceRecipients AudienceAll = []
audienceRecipients AudienceTeachers = []
audienceRecipients (AudienceTeachersAnd uids) = uids
audienceRecipients (AudienceOnly uids) = uids

-- | Determine the audience of a command from its structure alone.
--
-- This is a pure function independent of document state. For Create\/CreateAndLock
-- commands on user-specific entities, we can extract the exact user IDs.
-- For Modify\/Delete on user-specific entities, we conservatively return
-- 'AudienceAll' since we can't determine the affected user without the document.
commandAudience :: Command -> CommandAudience
-- Global structural changes: affect all users
commandAudience (SetDocument _) = AudienceAll
commandAudience (Competences _) = AudienceAll
commandAudience (Users _) = AudienceAll
commandAudience (Tasks _) = AudienceAll
commandAudience (Solutions _) = AudienceAll
commandAudience (Resources _) = AudienceAll
commandAudience (LessonNotes _) = AudienceAll
commandAudience (Migration _) = AudienceAll
-- Teacher-only entities
commandAudience (MesoPlans _) = AudienceTeachers
commandAudience (Lessons _) = AudienceTeachers
-- User-specific entities: extract user IDs from Create/CreateAndLock
commandAudience (Evidences (OnEvidences ec)) = evidenceAudience ec
commandAudience (CompetenceAssessments (OnCompetenceAssessments ec)) = assessmentAudience ec
commandAudience (CompetenceGridGrades (OnCompetenceGridGrades ec)) = gradeAudience ec
commandAudience (Assignments (OnAssignments ec)) = assignmentAudience ec
commandAudience (ParticipationRecords (OnParticipationRecords ec)) = participationAudience ec
commandAudience (Absences (OnAbsences ec)) = absenceAudience ec
commandAudience (Submissions (OnSubmissions ec)) = submissionAudience ec
-- Draft entities: teacher-only
commandAudience (DraftTasks _) = AudienceTeachers
commandAudience (DraftAssignments _) = AudienceTeachers
-- Publish: affects all users (creates real entities visible to students)
commandAudience (Publish _) = AudienceAll

-- Helpers for user-specific entity commands

evidenceAudience :: EntityCommand Evidence patch -> CommandAudience
evidenceAudience (Create ev) = AudienceTeachersAnd (maybeToList ev.userId)
evidenceAudience (CreateAndLock ev) = AudienceTeachersAnd (maybeToList ev.userId)
evidenceAudience _ = AudienceAll

assessmentAudience :: EntityCommand CompetenceAssessment patch -> CommandAudience
assessmentAudience (Create a) = AudienceTeachersAnd [a.userId]
assessmentAudience (CreateAndLock a) = AudienceTeachersAnd [a.userId]
assessmentAudience _ = AudienceAll

gradeAudience :: EntityCommand CompetenceGridGrade patch -> CommandAudience
gradeAudience (Create g) = AudienceTeachersAnd [g.userId]
gradeAudience (CreateAndLock g) = AudienceTeachersAnd [g.userId]
gradeAudience _ = AudienceAll

assignmentAudience :: EntityCommand Assignment patch -> CommandAudience
assignmentAudience (Create a) = AudienceTeachersAnd (Set.toList a.studentIds)
assignmentAudience (CreateAndLock a) = AudienceTeachersAnd (Set.toList a.studentIds)
assignmentAudience _ = AudienceAll

participationAudience :: EntityCommand ParticipationRecord patch -> CommandAudience
participationAudience (Create pr) = AudienceTeachersAnd [pr.userId]
participationAudience (CreateAndLock pr) = AudienceTeachersAnd [pr.userId]
participationAudience _ = AudienceAll

absenceAudience :: EntityCommand Absence patch -> CommandAudience
absenceAudience (Create a) = AudienceTeachersAnd [a.userId]
absenceAudience (CreateAndLock a) = AudienceTeachersAnd [a.userId]
absenceAudience _ = AudienceAll

submissionAudience :: EntityCommand Submission patch -> CommandAudience
submissionAudience (Create s) = AudienceTeachersAnd (ownerIds s.ownership)
submissionAudience (CreateAndLock s) = AudienceTeachersAnd (ownerIds s.ownership)
submissionAudience _ = AudienceAll
