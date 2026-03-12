{-# LANGUAGE CPP #-}

module Competences.Command
  ( Command (..)
  , MigrationCommand (..)
  , CommandId
  , handleCommand
  , module Competences.Command.Common
  , module Competences.Command.CompetenceAssessments
  , module Competences.Command.CompetenceGridGrades
  , module Competences.Command.Competences
  , module Competences.Command.Users
  , module Competences.Command.Evidences
  , module Competences.Command.Tasks
  , module Competences.Command.Assignments
  , module Competences.Command.DraftTasks
  , module Competences.Command.DraftAssignments
  , module Competences.Command.Publish
  , module Competences.Command.Solutions
  , module Competences.Command.Resources
  , module Competences.Command.MesoPlans
  , module Competences.Command.Lessons
  , module Competences.Command.LessonNotes
  , module Competences.Command.ParticipationRecords
  , module Competences.Command.Absences
  , module Competences.Command.Submissions
  )
where

import Competences.Command.Absences (AbsencePatch (..), AbsencesCommand (..), handleAbsencesCommand)
import Competences.Command.Assignments (AssignmentPatch (..), AssignmentsCommand (..), handleAssignmentsCommand)
import Competences.Command.DraftAssignments (DraftAssignmentsCommand (..), handleDraftAssignmentsCommand)
import Competences.Command.DraftTasks (DraftTasksCommand (..), handleDraftTasksCommand)
import Competences.Command.Publish (PublishData (..), handlePublish)
import Competences.Command.Resources (ResourcePatch (..), ResourcesCommand (..), handleResourcesCommand)
import Competences.Command.Common (AffectedUsers (..), EntityCommand (..), ModifyCommand (..), UpdateResult)
import Competences.Command.Solutions (SolutionPatch (..), SolutionsCommand (..), handleSolutionsCommand)
import Competences.Command.Submissions (SubmissionPatch (..), SubmissionsCommand (..), handleSubmissionsCommand)
import Competences.Command.CompetenceAssessments (CompetenceAssessmentPatch (..), CompetenceAssessmentsCommand (..), handleCompetenceAssessmentsCommand)
import Competences.Command.CompetenceGridGrades (CompetenceGridGradePatch (..), CompetenceGridGradesCommand (..), handleCompetenceGridGradesCommand)
import Competences.Command.Competences (CompetenceGridPatch (..), CompetencePatch (..), LevelInfoPatch (..), CompetencesCommand (..), handleCompetencesCommand)
import Competences.Command.Evidences (EvidencesCommand (..), EvidencePatch (..), handleEvidencesCommand)
import Competences.Command.Lessons (LessonsCommand (..), LessonPatch (..), handleLessonsCommand)
import Competences.Command.LessonNotes (LessonNotesCommand (..), LessonNotesPatch (..), handleLessonNotesCommand)
import Competences.Command.MesoPlans (MesoPlansCommand (..), MesoPlanPatch (..), handleMesoPlansCommand)
import Competences.Command.ParticipationRecords (ParticipationRecordsCommand (..), ParticipationRecordPatch (..), handleParticipationRecordsCommand)
import Competences.Command.Tasks (TasksCommand (..), TaskPatch (..), TaskGroupPatch (..), SubTaskPatch (..), handleTasksCommand)
import Competences.Command.Users (UsersCommand (..), UserPatch (..), handleUsersCommand)
import Competences.Document (Document (..), Lesson (..), User (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.User (Office365Id (..), UserId, UserRole (..))
import Data.Text (Text)
import Data.Text qualified as T
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import GHC.Generics (Generic)
import Optics.Core ((&), (.~), (%~), (^.))

-- | Migration commands for schema evolution and startup initialization
data MigrationCommand
  = UpdateLessonAssignments ![(LessonId, [AssignmentId])]
  | InitIfEmpty
  | EnsureTeacherO365 !(Id User) !Text
  deriving (Eq, Generic, Show)

instance Binary MigrationCommand

#ifdef WITH_AESON
instance FromJSON MigrationCommand

instance ToJSON MigrationCommand
#endif

-- | Top-level command type wrapping all context commands
data Command
  = SetDocument !Document
  | Competences !CompetencesCommand
  | Users !UsersCommand
  | Evidences !EvidencesCommand
  | Tasks !TasksCommand
  | Assignments !AssignmentsCommand
  | CompetenceAssessments !CompetenceAssessmentsCommand
  | CompetenceGridGrades !CompetenceGridGradesCommand
  | Solutions !SolutionsCommand
  | Resources !ResourcesCommand
  | MesoPlans !MesoPlansCommand
  | Lessons !LessonsCommand
  | LessonNotes !LessonNotesCommand
  | ParticipationRecords !ParticipationRecordsCommand
  | Absences !AbsencesCommand
  | Submissions !SubmissionsCommand
  | DraftTasks !DraftTasksCommand
  | DraftAssignments !DraftAssignmentsCommand
  | Publish !PublishData
  | Migration !MigrationCommand
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance Binary Command

#ifdef WITH_AESON
instance FromJSON Command

instance ToJSON Command
#endif

-- | Handle a command and return the updated document with affected users
handleCommand :: UserId -> Command -> Document -> UpdateResult
handleCommand userId cmd d = case cmd of
  SetDocument newDoc ->
    -- Replace entire document, all users affected
    let allUserIds = map (.id) $ Ix.toList $ newDoc ^. #users
     in Right (newDoc, AffectedUsers allUserIds)
  Competences c -> handleCompetencesCommand userId c d
  Users c -> handleUsersCommand userId c d
  Evidences c -> handleEvidencesCommand userId c d
  Tasks c -> handleTasksCommand userId c d
  Assignments c -> handleAssignmentsCommand userId c d
  CompetenceAssessments c -> handleCompetenceAssessmentsCommand userId c d
  CompetenceGridGrades c -> handleCompetenceGridGradesCommand userId c d
  Solutions c -> handleSolutionsCommand userId c d
  Resources c -> handleResourcesCommand userId c d
  MesoPlans c -> handleMesoPlansCommand userId c d
  Lessons c -> handleLessonsCommand userId c d
  LessonNotes c -> handleLessonNotesCommand userId c d
  ParticipationRecords c -> handleParticipationRecordsCommand userId c d
  Absences c -> handleAbsencesCommand userId c d
  Submissions c -> handleSubmissionsCommand userId c d
  DraftTasks c -> handleDraftTasksCommand userId c d
  DraftAssignments c -> handleDraftAssignmentsCommand userId c d
  Publish pd -> handlePublish pd d
  Migration c -> handleMigrationCommand c d

-- | Handle migration commands (system-level, userId not used)
handleMigrationCommand :: MigrationCommand -> Document -> UpdateResult
handleMigrationCommand (UpdateLessonAssignments updates) d =
  let applyUpdate doc (lid, aids) =
        case Ix.getOne (doc.lessons Ix.@= lid) of
          Nothing -> doc
          Just lesson ->
            let lesson' = lesson & #assignments .~ aids
             in doc & #lessons %~ Ix.insert lesson' . Ix.deleteIx lid
      doc' = foldl' applyUpdate d updates
   in Right (doc', allUsers doc')
handleMigrationCommand InitIfEmpty d
  | Ix.null (d ^. #users) = Right (d, allUsers d)
  | otherwise = Left "Document is not empty"
handleMigrationCommand (EnsureTeacherO365 newId email) d =
  let o365Id = Office365Id email
   in case Ix.getOne (d.users Ix.@= o365Id) of
        Just user
          | user.role == Teacher -> Left "Teacher already exists"
          | otherwise ->
              let user' = user & #role .~ Teacher
                  d' = d & #users %~ Ix.insert user' . Ix.deleteIx user.id
               in Right (d', allUsers d')
        Nothing ->
          let user =
                User
                  { id = newId
                  , name = T.takeWhile (/= '@') email
                  , role = Teacher
                  , office365Id = o365Id
                  }
              d' = d & #users %~ Ix.insert user
           in Right (d', allUsers d')

allUsers :: Document -> AffectedUsers
allUsers d = AffectedUsers $ map (.id) $ Ix.toList $ d ^. #users
