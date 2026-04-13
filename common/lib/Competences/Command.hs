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
  , module Competences.Command.CompetenceLevelExamples
  , module Competences.Command.Layouts
  )
where

import Competences.Command.Absences (AbsencePatch (..), AbsencesCommand (..), handleAbsencesCommand)
import Competences.Command.CompetenceLevelExamples (CompetenceLevelExamplePatch (..), CompetenceLevelExamplesCommand (..), handleCompetenceLevelExamplesCommand)
import Competences.Command.Assignments (AssignmentPatch (..), AssignmentsCommand (..), handleAssignmentsCommand)
import Competences.Command.DraftAssignments (DraftAssignmentsCommand (..), handleDraftAssignmentsCommand)
import Competences.Command.DraftTasks (DraftTasksCommand (..), handleDraftTasksCommand)
import Competences.Command.Publish (PublishData (..), handlePublish)
import Competences.Command.Resources (ResourcePatch (..), ResourcesCommand (..), handleResourcesCommand)
import Competences.Command.Common (AffectedUsers (..), CommandContext (..), EntityCommand (..), ModifyCommand (..), UpdateResult, requireTeacher)
import Competences.Command.Solutions (SolutionPatch (..), SolutionsCommand (..), handleSolutionsCommand)
import Competences.Command.Submissions (SubmissionPatch (..), SubmissionsCommand (..), handleSubmissionsCommand)
import Competences.Command.CompetenceAssessments (CompetenceAssessmentPatch (..), CompetenceAssessmentsCommand (..), handleCompetenceAssessmentsCommand)
import Competences.Command.CompetenceGridGrades (CompetenceGridGradePatch (..), CompetenceGridGradesCommand (..), handleCompetenceGridGradesCommand)
import Competences.Command.Competences (CompetenceGridPatch (..), CompetencePatch (..), LevelInfoPatch (..), CompetencesCommand (..), handleCompetencesCommand)
import Competences.Command.Evidences (EvidencesCommand (..), EvidencePatch (..), handleEvidencesCommand)
import Competences.Command.Layouts (LayoutsCommand (..), LayoutPatch (..), handleLayoutsCommand)
import Competences.Command.Lessons (LessonsCommand (..), LessonPatch (..), handleLessonsCommand)
import Competences.Command.LessonNotes (LessonNotesCommand (..), LessonNotesPatch (..), handleLessonNotesCommand)
import Competences.Command.MesoPlans (MesoPlansCommand (..), MesoPlanPatch (..), handleMesoPlansCommand)
import Competences.Command.ParticipationRecords (ParticipationRecordsCommand (..), ParticipationRecordPatch (..), handleParticipationRecordsCommand)
import Competences.Command.Tasks (TasksCommand (..), TaskPatch (..), handleTasksCommand)
import Competences.Command.Users (UsersCommand (..), UserPatch (..), handleUsersCommand)
import Competences.Document (Document (..), Lesson (..), User (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId)
import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.Lock (Lock)
import Competences.Document.User (Office365Id (..), UserRole (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..))
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
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
  | SortAssignmentTasksByIdentifier
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
  | CompetenceLevelExamples !CompetenceLevelExamplesCommand
  | Layouts !LayoutsCommand
  | Publish !PublishData
  | Migration !MigrationCommand
  | Unlock !Lock
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance Binary Command

#ifdef WITH_AESON
instance FromJSON Command

instance ToJSON Command
#endif

-- | Handle a command and return the updated document with affected users
handleCommand :: CommandContext -> Command -> Document -> UpdateResult
handleCommand cmdCtx cmd d = case cmd of
  SetDocument newDoc ->
    -- Replace entire document, all users affected
    let allUserIds = map (.id) $ Ix.toList $ newDoc ^. #users
     in Right (newDoc, AffectedUsers allUserIds)
  -- Teacher-only commands: require the acting user to be a teacher
  Competences c -> teacherOnly $ handleCompetencesCommand cmdCtx c d
  Users c -> teacherOnly $ handleUsersCommand cmdCtx c d
  Evidences c -> teacherOnly $ handleEvidencesCommand cmdCtx c d
  Tasks c -> teacherOnly $ handleTasksCommand cmdCtx c d
  Assignments c -> teacherOnly $ handleAssignmentsCommand cmdCtx c d
  CompetenceAssessments c -> teacherOnly $ handleCompetenceAssessmentsCommand cmdCtx c d
  CompetenceGridGrades c -> teacherOnly $ handleCompetenceGridGradesCommand cmdCtx c d
  Solutions c -> teacherOnly $ handleSolutionsCommand cmdCtx c d
  Resources c -> teacherOnly $ handleResourcesCommand cmdCtx c d
  MesoPlans c -> teacherOnly $ handleMesoPlansCommand cmdCtx c d
  Lessons c -> teacherOnly $ handleLessonsCommand cmdCtx c d
  LessonNotes c -> teacherOnly $ handleLessonNotesCommand cmdCtx c d
  ParticipationRecords c -> teacherOnly $ handleParticipationRecordsCommand cmdCtx c d
  Absences c -> teacherOnly $ handleAbsencesCommand cmdCtx c d
  DraftTasks c -> teacherOnly $ handleDraftTasksCommand cmdCtx c d
  DraftAssignments c -> teacherOnly $ handleDraftAssignmentsCommand cmdCtx c d
  CompetenceLevelExamples c -> teacherOnly $ handleCompetenceLevelExamplesCommand cmdCtx c d
  Layouts c -> teacherOnly $ handleLayoutsCommand cmdCtx c d
  -- Student commands: submissions have their own role checks (requires Student)
  Submissions c -> handleSubmissionsCommand cmdCtx c d
  -- System commands: no user role check needed
  Publish pd -> handlePublish pd d
  Migration c -> handleMigrationCommand c d
  -- Lock cleanup: permissive and idempotent — server validates ownership/staleness
  -- before persisting. Safe during replay even if the lock was already released.
  Unlock lock -> Right (d & #locks %~ Map.delete lock, mempty)
  where
    teacherOnly result = requireTeacher cmdCtx.userId d >> result

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
handleMigrationCommand SortAssignmentTasksByIdentifier d =
  let lookupIdentifier taskSet tid =
        case Ix.getOne (taskSet Ix.@= tid) of
          Just t -> t.identifier
          Nothing -> TaskIdentifier ""
      sortTasks taskSet a =
        a & #tasks %~ sortOn (lookupIdentifier taskSet)
      d' =
        d
          & #assignments %~ Ix.fromList . map (sortTasks d.tasks) . Ix.toList
          & #draftAssignments %~ Ix.fromList . map (sortTasks d.draftTasks) . Ix.toList
   in Right (d', allUsers d')

allUsers :: Document -> AffectedUsers
allUsers d = AffectedUsers $ map (.id) $ Ix.toList $ d ^. #users
