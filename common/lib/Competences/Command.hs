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
  , module Competences.Command.TeachingNotes
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
import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (CommandContext (..), EntityCommand (..), ModifyCommand (..), UpdateResult, requireTeacher)
import Competences.Command.Solutions (SolutionPatch (..), SolutionsCommand (..), handleSolutionsCommand)
import Competences.Command.Submissions (SubmissionPatch (..), SubmissionsCommand (..), handleSubmissionsCommand)
import Competences.Command.CompetenceAssessments (CompetenceAssessmentPatch (..), CompetenceAssessmentsCommand (..), handleCompetenceAssessmentsCommand)
import Competences.Command.CompetenceGridGrades (CompetenceGridGradePatch (..), CompetenceGridGradesCommand (..), handleCompetenceGridGradesCommand)
import Competences.Command.Competences (CompetenceGridPatch (..), CompetencePatch (..), LevelInfoPatch (..), CompetencesCommand (..), handleCompetencesCommand)
import Competences.Command.Evidences (EvidencesCommand (..), EvidencePatch (..), handleEvidencesCommand)
import Competences.Command.Layouts (LayoutsCommand (..), LayoutPatch (..), handleLayoutsCommand)
import Competences.Command.Lessons (LessonsCommand (..), LessonPatch (..), handleLessonsCommand)
import Competences.Command.TeachingNotes (TeachingNotesCommand (..), handleTeachingNotesCommand)
import Competences.Command.MesoPlans (MesoPlansCommand (..), MesoPlanPatch (..), handleMesoPlansCommand)
import Competences.Command.ParticipationRecords (ParticipationRecordsCommand (..), ParticipationRecordPatch (..), handleParticipationRecordsCommand)
import Competences.Command.Tasks (TasksCommand (..), TaskPatch (..), handleTasksCommand)
import Competences.Command.Users (UsersCommand (..), UserPatch (..), handleUsersCommand)
import Competences.Document (Document (..), User (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId)
import Competences.Document.Id (Id)
import Competences.Document.Lesson (Lesson (..), LessonId)
import Competences.Document.Lock (Lock)
import Competences.Document.User (EntraOid (..), Office365Id (..), UserRole (..))
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

-- | System-submitted commands: schema evolution, startup
-- initialization, and server-initiated identity maintenance (the
-- login handler's lazy oid binding and stub completion). Despite the
-- name, this is not a migrations-only bucket — it is the command
-- channel dispatched without a per-command role check ('handleCommand'
-- routes it past teacherOnly), which server-submitted writes on behalf
-- of arbitrary users require. Client reach is gated one layer up:
-- WebSocket.isAuthorized only lets students submit Submissions, so
-- students can never send these; teachers can, which is within their
-- existing trust (they may edit users anyway). Renaming the
-- type/constructors would change the persisted JSON wire format (the
-- command log stores constructor names), so the name stays until that
-- migration is worth doing. Only append constructors — the log
-- replays old encodings.
data MigrationCommand
  = UpdateLessonAssignments ![(LessonId, [AssignmentId])]
  | InitIfEmpty
  | EnsureTeacherO365 !(Id User) !Text
  | SortAssignmentTasksByIdentifier
  | BindEntraOid !(Id User) !Text
    -- ^ Bind the Entra object id on first login (lazy oid binding;
    -- submitted by the login handler after an address match).
  | CompleteUserIdentity !(Id User) !Text !Text
    -- ^ Fill an oid-provisioned stub's display fields on first login
    -- (address upn when empty, name when empty or still the raw oid
    -- text), from the assertion. Submitted by the login handler.
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
  | TeachingNotes !TeachingNotesCommand
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
    Right (newDoc, AudienceAll)
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
  TeachingNotes c -> teacherOnly $ handleTeachingNotesCommand cmdCtx c d
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
  -- All clients need to learn the lock was removed (UI lock indicators clear).
  Unlock lock -> Right (d & #locks %~ Map.delete lock, AudienceAll)
  where
    teacherOnly result = requireTeacher cmdCtx.userId d >> result

-- | Handle migration commands (system-level, userId not used)
handleMigrationCommand :: MigrationCommand -> Document -> UpdateResult
handleMigrationCommand (UpdateLessonAssignments updates) d =
  let applyUpdate :: Document -> (LessonId, [AssignmentId]) -> Document
      applyUpdate doc (lid, aids) =
        case Ix.getOne (doc.lessons Ix.@= lid) of
          Nothing -> doc
          Just lesson ->
            let lesson' = lesson & #assignments .~ aids
             in doc & #lessons %~ Ix.insert lesson' . Ix.deleteIx lid
      doc' = foldl' applyUpdate d updates
   in Right (doc', AudienceAll)
handleMigrationCommand InitIfEmpty d
  | Ix.null (d ^. #users) = Right (d, AudienceAll)
  | otherwise = Left "Document is not empty"
handleMigrationCommand (EnsureTeacherO365 newId identifier) d =
  -- Dual form: an address binds by office365Id; anything without '@'
  -- is treated as an Entra object id and binds by entraOid.
  let byAddress = "@" `T.isInfixOf` identifier
      normalized = T.toLower identifier
      existing
        | byAddress = Ix.getOne (d.users Ix.@= Office365Id normalized)
        | otherwise = Ix.getOne (d.users Ix.@= EntraOid normalized)
   in case existing of
        Just user
          | user.role == Teacher -> Left "Teacher already exists"
          | otherwise ->
              let user' = user & #role .~ Teacher
                  d' = d & #users %~ Ix.insert user' . Ix.deleteIx user.id
               in Right (d', AudienceAll)
        Nothing ->
          let user =
                User
                  { id = newId
                  , name = if byAddress then T.takeWhile (/= '@') normalized else normalized
                  , role = Teacher
                  , office365Id = Office365Id (if byAddress then normalized else "")
                  , entraOid = if byAddress then Nothing else Just (EntraOid normalized)
                  }
              d' = d & #users %~ Ix.insert user
           in Right (d', AudienceAll)
handleMigrationCommand (BindEntraOid userId oidText) d =
  let oid = EntraOid (T.toLower oidText)
   in case Ix.getOne (d.users Ix.@= userId) of
        Nothing -> Left "BindEntraOid: user not found"
        Just user -> case user.entraOid of
          Just existing
            | existing == oid -> Right (d, AudienceAll)
            | otherwise -> Left "BindEntraOid: user is already bound to a different Entra oid"
          Nothing ->
            let user' = user & #entraOid .~ Just oid
                d' = d & #users %~ Ix.insert user' . Ix.deleteIx user.id
             in Right (d', AudienceAll)
handleMigrationCommand (CompleteUserIdentity userId upn displayName) d =
  case Ix.getOne (d.users Ix.@= userId) of
    Nothing -> Left "CompleteUserIdentity: user not found"
    Just user ->
      let stubName = user.name == "" || Just (EntraOid (T.toLower user.name)) == user.entraOid
          emptyAddress = user.office365Id == Office365Id ""
          user' =
            user
              & #office365Id .~ (if emptyAddress then Office365Id (T.toLower upn) else user.office365Id)
              & #name .~ (if stubName then displayName else user.name)
       in if user' == user
            then Right (d, AudienceAll)
            else Right (d & #users %~ Ix.insert user' . Ix.deleteIx user.id, AudienceAll)
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
   in Right (d', AudienceAll)
