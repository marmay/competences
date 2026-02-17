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
  , module Competences.Command.Solutions
  , module Competences.Command.Resources
  , module Competences.Command.MesoPlans
  , module Competences.Command.Lessons
  , module Competences.Command.LessonNotes
  , module Competences.Command.ParticipationRecords
  )
where

import Competences.Command.Assignments (AssignmentPatch (..), AssignmentsCommand (..), handleAssignmentsCommand)
import Competences.Command.Resources (ResourcePatch (..), ResourcesCommand (..), handleResourcesCommand)
import Competences.Command.Common (AffectedUsers (..), EntityCommand (..), ModifyCommand (..), UpdateResult)
import Competences.Command.Solutions (SolutionPatch (..), SolutionsCommand (..), handleSolutionsCommand)
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
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import GHC.Generics (Generic)
import Optics.Core ((&), (.~), (%~), (^.))

-- | Migration commands for schema evolution
data MigrationCommand
  = UpdateLessonAssignments ![(LessonId, [AssignmentId])]
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
      allUserIds = map (.id) $ Ix.toList $ doc' ^. #users
   in Right (doc', AffectedUsers allUserIds)
