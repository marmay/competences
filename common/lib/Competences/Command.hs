module Competences.Command
  ( Command (..)
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
  )
where

import Competences.Command.Assignments (AssignmentPatch (..), AssignmentsCommand (..), handleAssignmentsCommand)
import Competences.Command.Common (AffectedUsers (..), EntityCommand (..), ModifyCommand (..), UpdateResult)
import Competences.Command.CompetenceAssessments (CompetenceAssessmentPatch (..), CompetenceAssessmentsCommand (..), handleCompetenceAssessmentsCommand)
import Competences.Command.CompetenceGridGrades (CompetenceGridGradePatch (..), CompetenceGridGradesCommand (..), handleCompetenceGridGradesCommand)
import Competences.Command.Competences (CompetenceGridPatch (..), CompetencePatch (..), CompetencesCommand (..), handleCompetencesCommand)
import Competences.Command.Evidences (EvidencesCommand (..), EvidencePatch (..), handleEvidencesCommand)
import Competences.Command.Tasks (TasksCommand (..), TaskPatch (..), TaskGroupPatch (..), SubTaskPatch (..), handleTasksCommand)
import Competences.Command.Users (UsersCommand (..), UserPatch (..), handleUsersCommand)
import Competences.Document (Document (..), User (..))
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.IxSet.Typed qualified as Ix
import GHC.Generics (Generic)
import Optics.Core ((^.))

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
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance FromJSON Command

instance ToJSON Command

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
