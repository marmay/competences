-- | Task completion status queries.
-- Determines whether a task counts as "done" for a given user by checking
-- the newest evidence containing that task against the task's primary competences.
module Competences.Query.TaskStatus
  ( TaskCompletionStatus (..)
  , EvidenceRef (..)
  , taskCompletionStatus
  , taskCompletionStatuses
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.ActivityType (ActivityType)
import Competences.Document.Assignment (Assignment (..), AssignmentName)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.Task (Task (..), TaskId, getTaskPrimaryCompetences)
import Competences.Document.User (UserId)
import Competences.Query.Evidence qualified as QEvidence
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time (Day)

-- | Reference to the evidence that determined the status.
data EvidenceRef = EvidenceRef
  { assignmentName :: !(Maybe AssignmentName)
  , activityType :: !ActivityType
  , date :: !Day
  }
  deriving (Eq, Show)

-- | Per-task completion status for a user.
--
-- A task is 'TaskDone' when all its primary competences have observations
-- at 'SelfReliant' or 'SelfReliantWithSillyMistakes' in the newest evidence
-- containing that task.
data TaskCompletionStatus
  = TaskDone !EvidenceRef
  | TaskNotDone !EvidenceRef
  | TaskNotEvaluated -- ^ No evidence containing this task found for the user
  deriving (Eq, Show)

-- | Compute completion status for a single task for a user.
--
-- Finds the newest evidence containing this task, then checks whether
-- all primary competences have observations at 'SelfReliant' or
-- 'SelfReliantWithSillyMistakes'.
taskCompletionStatus :: Document -> UserId -> Task -> TaskCompletionStatus
taskCompletionStatus doc userId task =
  let userEvs = QEvidence.userEvidencesDesc doc userId
   in taskCompletionStatusFromEvs doc userEvs task

-- | Batch version: compute status for multiple tasks.
-- Fetches user evidences once and reuses for all tasks.
taskCompletionStatuses :: Document -> UserId -> [Task] -> Map TaskId TaskCompletionStatus
taskCompletionStatuses doc userId tasks =
  let userEvs = QEvidence.userEvidencesDesc doc userId
   in Map.fromList
        [ (task.id, taskCompletionStatusFromEvs doc userEvs task)
        | task <- tasks
        ]

-- | Internal: compute status given pre-fetched evidence list.
taskCompletionStatusFromEvs :: Document -> [Evidence] -> Task -> TaskCompletionStatus
taskCompletionStatusFromEvs doc userEvs task =
  let primaryComps = getTaskPrimaryCompetences doc.taskGroups task
   in if null primaryComps
        then TaskNotEvaluated
        else case find (\e -> task.id `elem` e.tasks) userEvs of
          Nothing -> TaskNotEvaluated
          Just ev ->
            let ref = mkEvidenceRef doc ev
                allDone = all (isCompetenceDone ev) primaryComps
             in if allDone then TaskDone ref else TaskNotDone ref

-- | Check if a competence level has a satisfactory observation in an evidence.
isCompetenceDone :: Evidence -> CompetenceLevelId -> Bool
isCompetenceDone ev compLevelId =
  any isDone $ Ix.toList (ev.observations Ix.@= compLevelId)
  where
    isDone obs = obs.ability == SelfReliant || obs.ability == SelfReliantWithSillyMistakes

-- | Build evidence reference for display.
mkEvidenceRef :: Document -> Evidence -> EvidenceRef
mkEvidenceRef doc ev =
  let asmtName = case ev.assignmentId of
        Nothing -> Nothing
        Just aid ->
          (.name) <$> Ix.getOne (doc.assignments Ix.@= aid)
   in EvidenceRef
        { assignmentName = asmtName
        , activityType = ev.activityType
        , date = ev.date
        }
