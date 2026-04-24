-- | Task completion status queries.
-- Determines whether a task counts as "done" for a given user.
--
-- Two-phase lookup:
-- 1. Find newest evidence with stored per-task evaluations (via TaskId index)
-- 2. Fallback: find newest evidence with observations for the task's primary
--    competences (via CompetenceLevelId index) and derive status from those.
module Competences.Query.TaskStatus
  ( TaskCompletionStatus (..)
  , EvidenceRef (..)
  , mkEvidenceRef
  , taskCompletionStatus
  , taskCompletionStatuses
  , TaskStatusGroup (..)
  , taskStatusGroups
  , taskStatusGroup
  , groupByTaskStatus
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.ActivityType (ActivityType)
import Competences.Document.Assignment (Assignment (..), AssignmentName)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), EvidenceIxs, Observation (..))
import Competences.Document.Task (Task (..), TaskId, getTaskPrimaryCompetences)
import Competences.Document.User (UserId)
import Competences.Query.Evidence qualified as QEvidence
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.Proxy (Proxy (..))
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
-- A task is 'TaskDone' when all its primary competences have satisfactory
-- observations ('SelfReliant' or 'SelfReliantWithSillyMistakes').
data TaskCompletionStatus
  = TaskDone !EvidenceRef
  | TaskNotDone !EvidenceRef
  | TaskNotEvaluated -- ^ No relevant evidence found for the user
  deriving (Eq, Show)

-- | Compute completion status for a single task for a user.
taskCompletionStatus :: Document -> UserId -> Task -> TaskCompletionStatus
taskCompletionStatus doc userId task =
  let userEvs = doc.evidences Ix.@= userId
   in taskCompletionStatusFromIxSet doc userEvs task

-- | Batch version: compute status for multiple tasks.
-- Fetches user evidences once (as IxSet) and reuses for all tasks.
taskCompletionStatuses :: Document -> UserId -> [Task] -> Map TaskId TaskCompletionStatus
taskCompletionStatuses doc userId tasks =
  let userEvs = doc.evidences Ix.@= userId
   in Map.fromList
        [ (task.id, taskCompletionStatusFromIxSet doc userEvs task)
        | task <- tasks
        ]

-- | Internal: compute status using IxSet index lookups.
--
-- Phase 1: find newest evidence with non-empty per-task evaluations
--          (uses TaskId index on Evidence).
-- Phase 2: find newest evidence with observations for the task's primary
--          competences (uses CompetenceLevelId index on Evidence).
taskCompletionStatusFromIxSet :: Document -> Ix.IxSet EvidenceIxs Evidence -> Task -> TaskCompletionStatus
taskCompletionStatusFromIxSet doc userEvs task =
  -- Only consider evidences that reference this specific task.
  -- Within same (Day, LessonId) groups, order by reliability descending
  -- so that find naturally picks the highest-reliability evidence first.
  let taskUserEvs = userEvs Ix.@= task.id
      taskEvsByDay = Ix.toDescList (Proxy @Day) taskUserEvs
      ordered = concat $ QEvidence.groupByLessonDay taskEvsByDay
   in -- Phase 1: evidence with stored per-task evaluations
      case find hasNonEmptyEvals ordered of
        Just ev ->
          let ref = mkEvidenceRef doc ev
              taskEvals = ev.tasks Map.! task.id
              allDone = all isSatisfactory (Map.elems taskEvals)
           in if allDone then TaskDone ref else TaskNotDone ref
        Nothing ->
          -- Phase 2: derive from competence observations (still filtered by task)
          let primaryComps = getTaskPrimaryCompetences task
           in if null primaryComps
                then TaskNotEvaluated
                else
                  case find (hasAnyCompetenceObs primaryComps) ordered of
                    Nothing -> TaskNotEvaluated
                    Just ev ->
                      let ref = mkEvidenceRef doc ev
                          allDone = all (isCompetenceDone ev) primaryComps
                       in if allDone then TaskDone ref else TaskNotDone ref
  where
    hasNonEmptyEvals e =
      case Map.lookup task.id e.tasks of
        Just evals -> not (Map.null evals)
        Nothing -> False

    hasAnyCompetenceObs comps ev =
      any (\cId -> not $ Ix.null $ ev.observations Ix.@= cId) comps

-- | Whether an ability counts as satisfactory for task completion.
isSatisfactory :: Ability -> Bool
isSatisfactory SelfReliant = True
isSatisfactory SelfReliantWithSillyMistakes = True
isSatisfactory _ = False

-- | Check if a competence level has a satisfactory observation in an evidence.
isCompetenceDone :: Evidence -> CompetenceLevelId -> Bool
isCompetenceDone ev compLevelId =
  any isDone $ Ix.toList (ev.observations Ix.@= compLevelId)
  where
    isDone obs = isSatisfactory obs.ability

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

-- ============================================================================
-- Status grouping
-- ============================================================================

-- | Coarse status group for grouping tasks in UI views.
data TaskStatusGroup = GroupOpen | GroupInProgress | GroupDone
  deriving (Eq, Ord, Show)

-- | All status groups in display order.
taskStatusGroups :: [TaskStatusGroup]
taskStatusGroups = [GroupOpen, GroupInProgress, GroupDone]

-- | Classify a completion status into a coarse group.
taskStatusGroup :: TaskCompletionStatus -> TaskStatusGroup
taskStatusGroup TaskNotEvaluated = GroupOpen
taskStatusGroup (TaskNotDone _) = GroupInProgress
taskStatusGroup (TaskDone _) = GroupDone

-- | Partition items by task status group.
--
-- Groups are returned in fixed order: Open, InProgress, Done.
-- Empty groups are omitted. Items not found in the status map are classified as Open.
groupByTaskStatus :: (a -> TaskId) -> Map TaskId TaskCompletionStatus -> [a] -> [(TaskStatusGroup, [a])]
groupByTaskStatus getKey statuses items =
  let grouped = foldr insertItem Map.empty items
      insertItem item acc =
        let g = maybe GroupOpen taskStatusGroup (Map.lookup (getKey item) statuses)
         in Map.insertWith (++) g [item] acc
   in [(g, xs) | g <- [GroupOpen, GroupInProgress, GroupDone], Just xs <- [Map.lookup g grouped]]
