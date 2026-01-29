-- | Task queries on the Document.
-- Provides reusable lookups for tasks by ID, sorted listings, and multi-ID retrieval.
module Competences.Query.Task
  ( getTask
  , getTaskGroup
  , allTasksSorted
  , tasksByIds
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task, TaskGroup, TaskGroupId, TaskId, TaskIxs)
import Competences.Document.Task (TaskIdentifier)
import Data.Proxy (Proxy (..))

-- | Lookup a task by its primary key.
getTask :: Document -> TaskId -> Maybe Task
getTask doc taskId = Ix.getOne $ doc.tasks Ix.@= taskId

-- | Lookup a task group by its primary key.
getTaskGroup :: Document -> TaskGroupId -> Maybe TaskGroup
getTaskGroup doc groupId = Ix.getOne $ doc.taskGroups Ix.@= groupId

-- | All tasks, sorted by TaskIdentifier.
allTasksSorted :: Document -> [Task]
allTasksSorted doc = Ix.toAscList (Proxy @TaskIdentifier) doc.tasks

-- | Tasks matching a list of IDs (as IxSet for further filtering/sorting).
tasksByIds :: Document -> [TaskId] -> Ix.IxSet TaskIxs Task
tasksByIds doc taskIds = doc.tasks Ix.@+ taskIds
