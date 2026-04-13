-- | Task queries on the Document.
-- Provides reusable lookups for tasks by ID, sorted listings, and multi-ID retrieval.
module Competences.Query.Task
  ( getTask
  , getTaskOrDraft
  , allTasksSorted
  , tasksByIds
  )
where

import Control.Applicative ((<|>))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task, TaskId, TaskIxs)
import Competences.Document.Task (TaskIdentifier)
import Data.Proxy (Proxy (..))

-- | Lookup a task by its primary key.
getTask :: Document -> TaskId -> Maybe Task
getTask doc taskId = Ix.getOne $ doc.tasks Ix.@= taskId

-- | Lookup a task by ID, searching both published and draft collections.
getTaskOrDraft :: Document -> TaskId -> Maybe Task
getTaskOrDraft doc taskId =
  Ix.getOne (doc.tasks Ix.@= taskId)
    <|> Ix.getOne (doc.draftTasks Ix.@= taskId)

-- | All tasks, sorted by TaskIdentifier.
allTasksSorted :: Document -> [Task]
allTasksSorted doc = Ix.toAscList (Proxy @TaskIdentifier) doc.tasks

-- | Tasks matching a list of IDs (as IxSet for further filtering/sorting).
tasksByIds :: Document -> [TaskId] -> Ix.IxSet TaskIxs Task
tasksByIds doc taskIds = doc.tasks Ix.@+ taskIds
