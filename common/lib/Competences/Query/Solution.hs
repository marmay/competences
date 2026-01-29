-- | Solution queries on the Document.
-- Provides reusable lookups for solutions by task or user.
module Competences.Query.Solution
  ( taskSolutions
  , userTaskSolution
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Solution, SolutionIxs, TaskId, UserId)

-- | All solutions for a task (as IxSet for further filtering).
taskSolutions :: Document -> TaskId -> Ix.IxSet SolutionIxs Solution
taskSolutions doc taskId = doc.solutions Ix.@= taskId

-- | Get a specific user's solution for a task, if it exists.
userTaskSolution :: Document -> UserId -> TaskId -> Maybe Solution
userTaskSolution doc userId taskId =
  Ix.getOne $ doc.solutions Ix.@= taskId Ix.@= userId
