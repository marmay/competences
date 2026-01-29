-- | Solution queries on the Document.
-- Provides reusable lookups for solutions by task or user.
module Competences.Query.Solution
  ( -- * Single-entity lookup
    getSolution
    -- * Task-scoped queries
  , taskSolutions
  , userTaskSolution
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Solution, SolutionId, SolutionIxs, TaskId, UserId)

-- | Lookup a solution by primary key.
getSolution :: Document -> SolutionId -> Maybe Solution
getSolution doc solutionId = Ix.getOne $ doc.solutions Ix.@= solutionId

-- | All solutions for a task (as IxSet for further filtering).
taskSolutions :: Document -> TaskId -> Ix.IxSet SolutionIxs Solution
taskSolutions doc taskId = doc.solutions Ix.@= taskId

-- | Get a specific user's solution for a task, if it exists.
userTaskSolution :: Document -> UserId -> TaskId -> Maybe Solution
userTaskSolution doc userId taskId =
  Ix.getOne $ doc.solutions Ix.@= taskId Ix.@= userId
