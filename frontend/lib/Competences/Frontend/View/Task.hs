-- | Unified task view primitives.
--
-- Re-exports all View.Task submodules plus shared state management types.
-- Import this module for convenient access to all task view primitives.
module Competences.Frontend.View.Task
  ( -- * State management
    TaskViewState (..)
  , TaskViewAction (..)
  , updateTaskView
  , initialTaskViewState
    -- * Re-exports
  , module Competences.Frontend.View.Task.Badge
  , module Competences.Frontend.View.Task.Detail
  )
where

import Competences.Document.Solution (SolutionId)
import Competences.Document.Task (TaskId)
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Task.Badge
import Competences.Frontend.View.Task.Detail
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Optics.Core ((&), (.~))

-- | Shared state for task view expansion and hold-to-delete.
data TaskViewState = TaskViewState
  { expandedTasks :: !(Set TaskId)
  , expandedSolutions :: !(Set SolutionId)
  , holdDeleteSolution :: !(HoldButton.HoldState SolutionId)
  }
  deriving (Eq, Generic, Show)

-- | Actions for task view state.
data TaskViewAction
  = ToggleTask !TaskId
  | ToggleSolution !SolutionId
  | AddSolution !TaskId
  | HoldDeleteSolution !(HoldButton.HoldAction SolutionId)
  deriving (Eq, Show)

-- | Update task view state. Pure function.
-- Handles expansion and hold state. 'AddSolution' is side-effectful —
-- the parent must dispatch it to 'modifySyncDocument'.
-- 'HoldDeleteSolution' hold completion is also dispatched by the parent.
updateTaskView :: TaskViewAction -> TaskViewState -> TaskViewState
updateTaskView (ToggleTask taskId) s =
  s & #expandedTasks .~ toggle taskId s.expandedTasks
updateTaskView (ToggleSolution solId) s =
  s & #expandedSolutions .~ toggle solId s.expandedSolutions
updateTaskView (AddSolution _) s = s
updateTaskView (HoldDeleteSolution _) s = s -- handled by parent via handleHoldAction'

-- | Initial state with a given set of expanded tasks.
-- Solutions start collapsed.
initialTaskViewState :: [TaskId] -> TaskViewState
initialTaskViewState expanded =
  TaskViewState
    { expandedTasks = Set.fromList expanded
    , expandedSolutions = Set.empty
    , holdDeleteSolution = HoldButton.emptyHoldState
    }

-- ============================================================================
-- Internal
-- ============================================================================

toggle :: (Ord a) => a -> Set a -> Set a
toggle x s
  | Set.member x s = Set.delete x s
  | otherwise = Set.insert x s
