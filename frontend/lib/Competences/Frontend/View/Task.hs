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
  , module Competences.Frontend.View.Task.Selector
  )
where

import Competences.Document.Solution (SolutionId)
import Competences.Document.Task (TaskId)
import Competences.Frontend.View.Task.Badge
import Competences.Frontend.View.Task.Detail
import Competences.Frontend.View.Task.Selector
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Optics.Core ((&), (.~))

-- | Shared state for task view expansion.
-- Tracks which tasks and solutions are expanded.
-- Replaces TaskResource.TaskResourceList.
data TaskViewState = TaskViewState
  { expandedTasks :: !(Set TaskId)
  , expandedSolutions :: !(Set SolutionId)
  }
  deriving (Eq, Generic, Show)

-- | Actions for task view state.
data TaskViewAction
  = ToggleTask !TaskId
  | ToggleSolution !SolutionId
  deriving (Eq, Show)

-- | Update task view state. Pure function.
updateTaskView :: TaskViewAction -> TaskViewState -> TaskViewState
updateTaskView (ToggleTask taskId) s =
  s & #expandedTasks .~ toggle taskId s.expandedTasks
updateTaskView (ToggleSolution solId) s =
  s & #expandedSolutions .~ toggle solId s.expandedSolutions

-- | Initial state with a given set of expanded tasks.
-- Solutions always start collapsed.
initialTaskViewState :: [TaskId] -> TaskViewState
initialTaskViewState expanded =
  TaskViewState
    { expandedTasks = Set.fromList expanded
    , expandedSolutions = Set.empty
    }

-- ============================================================================
-- Internal
-- ============================================================================

toggle :: (Ord a) => a -> Set a -> Set a
toggle x s
  | Set.member x s = Set.delete x s
  | otherwise = Set.insert x s
