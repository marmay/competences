-- | Task-with-solutions convenience type.
--
-- Used by views that need pre-assembled task + solutions data.
-- Note: @taskContent@ and @taskPurpose@ duplicate fields on @Task@
-- (kept for backward compatibility with projection code).
module Competences.Frontend.Component.TaskResource
  ( TaskWithSolutions (..)
  )
where

import Competences.Document (Solution, Task)
import Competences.Document.Task (TaskPurpose)
import Competences.TaskContent.RichContent (RichContent)
import GHC.Generics (Generic)

-- | A task with its pre-computed content and solutions.
data TaskWithSolutions = TaskWithSolutions
  { task :: !Task
  , taskContent :: !(Maybe RichContent)
  , taskPurpose :: !TaskPurpose
  , solutions :: ![Solution]
  }
  deriving (Eq, Generic, Show)
