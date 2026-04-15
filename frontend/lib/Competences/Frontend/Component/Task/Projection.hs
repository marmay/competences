-- | Denormalized task projection for list views.
--
-- 'taskContent' and 'taskPurpose' duplicate fields on 'Task'. Callers read
-- them via OverloadedRecordDot without importing 'Task(..)'.
module Competences.Frontend.Component.Task.Projection
  ( TaskWithSolutions (..)
  )
where

import Competences.Document (Solution, Task)
import Competences.Document.Task (TaskPurpose)
import Competences.TaskContent.RichContent (RichContent)
import GHC.Generics (Generic)

data TaskWithSolutions = TaskWithSolutions
  { task :: !Task
  , taskContent :: !(Maybe RichContent)
  , taskPurpose :: !TaskPurpose
  , solutions :: ![Solution]
  }
  deriving (Eq, Generic, Show)
