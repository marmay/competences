-- | Per-task resources component for the Assignment Viewer.
--
-- Given a task (by 'TaskId'), discovers related learning materials
-- (resources and other tasks with Complete solutions) based on
-- competence-level overlap, groups them by lesson notes, and renders
-- them inside the task's disclosure body.
--
-- Delegates to the shared 'ResourceLookup' module for discovery and
-- 'ResourceLookup.View' for rendering.
module Competences.Frontend.Component.Assignment.TaskResources
  ( taskResourcesComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskId
  , getTaskAttributes
  )
import Competences.Frontend.Component.ResourceLookup (GroupedResources (..), findGroupedResources)
import Competences.Frontend.Component.ResourceLookup.View
  ( GroupedResourcesAction
  , GroupedResourcesModel
  , groupedResourcesComponent
  )
import Competences.Frontend.SyncContext (SyncContext (..))
import Miso qualified as M

-- | Create a task resources component that discovers and displays
-- related materials for a given task.
--
-- Thin wrapper around 'groupedResourcesComponent' with a projection
-- that extracts the relevant competence levels for the given task.
taskResourcesComponent :: SyncContext -> TaskId -> M.Component p GroupedResourcesModel GroupedResourcesAction
taskResourcesComponent r taskId =
  groupedResourcesComponent r (computeGroupedResources taskId)

-- | Projection function: given a 'TaskId', extract grouped resources from a 'Document'.
computeGroupedResources :: TaskId -> Document -> GroupedResources
computeGroupedResources taskId doc =
  case Ix.getOne (doc.tasks Ix.@= taskId) of
    Nothing -> GroupedResources [] [] []
    Just task ->
      let attrs = getTaskAttributes doc.taskGroups task
          compLevels = attrs.primary <> attrs.secondary
       in findGroupedResources doc compLevels
