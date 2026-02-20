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
import Competences.Frontend.Component.ResourceLookup.View (groupedResourcesComponent)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , subscribeDocument
  )
import Competences.Frontend.View.Component (component)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~))

-- ============================================================================
-- Types
-- ============================================================================

-- | Model for the task resources component.
-- Stores the computed grouped resources and delegates rendering to the shared component.
data TaskResourcesModel = TaskResourcesModel
  { groupedResources :: !GroupedResources
  }
  deriving (Eq, Generic, Show)

-- | Actions for the task resources component.
data Action
  = UpdateResources !DocumentChange
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create a task resources component that discovers and displays
-- related materials for a given task.
taskResourcesComponent :: SyncContext -> TaskId -> M.Component p TaskResourcesModel Action
taskResourcesComponent r taskId =
  (M.component initModel update view')
    { M.subs = [subscribeDocument r UpdateResources]
    }
  where
    initModel :: TaskResourcesModel
    initModel =
      TaskResourcesModel
        { groupedResources = GroupedResources [] [] []
        }

    update (UpdateResources docChange) =
      M.modify $ \m ->
        let gr = computeGroupedResources taskId docChange.document
         in m & #groupedResources .~ gr

    view' :: TaskResourcesModel -> M.View TaskResourcesModel Action
    view' m =
      component
        ("task-resources-" <> M.ms (show taskId))
        (groupedResourcesComponent r m.groupedResources)

-- ============================================================================
-- Computation
-- ============================================================================

-- | Compute grouped resources for a specific task by extracting its
-- competence levels and delegating to the shared lookup.
computeGroupedResources :: TaskId -> Document -> GroupedResources
computeGroupedResources taskId doc =
  case Ix.getOne (doc.tasks Ix.@= taskId) of
    Nothing -> GroupedResources [] [] []
    Just task ->
      let attrs = getTaskAttributes doc.taskGroups task
          compLevels = attrs.primary <> attrs.secondary
       in findGroupedResources doc compLevels
