{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.Resource.Modal
-- Description : Modal component for displaying tasks and learning resources
--
-- Shows tasks and learning resources for a specific competence level.
-- Used via the central WindowManager.
module Competences.Frontend.Component.Resource.Modal
  ( resourceModalComponent
  , ResourceModalConfig (..)
  )
where

import Competences.Document (Resource (..), Task (..))
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.TaskResource
  ( DisplayMode (..)
  , TaskResourceList
  , TaskWithSolutions (..)
  , initialState
  , taskResourceListView
  , updateTaskResourceList
  )
import Competences.Frontend.Component.TaskResource qualified as TRL
import Competences.Document.Task (TaskId)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.ResourceList qualified as ResourceList
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.TaskStatus (viewTaskCompletionStatusFromMap)
import Competences.Query.TaskStatus (TaskCompletionStatus, TaskStatusGroup (..), groupByTaskStatus)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration passed when opening the modal
data ResourceModalConfig = ResourceModalConfig
  { tasks :: ![TaskWithSolutions]
  , resources :: ![Resource]
  , showPurposeBadge :: !Bool
  , taskStatuses :: !(Map.Map TaskId TaskCompletionStatus)
  }
  deriving (Eq)

-- ============================================================================
-- Model
-- ============================================================================

-- | View mode for the resource modal
data ResourceViewMode
  = ViewTasks
  | ViewLearningResources
  deriving (Eq, Generic, Show)

-- | Internal model for the component
data Model = Model
  { config :: !ResourceModalConfig
  , taskListState :: !TaskResourceList
  , viewMode :: !ResourceViewMode
  , expandedResources :: !(Set.Set ResourceId)
  , collapsedGroups :: !(Set.Set TaskStatusGroup)
  }
  deriving (Eq, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = TaskListAction !TRL.Action
  | SwitchViewMode !ResourceViewMode
  | ToggleResourceExpanded !ResourceId
  | ToggleStatusGroup !TaskStatusGroup
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

resourceModalComponent :: ResourceModalConfig -> M.Component p Model Action
resourceModalComponent cfg =
  M.component model update view
  where
    -- Determine default view mode based on available content
    defaultMode
      | not (null cfg.tasks) = ViewTasks
      | otherwise = ViewLearningResources

    model =
      Model
        { config = cfg
        , taskListState = initialState TasksCollapsed cfg.taskStatuses cfg.tasks
        , viewMode = defaultMode
        , expandedResources = Set.empty
        , collapsedGroups = Set.empty
        }

    update (TaskListAction action) =
      M.modify $ \m ->
        m {taskListState = updateTaskResourceList action m.taskListState}

    update (SwitchViewMode newMode) =
      M.modify $ \m -> m {viewMode = newMode}

    update (ToggleResourceExpanded resId) =
      M.modify $ \m ->
        let newExpanded =
              if Set.member resId m.expandedResources
                then Set.delete resId m.expandedResources
                else Set.insert resId m.expandedResources
         in m {expandedResources = newExpanded}

    update (ToggleStatusGroup group) =
      M.modify $ \m ->
        let newCollapsed =
              if Set.member group m.collapsedGroups
                then Set.delete group m.collapsedGroups
                else Set.insert group m.collapsedGroups
         in m {collapsedGroups = newCollapsed}

    view :: Model -> M.View Model Action
    view m =
      Layout.scrollContent $ Layout.padL $ Layout.vFlow Layout.gapM
        [ -- Mode switcher
          modeSwitcher m.viewMode (not $ null m.config.tasks) (not $ null m.config.resources)
        , -- Content
          case m.viewMode of
            ViewTasks
              | Map.null m.config.taskStatuses ->
                  -- No focused user: flat list without grouping
                  taskResourceListView m.config.showPurposeBadge (const Layout.empty) m.config.taskStatuses m.config.tasks m.taskListState TaskListAction
              | otherwise ->
                  groupedTasksView m
            ViewLearningResources ->
              ResourceList.resourcesListView m.config.resources m.expandedResources ToggleResourceExpanded
        ]

-- ============================================================================
-- View Helpers
-- ============================================================================

-- | Mode switcher using button group
modeSwitcher :: ResourceViewMode -> Bool -> Bool -> M.View Model Action
modeSwitcher currentMode hasTasks hasResources =
  Button.buttonGroup
    [ modeButton ViewTasks C.LblTasks hasTasks
    , modeButton ViewLearningResources C.LblLearningResources hasResources
    ]
  where
    modeButton mode label hasContent =
      Button.toggleSm (mode == currentMode) $ Button.button label (hasContent, SwitchViewMode mode)

-- ============================================================================
-- Status-grouped task view
-- ============================================================================

-- | Render tasks grouped by completion status
groupedTasksView :: Model -> M.View Model Action
groupedTasksView m =
  let groups = groupByTaskStatus (.task.id) m.config.taskStatuses m.config.tasks
   in if null groups
        then
          MH.div_
            [class_ "text-muted-foreground text-sm py-4 text-center"]
            [M.text $ C.translate' C.LblNoTasksAvailable]
        else MH.div_ [class_ "space-y-3"] (map (viewStatusGroup m) groups)

-- | Render a single status group as a collapsible section
viewStatusGroup :: Model -> (TaskStatusGroup, [TaskWithSolutions]) -> M.View Model Action
viewStatusGroup m (group, tasks) =
  let isExpanded = not $ Set.member group m.collapsedGroups
      title = statusGroupLabel group
      content =
        taskResourceListView
          m.config.showPurposeBadge
          (viewTaskCompletionStatusFromMap m.config.taskStatuses)
          m.config.taskStatuses
          tasks
          m.taskListState
          TaskListAction
   in Disclosure.disclosure (ToggleStatusGroup group) $
        Disclosure.contents (Disclosure.titleText title) isExpanded content []

-- | Translated label for a status group
statusGroupLabel :: TaskStatusGroup -> M.MisoString
statusGroupLabel = C.translate' . C.LblTaskStatusGroup
