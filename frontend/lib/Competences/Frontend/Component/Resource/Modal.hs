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
  , openResourceModal
  )
where

import Competences.Document (Document, Task (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ResourceLookup (GroupedResources)
import Competences.Frontend.Component.ResourceLookup.View (groupedResourcesComponent)
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Task qualified as TaskComp
import Competences.Frontend.Component.TaskResource (TaskWithSolutions (..))
import Competences.Frontend.View.Task qualified as VT
import Competences.Document.Task (TaskId)
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalId (..), ModalHeight (..), ModalWidth (..), WindowChrome (..), inlineComponent, openFramedModal)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.TaskStatus (viewTaskCompletionStatusFromMap)
import Competences.Query.TaskStatus (TaskCompletionStatus, TaskStatusGroup (..), groupByTaskStatus)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH

-- | Open the resource modal as a framed modal.
openResourceModal :: SyncContext -> ResourceModalConfig -> IO ()
openResourceModal r cfg =
  let frameCfg = ModalConfig (WindowChrome (C.translate' C.LblMaterials) Icon.IcnResources Nothing) (ModalId "resources") ModalWide ModalFull Nothing
   in openFramedModal r.windowManager frameCfg (resourceModalComponent r cfg)

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration passed when opening the modal.
--
-- @resourceProjection@ is a function from 'Document' to 'GroupedResources',
-- allowing the inner component to recompute resources on document changes.
data ResourceModalConfig = ResourceModalConfig
  { tasks :: ![TaskWithSolutions]
  , resourceProjection :: !(Document -> GroupedResources)
  , showPurposeBadge :: !Bool
  , taskStatuses :: !(Map.Map TaskId TaskCompletionStatus)
  }

-- | Compares all fields except @resourceProjection@ (a function).
-- The projection is set once at modal creation and never changes.
instance Eq ResourceModalConfig where
  a == b =
    a.tasks == b.tasks
      && a.showPurposeBadge == b.showPurposeBadge
      && a.taskStatuses == b.taskStatuses

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
  , taskListState :: !VT.TaskViewState
  , viewMode :: !ResourceViewMode
  , collapsedGroups :: !(Set.Set TaskStatusGroup)
  }
  deriving (Eq, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = TaskListAction !VT.TaskViewAction
  | SwitchViewMode !ResourceViewMode
  | ToggleStatusGroup !TaskStatusGroup
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

resourceModalComponent :: SyncContext -> ResourceModalConfig -> M.Component p Model Action
resourceModalComponent r cfg =
  M.component model update view
  where
    -- Determine default view mode based on available content
    defaultMode
      | not (null cfg.tasks) = ViewTasks
      | otherwise = ViewLearningResources

    model =
      Model
        { config = cfg
        , taskListState = VT.initialTaskViewState [] -- tasks start collapsed
        , viewMode = defaultMode
        , collapsedGroups = Set.empty
        }

    update (TaskListAction action) =
      M.modify $ \m ->
        m {taskListState = VT.updateTaskView action m.taskListState}

    update (SwitchViewMode newMode) =
      M.modify $ \m -> m {viewMode = newMode}

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
        [ -- Mode switcher (always show both tabs)
          modeSwitcher m.viewMode (not $ null m.config.tasks) True
        , -- Content
          case m.viewMode of
            ViewTasks
              | Map.null m.config.taskStatuses ->
                  -- No focused user: flat list without grouping
                  viewModalTaskList r m (const Layout.empty) m.config.tasks
              | otherwise ->
                  groupedTasksView r m
            ViewLearningResources ->
              inlineComponent
                "resource-modal-learning-resources"
                (groupedResourcesComponent r cfg.resourceProjection)
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
groupedTasksView :: SyncContext -> Model -> M.View Model Action
groupedTasksView r m =
  let groups = groupByTaskStatus (.task.id) m.config.taskStatuses m.config.tasks
   in if null groups
        then
          MH.div_
            [class_ "text-muted-foreground text-sm py-4 text-center"]
            [M.text $ C.translate' C.LblNoTasksAvailable]
        else MH.div_ [class_ "space-y-3"] (map (viewStatusGroup r m) groups)

-- | Render a single status group as a collapsible section
viewStatusGroup :: SyncContext -> Model -> (TaskStatusGroup, [TaskWithSolutions]) -> M.View Model Action
viewStatusGroup r m (group, tasks) =
  let isExpanded = not $ Set.member group m.collapsedGroups
      title = statusGroupLabel group
      content = viewModalTaskList r m (viewTaskCompletionStatusFromMap m.config.taskStatuses) tasks
   in Disclosure.disclosure (ToggleStatusGroup group) $
        Disclosure.contents (Disclosure.titleText title) isExpanded content []

-- | Translated label for a status group
statusGroupLabel :: TaskStatusGroup -> M.MisoString
statusGroupLabel = C.translate' . C.LblTaskStatusGroup

-- ============================================================================
-- Task rendering (delegates to Component.Task.taskListView)
-- ============================================================================

viewModalTaskList
  :: SyncContext
  -> Model
  -> (TaskId -> M.View Model Action)
  -- ^ Per-task extra annotation (e.g., completion status indicator)
  -> [TaskWithSolutions]
  -> M.View Model Action
viewModalTaskList r m taskExtra =
  TaskComp.taskListView
    r
    m.taskListState
    (`Map.lookup` m.config.taskStatuses)
    (modalAnnotations r m taskExtra)
    (const [])
    TaskListAction

modalAnnotations :: SyncContext -> Model -> (TaskId -> M.View Model Action) -> TaskWithSolutions -> [M.View Model Action]
modalAnnotations r m taskExtra tws =
  concat
    [ [taskExtra tws.task.id]
    , [VT.purposeBadge tws.taskPurpose | m.config.showPurposeBadge]
    , [VT.assessmentStar tws.taskPurpose | m.config.showPurposeBadge]
    , [TaskComp.taskEditButton r Published tws.task]
    ]
