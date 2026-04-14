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

import Competences.Document (Document, Solution (..), Task (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ResourceLookup (GroupedResources)
import Competences.Frontend.Component.ResourceLookup.View (groupedResourcesComponent)
import Competences.Frontend.Component.RichContent (renderRichText, renderRichTextWithFiles)
import Competences.Frontend.Component.TaskResource (TaskWithSolutions (..))
import Competences.Frontend.View.Task qualified as VT
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Document.Task (TaskId, taskDisplayName)
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
                  viewModalTaskList r m m.config.showPurposeBadge (const Layout.empty) m.config.tasks
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
      content = viewModalTaskList r m m.config.showPurposeBadge
                  (viewTaskCompletionStatusFromMap m.config.taskStatuses)
                  tasks
   in Disclosure.disclosure (ToggleStatusGroup group) $
        Disclosure.contents (Disclosure.titleText title) isExpanded content []

-- | Translated label for a status group
statusGroupLabel :: TaskStatusGroup -> M.MisoString
statusGroupLabel = C.translate' . C.LblTaskStatusGroup

-- ============================================================================
-- Task rendering (using View/Task primitives)
-- ============================================================================

-- | Render a task list using View/Task primitives.
viewModalTaskList
  :: SyncContext
  -> Model
  -> Bool
  -- ^ Show purpose badge
  -> (TaskId -> M.View Model Action)
  -- ^ Per-task extra view (e.g., completion status indicator)
  -> [TaskWithSolutions]
  -> M.View Model Action
viewModalTaskList _r _m _showPurpose _taskExtra [] =
  Layout.centeredPlaceholder (C.translate' C.LblNoTasksAvailable)
viewModalTaskList r m showPurpose taskExtra tasks =
  Layout.vFlow Layout.gapM (map (viewModalTask r m showPurpose taskExtra) tasks)

viewModalTask
  :: SyncContext -> Model -> Bool -> (TaskId -> M.View Model Action)
  -> TaskWithSolutions -> M.View Model Action
viewModalTask r m showPurpose taskExtra tws =
  let taskId = tws.task.id
      displayName = M.ms (taskDisplayName tws.task)
      isExpanded = Set.member taskId m.taskListState.expandedTasks
      mPalette = VT.taskStatusPalette (Map.lookup taskId m.config.taskStatuses)
      hasContent = case tws.taskContent of
        Nothing -> False
        Just c -> c /= mempty
      hasSolutions = not (null tws.solutions)

      annotations = concat
        [ [taskExtra taskId]
        , [VT.purposeBadge tws.taskPurpose | showPurpose]
        , [VT.assessmentStar tws.taskPurpose | showPurpose]
        ]

      body = MH.div_ [class_ "space-y-3"] $ concat
        [ [ VT.taskContentView (renderRichTextWithFiles r.formulaCache r tws.task.attachments rc)
          | hasContent, Just rc <- [tws.taskContent]
          ]
        , [ viewModalSolutions r m tws.solutions | hasSolutions ]
        ]
   in if hasContent || hasSolutions
        then VT.taskDisclosureView mPalette (TaskListAction (VT.ToggleTask taskId)) displayName annotations isExpanded body
        else
          MH.div_
            [class_ "border rounded-lg overflow-hidden"]
            [ MH.div_
                [class_ $ "flex items-center justify-between px-3 py-2 " <> VT.taskStatusHeaderBg (Map.lookup taskId m.config.taskStatuses)]
                [ VT.taskHeader displayName
                , Layout.hFlow (Layout.gapS <> Layout.crossCenter) annotations
                ]
            ]

viewModalSolutions :: SyncContext -> Model -> [Solution] -> M.View Model Action
viewModalSolutions r m sols =
  MH.div_ [class_ "space-y-1"] (map (viewModalOneSolution r m) sols)

viewModalOneSolution :: SyncContext -> Model -> Solution -> M.View Model Action
viewModalOneSolution r m sol =
  let isExpanded = Set.member sol.id m.taskListState.expandedSolutions
      rendered =
        if sol.content == mempty
          then Typography.muted "Kein Inhalt"
          else VT.taskContentView (renderRichText r.formulaCache sol.content)
   in VT.solutionView (VT.solutionTypeLabel sol.solutionType) isExpanded rendered (TaskListAction (VT.ToggleSolution sol.id))
