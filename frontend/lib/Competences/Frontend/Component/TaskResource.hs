module Competences.Frontend.Component.TaskResource
  ( TaskResourceList (..)
  , DisplayMode (..)
  , TaskWithSolutions (..)
  , Action (..)
  , initialState
  , taskResourceListView
  , taskExpandedCard
  , updateTaskResourceList
  )
where

import Competences.Document (Solution (..), Task (..))
import Competences.Document.Solution (SolutionId, SolutionType (..))
import Competences.Document.Task (TaskId, TaskPurpose (..), taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (FormulaCache, renderRichText, renderRichTextWithFiles)
import Competences.Frontend.SyncContext.SyncDocument (SyncContext (..))
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Color (PaletteName)
import Competences.Frontend.View.Color.Status qualified as Status
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus (TaskCompletionStatus (..))
import Competences.TaskContent.RichContent (RichContent)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- Types
-- ============================================================================

-- | Display mode for the task list
data DisplayMode
  = TasksCollapsed  -- ^ Tasks start collapsed (for resource modal)
  | TasksExpanded   -- ^ Tasks start expanded (for assignments)
  deriving (Eq, Show)

-- | A task with its pre-computed content and solutions
data TaskWithSolutions = TaskWithSolutions
  { task :: !Task
  , taskContent :: !(Maybe RichContent)
  , taskPurpose :: !TaskPurpose
  , solutions :: ![Solution]
  }
  deriving (Eq, Generic, Show)

-- | State for the task resource list
data TaskResourceList = TaskResourceList
  { expandedTasks :: !(Set TaskId)
  , expandedSolutions :: !(Set SolutionId)
  }
  deriving (Eq, Generic, Show)

-- | Actions for the task resource list
data Action
  = ToggleTask !TaskId
  | ToggleSolution !SolutionId
  deriving (Eq, Show)

-- ============================================================================
-- Initial State
-- ============================================================================

-- | Create initial state based on display mode.
-- When 'TasksExpanded', tasks with 'TaskDone' status start collapsed.
initialState :: DisplayMode -> Map TaskId TaskCompletionStatus -> [TaskWithSolutions] -> TaskResourceList
initialState mode statuses tasks =
  TaskResourceList
    { expandedTasks = case mode of
        TasksCollapsed -> Set.empty
        TasksExpanded -> Set.fromList
          [ t.task.id
          | t <- tasks
          , not (isDone (Map.lookup t.task.id statuses))
          ]
    , expandedSolutions = Set.empty  -- Solutions always start collapsed
    }
  where
    isDone (Just (TaskDone _)) = True
    isDone _ = False

-- ============================================================================
-- View
-- ============================================================================

-- | Render a task resource list
-- This is a pure view function that takes state and returns a view with actions
-- The showPurposeBadge parameter controls whether to display Practice/Assessment badges
-- (typically hidden for students, shown for teachers)
-- The taskExtra parameter renders optional per-task content (e.g., completion status)
-- The taskStatuses map is used to tint disclosure headers by completion status
-- The extraBody parameter renders optional per-task body content appended after solutions
taskResourceListView
  :: SyncContext
  -> Bool  -- ^ Show purpose badge (Practice/Assessment)
  -> (TaskId -> M.View model a)  -- ^ Per-task extra view (e.g., status indicator); use @const empty@ for none
  -> Map TaskId TaskCompletionStatus  -- ^ Task statuses for header tinting
  -> [TaskWithSolutions]
  -> TaskResourceList
  -> (TaskId -> [M.View model a])  -- ^ Extra body content per task (e.g., related materials)
  -> (Action -> a)  -- ^ Lift action to parent action type
  -> M.View model a
taskResourceListView r showPurposeBadge taskExtra statuses tasks state extraBody liftAction =
  if null tasks
    then Layout.centeredPlaceholder (C.translate' C.LblNoTasksAvailable)
    else
      Layout.vFlow
        Layout.gapM
        (map (viewTask r showPurposeBadge taskExtra statuses state extraBody liftAction) tasks)

-- | View a single task with its solutions
viewTask :: SyncContext -> Bool -> (TaskId -> M.View model a) -> Map TaskId TaskCompletionStatus -> TaskResourceList -> (TaskId -> [M.View model a]) -> (Action -> a) -> TaskWithSolutions -> M.View model a
viewTask r showPurposeBadge taskExtra statuses state extraBody liftAction tws =
  let isExpanded = Set.member tws.task.id state.expandedTasks
      displayName = taskDisplayName tws.task
      hasContent = case tws.taskContent of
        Nothing -> False
        Just c -> c /= mempty
      hasSolutions = not (null tws.solutions)
      extra = extraBody tws.task.id
      isExpandable = hasContent || hasSolutions || not (null extra)
      mPalette = taskStatusPalette (Map.lookup tws.task.id statuses)
      headerBg = taskStatusHeaderBg (Map.lookup tws.task.id statuses)
      titleLeft = Disclosure.titleIconText Icon.IcnTask (M.ms displayName)
      titleRight =
        Layout.hFlow
          (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
          [ taskExtra tws.task.id
          , if showPurposeBadge
              then purposeBadge tws.taskPurpose
              else Layout.empty
          , if tws.taskPurpose == Assessment
              then Icon.icon [class_ "w-4 h-4 text-amber-500"] Icon.IcnStar
              else Layout.empty
          ]
      titleView = Disclosure.titleWithAnnotation titleLeft titleRight
      contentView =
        MH.div_
          [class_ "space-y-3"]
          ( [ case tws.taskContent of
                Nothing -> Layout.empty
                Just content ->
                  if content == mempty
                    then Layout.empty
                    else
                      MH.div_
                        [class_ "prose prose-stone prose-sm max-w-none"]
                        [renderRichTextWithFiles r.formulaCache r tws.task.attachments content]
            , if null tws.solutions
                then Layout.empty
                else viewSolutions r.formulaCache state liftAction tws.solutions
            ]
              <> extra
          )
   in if isExpandable
        then
          Disclosure.maybePaletteDisclosure mPalette (liftAction $ ToggleTask tws.task.id) $
            Disclosure.contents titleView isExpanded contentView []
        else
          MH.div_
            [class_ "border rounded-lg overflow-hidden"]
            [ MH.div_
                [class_ $ "flex items-center justify-between px-3 py-2 " <> headerBg]
                [titleLeft, titleRight]
            ]

-- | View solutions list within a task
viewSolutions :: FormulaCache -> TaskResourceList -> (Action -> a) -> [Solution] -> M.View model a
viewSolutions fc state liftAction sols =
  MH.div_
    [class_ "space-y-1"]
    (map (viewSolution fc state liftAction) sols)

-- | View a single solution
viewSolution :: FormulaCache -> TaskResourceList -> (Action -> a) -> Solution -> M.View model a
viewSolution fc state liftAction sol =
  let isExpanded = Set.member sol.id state.expandedSolutions
      titleView = Disclosure.titleIconText Icon.IcnSolution (solutionTypeLabel sol.solutionType)
      bodyView =
        if sol.content == mempty
          then Typography.muted "Kein Inhalt"
          else
            MH.div_
              [class_ "prose prose-stone prose-sm max-w-none"]
              [renderRichText fc sol.content]
   in Disclosure.innerDisclosure (liftAction $ ToggleSolution sol.id) $
        Disclosure.contents titleView isExpanded bodyView []

-- ============================================================================
-- Always-expanded views (for lesson notes viewer)
-- ============================================================================

-- | Render a task always expanded (no disclosure, no status palette).
-- Shows task content and solutions inline.
taskExpandedCard :: SyncContext -> TaskWithSolutions -> M.View model action
taskExpandedCard r tws =
  Card.contentCard Icon.IcnTask (M.ms $ taskDisplayName tws.task) $
        [ case tws.taskContent of
            Just rc
              | rc /= mempty ->
                  MH.div_
                    [class_ "px-3 pb-3 prose prose-stone prose-sm max-w-none"]
                    [renderRichTextWithFiles r.formulaCache r tws.task.attachments rc]
            _ -> Layout.empty
        , if null tws.solutions
            then Layout.empty
            else
              MH.div_
                [class_ "px-3 pb-3 space-y-3"]
                (map (solutionInlineView r.formulaCache) tws.solutions)
        ]

-- | Render a solution always visible with type label (no disclosure).
solutionInlineView :: FormulaCache -> Solution -> M.View model action
solutionInlineView fc sol =
  Layout.vFlow Layout.gapMicro
    [ Typography.small (solutionTypeLabel sol.solutionType)
    , if sol.content == mempty
        then Layout.empty
        else
          MH.div_
            [class_ "prose prose-stone prose-sm max-w-none"]
            [renderRichText fc sol.content]
    ]

-- ============================================================================
-- Task status styling
-- ============================================================================

-- | Convert task completion status to a color palette.
taskStatusPalette :: Maybe TaskCompletionStatus -> Maybe PaletteName
taskStatusPalette (Just (TaskDone _)) = Just (Status.statusPalette Status.Ok)
taskStatusPalette (Just (TaskNotDone _)) = Just (Status.statusPalette Status.Pending)
taskStatusPalette _ = Nothing

-- | Header background class based on task completion status.
-- Used for non-expandable tasks that don't use Disclosure.
taskStatusHeaderBg :: Maybe TaskCompletionStatus -> Text
taskStatusHeaderBg (Just (TaskDone _)) = "bg-status-ok"
taskStatusHeaderBg (Just (TaskNotDone _)) = "bg-status-pending"
taskStatusHeaderBg _ = "bg-muted/50"

-- ============================================================================
-- Badges
-- ============================================================================

purposeBadge :: TaskPurpose -> M.View model a
purposeBadge purpose =
  Badge.variant
    (purposeBadgeVariant purpose)
    (Badge.badgeLabel $ C.LblTaskPurpose purpose)

purposeBadgeVariant :: TaskPurpose -> Badge.BadgeVariant
purposeBadgeVariant Practice = Badge.Secondary
purposeBadgeVariant Assessment = Badge.Primary

solutionTypeLabel :: SolutionType -> M.MisoString
solutionTypeLabel = C.translate' . C.LblSolutionType

-- ============================================================================
-- State Update
-- ============================================================================

-- | Update the task resource list state
updateTaskResourceList :: Action -> TaskResourceList -> TaskResourceList
updateTaskResourceList (ToggleTask taskId) state =
  state
    & #expandedTasks
      .~ ( if Set.member taskId state.expandedTasks
             then Set.delete taskId state.expandedTasks
             else Set.insert taskId state.expandedTasks
         )
updateTaskResourceList (ToggleSolution solId) state =
  state
    & #expandedSolutions
      .~ ( if Set.member solId state.expandedSolutions
             then Set.delete solId state.expandedSolutions
             else Set.insert solId state.expandedSolutions
         )
