module Competences.Frontend.Component.TaskResource
  ( TaskResourceList (..)
  , DisplayMode (..)
  , TaskWithSolutions (..)
  , Action (..)
  , initialState
  , taskResourceListView
  , updateTaskResourceList
  )
where

import Competences.Document (Solution (..), Task (..))
import Competences.Document.Solution (SolutionId, SolutionType (..))
import Competences.Document.Task (TaskId, TaskIdentifier (..), TaskPurpose (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.TaskStatus (TaskCompletionStatus (..))
import Competences.TaskContent.RichContent (RichContent)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text qualified as T
import Data.Set qualified as Set
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
  , taskContent :: !(Maybe RichContent)  -- ^ Pre-computed from getTaskContent
  , taskPurpose :: !TaskPurpose   -- ^ Pre-computed from getTaskAttributes
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
taskResourceListView
  :: Bool  -- ^ Show purpose badge (Practice/Assessment)
  -> (TaskId -> M.View model a)  -- ^ Per-task extra view (e.g., status indicator); use @const V.empty@ for none
  -> Map TaskId TaskCompletionStatus  -- ^ Task statuses for header tinting
  -> [TaskWithSolutions]
  -> TaskResourceList
  -> (Action -> a)  -- ^ Lift action to parent action type
  -> M.View model a
taskResourceListView showPurposeBadge taskExtra statuses tasks state liftAction =
  if null tasks
    then
      MH.div_
        [class_ "text-muted-foreground text-sm py-4 text-center"]
        [M.text $ C.translate' C.LblNoTasksAvailable]
    else
      MH.div_
        [class_ "space-y-2"]
        (map (viewTask showPurposeBadge taskExtra statuses state liftAction) tasks)

-- | View a single task with its solutions
viewTask :: Bool -> (TaskId -> M.View model a) -> Map TaskId TaskCompletionStatus -> TaskResourceList -> (Action -> a) -> TaskWithSolutions -> M.View model a
viewTask showPurposeBadge taskExtra statuses state liftAction tws =
  let isExpanded = Set.member tws.task.id state.expandedTasks
      TaskIdentifier identifier = tws.task.identifier
      hasContent = case tws.taskContent of
        Nothing -> False
        Just c -> c /= mempty
      hasSolutions = not (null tws.solutions)
      isExpandable = hasContent || hasSolutions
      headerBg = taskStatusHeaderBg (Map.lookup tws.task.id statuses)
      titleLeft =
        MH.div_
          [class_ "flex items-center gap-2"]
          [ Icon.icon [] Icon.IcnTask
          , MH.span_ [class_ "font-medium"] [M.text $ M.ms identifier]
          ]
      titleRight =
        MH.div_
          [class_ "flex items-center gap-2"]
          [ taskExtra tws.task.id
          , if showPurposeBadge
              then purposeBadge tws.taskPurpose
              else V.empty
          ]
      titleView =
        MH.div_
          [class_ "flex items-center justify-between flex-1"]
          [titleLeft, titleRight]
      contentView =
        MH.div_
          [class_ "space-y-3"]
          [ case tws.taskContent of
              Nothing -> V.empty
              Just content ->
                if content == mempty
                  then V.empty
                  else
                    MH.div_
                      [class_ "prose prose-stone prose-sm max-w-none"]
                      [renderRichText content]
          , if null tws.solutions
              then V.empty
              else viewSolutions state liftAction tws.solutions
          ]
   in if isExpandable
        then Disclosure.collapsibleStyled headerBg isExpanded (liftAction $ ToggleTask tws.task.id) titleView contentView
        else
          MH.div_
            [class_ "border rounded-lg overflow-hidden"]
            [ MH.div_
                [class_ $ "flex items-center justify-between px-3 py-2 " <> headerBg]
                [titleLeft, titleRight]
            ]

-- | View solutions list within a task
viewSolutions :: TaskResourceList -> (Action -> a) -> [Solution] -> M.View model a
viewSolutions state liftAction sols =
  MH.div_
    [class_ "border-t pt-2 mt-2"]
    [ Typography.small $ C.translate' C.LblSolutions
    , MH.div_
        [class_ "space-y-1 mt-1"]
        (map (viewSolution state liftAction) sols)
    ]

-- | View a single solution
viewSolution :: TaskResourceList -> (Action -> a) -> Solution -> M.View model a
viewSolution state liftAction sol =
  let isExpanded = Set.member sol.id state.expandedSolutions
   in Disclosure.collapsible
        isExpanded
        (liftAction $ ToggleSolution sol.id)
        ( MH.div_
            [class_ "flex items-center gap-2"]
            [Icon.icon [] Icon.IcnSolution, solutionTypeBadge sol.solutionType]
        )
        ( if sol.content == mempty
            then Typography.muted "Kein Inhalt"
            else
              MH.div_
                [class_ "prose prose-stone prose-sm max-w-none"]
                [renderRichText sol.content]
        )

-- ============================================================================
-- Task status header styling
-- ============================================================================

-- | Header background class based on task completion status.
taskStatusHeaderBg :: Maybe TaskCompletionStatus -> T.Text
taskStatusHeaderBg (Just (TaskDone _)) = "bg-green-50"
taskStatusHeaderBg (Just (TaskNotDone _)) = "bg-yellow-50"
taskStatusHeaderBg _ = "bg-muted/50"

-- ============================================================================
-- Badges
-- ============================================================================

purposeBadge :: TaskPurpose -> M.View model a
purposeBadge purpose =
  Badge.render
    (purposeBadgeVariant purpose)
    (C.translate' $ C.LblTaskPurpose purpose)

purposeBadgeVariant :: TaskPurpose -> Badge.BadgeVariant
purposeBadgeVariant Practice = Badge.Secondary
purposeBadgeVariant Assessment = Badge.Primary

solutionTypeBadge :: SolutionType -> M.View model a
solutionTypeBadge st =
  Badge.render (solutionTypeBadgeVariant st) (solutionTypeLabel st)

solutionTypeLabel :: SolutionType -> M.MisoString
solutionTypeLabel = C.translate' . C.LblSolutionType

solutionTypeBadgeVariant :: SolutionType -> Badge.BadgeVariant
solutionTypeBadgeVariant Hint = Badge.Secondary
solutionTypeBadgeVariant Results = Badge.Outline
solutionTypeBadgeVariant Complete = Badge.Primary

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
