module Competences.Frontend.Component.TaskResourceList
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
import Competences.Frontend.Component.TaskContentView (renderTaskContentText)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
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
  , taskContent :: !(Maybe Text)  -- ^ Pre-computed from getTaskContent
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

-- | Create initial state based on display mode
initialState :: DisplayMode -> [TaskWithSolutions] -> TaskResourceList
initialState mode tasks =
  TaskResourceList
    { expandedTasks = case mode of
        TasksCollapsed -> Set.empty
        TasksExpanded -> Set.fromList [t.task.id | t <- tasks]
    , expandedSolutions = Set.empty  -- Solutions always start collapsed
    }

-- ============================================================================
-- View
-- ============================================================================

-- | Render a task resource list
-- This is a pure view function that takes state and returns a view with actions
-- The showPurposeBadge parameter controls whether to display Practice/Assessment badges
-- (typically hidden for students, shown for teachers)
taskResourceListView
  :: Bool  -- ^ Show purpose badge (Practice/Assessment)
  -> [TaskWithSolutions]
  -> TaskResourceList
  -> (Action -> a)  -- ^ Lift action to parent action type
  -> M.View model a
taskResourceListView showPurposeBadge tasks state liftAction =
  if null tasks
    then
      MH.div_
        [class_ "text-muted-foreground text-sm py-4 text-center"]
        [M.text $ C.translate' C.LblNoTasksAvailable]
    else
      MH.div_
        [class_ "space-y-2"]
        (map (viewTask showPurposeBadge state liftAction) tasks)

-- | View a single task with its solutions
viewTask :: Bool -> TaskResourceList -> (Action -> a) -> TaskWithSolutions -> M.View model a
viewTask showPurposeBadge state liftAction tws =
  let isExpanded = Set.member tws.task.id state.expandedTasks
      TaskIdentifier identifier = tws.task.identifier
      -- Check if task has any expandable content
      hasContent = case tws.taskContent of
        Nothing -> False
        Just c -> c /= ""
      hasSolutions = not (null tws.solutions)
      isExpandable = hasContent || hasSolutions
      -- Only allow clicking and show expand icon if expandable
      headerClasses = if isExpandable
        then "flex items-center justify-between px-4 py-3 bg-muted/50 cursor-pointer hover:bg-muted transition-colors"
        else "flex items-center justify-between px-4 py-3 bg-muted/50"
      headerAttrs = if isExpandable
        then [class_ headerClasses, MH.onClick (liftAction $ ToggleTask tws.task.id)]
        else [class_ headerClasses]
   in MH.div_
        [class_ "border rounded-lg overflow-hidden"]
        [ -- Task header (always visible, clickable to expand only if has content)
          MH.div_
            headerAttrs
            [ MH.div_
                [class_ "flex items-center gap-2"]
                [ -- Expand/collapse icon (only if expandable)
                  if isExpandable
                    then V.icon [] (if isExpanded then IcnArrowDown else IcnExpandShrinkArrowRight)
                    else V.empty
                , -- Task icon
                  V.icon [] IcnTask
                , -- Task identifier
                  MH.span_ [class_ "font-medium"] [M.text $ M.ms identifier]
                ]
            , -- Purpose badge (only shown for teachers)
              if showPurposeBadge
                then purposeBadge tws.taskPurpose
                else V.empty
            ]
        , -- Task content and solutions (shown when expanded)
          if isExpanded && isExpandable
            then
              MH.div_
                [class_ "px-4 py-3 border-t space-y-3"]
                [ -- Task content
                  case tws.taskContent of
                    Nothing -> V.empty
                    Just content ->
                      if content == ""
                        then V.empty
                        else
                          MH.div_
                            [class_ "prose prose-stone prose-sm max-w-none"]
                            [renderTaskContentText content]
                , -- Solutions section
                  if null tws.solutions
                    then V.empty
                    else viewSolutions state liftAction tws.solutions
                ]
            else V.empty
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
   in MH.div_
        [class_ "border rounded-lg overflow-hidden bg-background"]
        [ -- Solution header
          MH.div_
            [ class_ "flex items-center gap-2 px-4 py-2 cursor-pointer hover:bg-muted/50 transition-colors"
            , MH.onClick (liftAction $ ToggleSolution sol.id)
            ]
            [ V.icon [] (if isExpanded then IcnArrowDown else IcnExpandShrinkArrowRight)
            , V.icon [] IcnSolution
            , solutionTypeBadge sol.solutionType
            ]
        , -- Solution content (shown when expanded)
          if isExpanded
            then
              MH.div_
                [class_ "px-4 py-3 border-t"]
                [ if sol.content == ""
                    then Typography.muted "Kein Inhalt"
                    else
                      MH.div_
                        [class_ "prose prose-stone prose-sm max-w-none"]
                        [renderTaskContentText sol.content]
                ]
            else V.empty
        ]

-- ============================================================================
-- Badges
-- ============================================================================

purposeBadge :: TaskPurpose -> M.View model a
purposeBadge purpose =
  Badge.badge
    (purposeBadgeVariant purpose)
    (C.translate' $ C.LblTaskPurpose purpose)

purposeBadgeVariant :: TaskPurpose -> Badge.BadgeVariant
purposeBadgeVariant Practice = Badge.BadgeSecondary
purposeBadgeVariant Assessment = Badge.BadgePrimary

solutionTypeBadge :: SolutionType -> M.View model a
solutionTypeBadge st =
  Badge.badge (solutionTypeBadgeVariant st) (solutionTypeLabel st)

solutionTypeLabel :: SolutionType -> M.MisoString
solutionTypeLabel = C.translate' . C.LblSolutionType

solutionTypeBadgeVariant :: SolutionType -> Badge.BadgeVariant
solutionTypeBadgeVariant Hint = Badge.BadgeSecondary
solutionTypeBadgeVariant Results = Badge.BadgeOutline
solutionTypeBadgeVariant Complete = Badge.BadgePrimary

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
