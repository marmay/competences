-- | Connected task view component.
--
-- Subscribes to SyncContext and renders a task using View.Task primitives.
-- Determines context (teacher, focused student) to drive display rules:
--
-- * Edit button (LockButton): shown when user is a teacher
-- * Purpose badge: shown when a student is focused
-- * Collapsing: only in 'TaskInAssignment' mode
--
-- Usage:
--
-- @
-- inlineComponent ("task-" <> ms (show taskId))
--   (taskComponent r (TaskConfig taskId Published TaskInDetail))
-- @
module Competences.Frontend.Component.Task
  ( TaskConfig (..)
  , TaskDisplayMode (..)
  , taskComponent
    -- * Task list rendering (polymorphic, for embedding in parent components)
  , taskListView
    -- * Standalone edit button (polymorphic, for use in annotations)
  , taskEditButton
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), SolutionsCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Solution (..), Task (..), User (..), UserRole (..))
import Competences.Document.Solution (SolutionId, mkSolution)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.TaskResource (TaskWithSolutions (..))
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.Component.RichContent (renderRichText, renderRichTextWithFiles)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.SyncDocument (SyncDocumentEnv (..), syncDocumentEnv)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Query.TaskStatus (TaskCompletionStatus)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Task qualified as V
import Competences.Frontend.View.Typography qualified as Typography
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((.~))

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration for a task component instance.
data TaskConfig = TaskConfig
  { taskId :: !TaskId
  , origin :: !EntityOrigin
  , displayMode :: !TaskDisplayMode
  }

-- | How the task is displayed. Controls collapsibility and framing.
data TaskDisplayMode
  = TaskInAssignment
  -- ^ Collapsible disclosure, status-tinted header
  | TaskInDetail
  -- ^ Expanded, no disclosure frame
  | TaskInLessonNotes
  -- ^ Expanded, content-card frame
  deriving (Eq, Show)

-- ============================================================================
-- Model & Actions
-- ============================================================================

data TaskProjection = TaskProjection
  { task :: !(Maybe Task)
  , solutions :: ![Solution]
  , isTeacher :: !Bool
  , hasFocusedStudent :: !Bool
  }
  deriving (Eq, Generic, Show)

data Model = Model
  { projection :: !TaskProjection
  , expanded :: !Bool
  , expandedSolutions :: !(Set SolutionId)
  }
  deriving (Eq, Generic, Show)

data Action
  = ProjectionChanged !(ProjectedChange TaskProjection)
  | ToggleExpanded
  | ToggleSolution !SolutionId
  | AddSolution !TaskId
  | DeleteSolution !SolutionId
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

taskComponent :: SyncContext -> TaskConfig -> M.Component p Model Action
taskComponent r cfg =
  (M.component model update view)
    { M.subs = [subscribeWithProjection r (taskProjection r cfg) ProjectionChanged]
    }
  where
    model = Model
      { projection = TaskProjection Nothing [] False False
      , expanded = cfg.displayMode /= TaskInAssignment
      , expandedSolutions = Set.empty
      }

    update (ProjectionChanged change) =
      M.modify $ #projection .~ change.projection
    update ToggleExpanded =
      M.modify $ \m -> m {expanded = not m.expanded}
    update (ToggleSolution sid) =
      M.modify $ \m -> m
        { expandedSolutions =
            if Set.member sid m.expandedSolutions
              then Set.delete sid m.expandedSolutions
              else Set.insert sid m.expandedSolutions
        }
    update (AddSolution taskId) = M.io_ $ do
      solId <- nextId r
      let connectedUserId = (syncDocumentEnv r).connectedUser.id
          sol = mkSolution solId taskId connectedUserId
      modifySyncDocument r $ Solutions (OnSolutions (CreateAndLock sol))
    update (DeleteSolution solId) =
      M.io_ $ modifySyncDocument r $ Solutions (OnSolutions (Delete solId))

    view m = case m.projection.task of
      Nothing -> Layout.empty
      Just task -> viewTask r cfg m task

-- ============================================================================
-- Projection
-- ============================================================================

taskProjection :: SyncContext -> TaskConfig -> Document -> Maybe User -> TaskProjection
taskProjection r cfg doc mUser =
  let mTask = case cfg.origin of
        Published -> Ix.getOne (doc.tasks Ix.@= cfg.taskId)
        Draft -> Ix.getOne (doc.draftTasks Ix.@= cfg.taskId)
      solutions = Ix.toList (doc.solutions Ix.@= cfg.taskId)
      connectedUser :: User
      connectedUser = (syncDocumentEnv r).connectedUser
      connectedRole = connectedUser.role
      hasFocusedStudent = case mUser of
        Just u -> u.role == Student
        Nothing -> False
   in TaskProjection mTask solutions (connectedRole == Teacher) hasFocusedStudent

-- ============================================================================
-- View
-- ============================================================================

viewTask :: SyncContext -> TaskConfig -> Model -> Task -> M.View Model Action
viewTask r cfg m task =
  let displayName = ms (taskDisplayName task)
      annotations = headerAnnotations r cfg m task
      body = taskBody r cfg m task
   in case cfg.displayMode of
        TaskInAssignment ->
          V.taskDisclosureView Nothing ToggleExpanded displayName annotations m.expanded body
        _ ->
          V.taskOpenView displayName annotations body

-- ============================================================================
-- Header
-- ============================================================================

-- | Context-driven header annotations.
headerAnnotations :: SyncContext -> TaskConfig -> Model -> Task -> [M.View Model Action]
headerAnnotations r cfg m task =
  concat
    [ [V.purposeBadge task.purpose | m.projection.hasFocusedStudent]
    , [V.assessmentStar task.purpose | m.projection.hasFocusedStudent]
    , [editButton r cfg task | m.projection.isTeacher]
    ]

-- ============================================================================
-- Body
-- ============================================================================

-- | Body content as a single div.
taskBody :: SyncContext -> TaskConfig -> Model -> Task -> M.View Model Action
taskBody r cfg m task =
  MH.div_ [class_ "space-y-3"] (bodyParts r cfg m task)

-- | Body content parts (for compositing into card or flat layout).
bodyParts :: SyncContext -> TaskConfig -> Model -> Task -> [M.View Model Action]
bodyParts r cfg m task =
  concat
    [ [taskContentRendered r task | hasContent task]
    , [viewSolutions r cfg m m.projection.solutions | not (null m.projection.solutions)]
    ]

hasContent :: Task -> Bool
hasContent task = case task.content of
  Nothing -> False
  Just c -> c /= mempty

-- ============================================================================
-- Edit button (LockButton)
-- ============================================================================

editButton :: SyncContext -> TaskConfig -> Task -> M.View Model Action
editButton r cfg task = taskEditButton r cfg.origin task

-- | Standalone edit button for a task. Polymorphic — can be used in any
-- parent component's annotations. Opens the pin editor for the task.
taskEditButton :: SyncContext -> EntityOrigin -> Task -> M.View m a
taskEditButton r origin task =
  let wrap = wrapForOrigin origin
   in inlineComponent
        ("task-edit-btn-" <> ms (show task.id))
        (lockButtonComponent r
          (LockButtonConfig (TaskLock task.id) (wrap (Tasks (OnTasks (Modify task.id Lock)))) Button.IconOnlyS))

-- ============================================================================
-- Task content rendering
-- ============================================================================

taskContentRendered :: SyncContext -> Task -> M.View Model Action
taskContentRendered r task = case task.content of
  Nothing -> Layout.empty
  Just content ->
    if content == mempty
      then Layout.empty
      else V.taskContentView (renderRichTextWithFiles r.formulaCache r task.attachments content)

-- ============================================================================
-- Solutions
-- ============================================================================

viewSolutions :: SyncContext -> TaskConfig -> Model -> [Solution] -> M.View Model Action
viewSolutions r cfg m =
  renderSolutionList r m.expandedSolutions m.projection.isTeacher
    ToggleSolution DeleteSolution (AddSolution cfg.taskId)

-- | Render a list of solutions with collapsible disclosures and optional teacher actions.
-- Shared between the task component and 'taskListView'.
renderSolutionList
  :: SyncContext
  -> Set SolutionId
  -> Bool
  -- ^ Is teacher (show edit/delete actions and add button)
  -> (SolutionId -> a)
  -- ^ Toggle expand/collapse
  -> (SolutionId -> a)
  -- ^ Delete action
  -> a
  -- ^ Add solution action
  -> [Solution]
  -> M.View m a
renderSolutionList r expandedSet isTeacher mkToggle mkDelete addAction sols =
  MH.div_ [class_ "space-y-1"]
    ( map (renderOneSol r expandedSet isTeacher mkToggle mkDelete) sols
        <> [addSolButton | isTeacher]
    )
  where
    addSolButton = Button.secondary (Button.ButtonConfig (Button.IconText Icon.IcnAdd (C.translate' C.LblAddSolution)) (Just addAction))

renderOneSol
  :: SyncContext -> Set SolutionId -> Bool
  -> (SolutionId -> a) -> (SolutionId -> a)
  -> Solution -> M.View m a
renderOneSol r expandedSet isTeacher mkToggle mkDelete sol =
  let isExpanded = Set.member sol.id expandedSet
      rendered =
        if sol.content == mempty
          then Typography.muted (C.translate' C.LblNoContent)
          else V.taskContentView (renderRichText r.formulaCache sol.content)
      actions
        | isTeacher =
            [ Disclosure.viewAction (solutionEditButton r sol)
            , Disclosure.destructiveAction Icon.IcnDelete (mkDelete sol.id)
            ]
        | otherwise = []
   in V.solutionView (V.solutionTypeLabel sol.solutionType) isExpanded rendered actions (mkToggle sol.id)

-- | LockButton for editing a solution (opens pin editor).
solutionEditButton :: SyncContext -> Solution -> M.View m a
solutionEditButton r sol =
  inlineComponent
    ("sol-edit-btn-" <> ms (show sol.id))
    (lockButtonComponent r
      (LockButtonConfig (SolutionLock sol.id) (Solutions (OnSolutions (Modify sol.id Lock))) Button.IconOnlyS))

-- ============================================================================
-- Task list rendering (polymorphic, for parent components)
-- ============================================================================

-- | Render a list of tasks with disclosures, solutions, and status tinting.
--
-- This is the shared rendering function for task lists in parent components
-- (assignment viewer, resource modal, etc.). Each parent provides:
--
-- * A status lookup for header tinting
-- * Per-task annotations (badges, status dots)
-- * Per-task extra body content (e.g., related materials)
-- * An action lifter to wrap 'V.TaskViewAction'
taskListView
  :: SyncContext
  -> V.TaskViewState
  -> (TaskId -> Maybe TaskCompletionStatus)
  -- ^ Status lookup (for header tinting)
  -> (TaskWithSolutions -> [M.View m a])
  -- ^ Per-task annotations (right of header)
  -> (TaskId -> [M.View m a])
  -- ^ Per-task extra body content (appended after solutions)
  -> (V.TaskViewAction -> a)
  -- ^ Lift task view actions to parent action type
  -> [TaskWithSolutions]
  -> M.View m a
taskListView _ _ _ _ _ _ [] =
  Layout.centeredPlaceholder (C.translate' C.LblNoTasksAvailable)
taskListView r state statusLookup mkAnnotations mkExtraBody liftAction tasks =
  Layout.vFlow Layout.gapM (map renderOne tasks)
  where
    isTeacher = (syncDocumentEnv r).connectedUser.role == Teacher

    renderOne tws =
      let tid = tws.task.id
          name = ms (taskDisplayName tws.task)
          expanded = Set.member tid state.expandedTasks
          contentPresent = case tws.taskContent of
            Nothing -> False
            Just c -> c /= mempty
          solsPresent = not (null tws.solutions)
          extra = mkExtraBody tid

          parts = concat
            [ [ V.taskContentView (renderRichTextWithFiles r.formulaCache r tws.task.attachments rc)
              | contentPresent
              , Just rc <- [tws.taskContent]
              ]
            , [ renderSolutions tid tws.solutions | solsPresent ]
            , extra
            ]

          mBody = if null parts then Nothing else Just (MH.div_ [class_ "space-y-3"] parts)
       in V.taskItemView (statusLookup tid) (liftAction (V.ToggleTask tid)) name (mkAnnotations tws) expanded mBody

    renderSolutions tid =
      renderSolutionList r state.expandedSolutions isTeacher
        (liftAction . V.ToggleSolution)
        (liftAction . V.DeleteSolution)
        (liftAction (V.AddSolution tid))
