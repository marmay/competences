-- | Connected task view component.
--
-- Subscribes to SyncContext and renders a task using View.Task primitives.
-- Determines context (teacher, focused student) to drive display rules.
module Competences.Frontend.Component.Task.Component
  ( TaskConfig (..)
  , TaskDisplayMode (..)
  , taskComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Solution (..), Task (..), User (..), UserRole (..))
import Competences.Document.Solution (SolutionId, mkSolution)
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.Component.Task.EditButton (taskEditButton)
import Competences.Frontend.Component.Task.ListView (renderSolutionList)
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.SyncDocument (SyncDocumentEnv (..), syncDocumentEnv)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Task qualified as V
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

data TaskConfig = TaskConfig
  { taskId :: !TaskId
  , origin :: !EntityOrigin
  , displayMode :: !TaskDisplayMode
  }

data TaskDisplayMode
  = TaskInAssignment
  | TaskInDetail
  | TaskInLessonNotes
  | TaskPreview
  -- ^ Collapsible, content only (no solutions, no edit button)
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
      , expanded = cfg.displayMode `notElem` [TaskInAssignment, TaskPreview]
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
        TaskPreview ->
          V.taskDisclosureView Nothing ToggleExpanded displayName [] m.expanded body
        _ ->
          V.taskOpenView displayName annotations body

headerAnnotations :: SyncContext -> TaskConfig -> Model -> Task -> [M.View Model Action]
headerAnnotations r cfg m task =
  concat
    [ [V.purposeBadge task.purpose | m.projection.hasFocusedStudent]
    , [V.assessmentStar task.purpose | m.projection.hasFocusedStudent]
    , [taskEditButton r cfg.origin task | m.projection.isTeacher]
    ]

taskBody :: SyncContext -> TaskConfig -> Model -> Task -> M.View Model Action
taskBody r cfg m task =
  MH.div_ [class_ "space-y-3"] $
    concat
      [ [taskContentRendered r task | hasContent task]
      , [ viewSolutions r cfg m m.projection.solutions
        | cfg.displayMode /= TaskPreview
        , not (null m.projection.solutions)
        ]
      ]

hasContent :: Task -> Bool
hasContent task = case task.content of
  Nothing -> False
  Just c -> c /= mempty

taskContentRendered :: SyncContext -> Task -> M.View Model Action
taskContentRendered r task = case task.content of
  Nothing -> Layout.empty
  Just content ->
    if content == mempty
      then Layout.empty
      else V.taskContentView (renderRichTextWithFiles r.formulaCache r task.attachments content)

viewSolutions :: SyncContext -> TaskConfig -> Model -> [Solution] -> M.View Model Action
viewSolutions r cfg m =
  renderSolutionList r m.expandedSolutions m.projection.isTeacher
    ToggleSolution DeleteSolution (AddSolution cfg.taskId)
