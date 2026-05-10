-- | Task editor modal for creating/editing tasks inline from the assignment editor.
-- The "Add" button is now native to SearchSelect (via onCreate callback).
-- This module provides the modal that opens when a new task is created.
module Competences.Frontend.Component.Assignment.TaskSearchSelectWithAdd
  ( openTaskEditorModal
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), Task (..), User)
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Id (idToText)
import Competences.Document.Task (TaskId, defaultTask)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Task.Detailed qualified as TaskComp
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , ModalConfig (..)
  , ModalHeight (..)
  , ModalId (..)
  , ModalWidth (..)
  , ProjectedChange (..)
  , SyncContext (..)
  , WindowChrome (..)
  , WindowMode
  , closeWindow
  , openFramedModalWith
  , subscribeDocument
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)

-- ============================================================================
-- Task editor modal
-- ============================================================================

data ModalModel = ModalModel
  { taskExists :: !Bool
  }
  deriving (Eq, Generic, Show)

newtype ModalAction
  = ModalDocumentChanged DocumentChange
  deriving (Eq, Show)

-- | Open a modal containing the task detail editor.
-- The modal auto-closes when the task is deleted (via the inner editor's
-- Delete button), so no custom footer buttons are needed.
openTaskEditorModal :: SyncContext -> EntityOrigin -> TaskId -> IO ()
openTaskEditorModal r origin taskId =
  let cfg = ModalConfig
        { chrome = WindowChrome (C.translate' C.LblEditTask) Icon.IcnTask Nothing
        , modalId = ModalId ("new-task-" <> idToText taskId)
        , width = ModalWide
        , height = ModalFull
        , pinnable = Nothing
        }
   in openFramedModalWith r.windowManager cfg (taskEditorModalComponent r origin taskId)

taskEditorModalComponent
  :: SyncContext
  -> EntityOrigin
  -> TaskId
  -> WindowMode
  -> M.Component p ModalModel ModalAction
taskEditorModalComponent r origin taskId wm =
  (M.component model update view)
    { M.subs = [subscribeDocument r ModalDocumentChanged]
    }
  where
    model = ModalModel {taskExists = True}

    update (ModalDocumentChanged dc) = do
      let doc = dc.document
          mTask = case origin of
            Published -> Ix.getOne $ doc.tasks Ix.@= taskId
            Draft -> Ix.getOne $ doc.draftTasks Ix.@= taskId
      case mTask of
        Nothing -> M.io_ $ closeWindow wm
        Just _ -> pure ()

    -- Stub task for taskDetailView (it uses task.id for routing)
    stubTask = defaultTask taskId

    view _m =
      MH.div_
        [class_ "p-4"]
        [taskDetailView r stubTask]

-- ---------------------------------------------------------------------------
-- Task detail view: assignment-refs banner + standard task component
-- ---------------------------------------------------------------------------

-- | Lightweight projection: assignment names referencing a given task
data TaskAssignmentRefs = TaskAssignmentRefs
  { assignmentNames :: ![AssignmentName]
  }
  deriving (Eq, Generic, Show)

taskAssignmentRefsProjection :: TaskId -> Document -> Maybe User -> TaskAssignmentRefs
taskAssignmentRefsProjection taskId doc _mUser =
  let names =
        [ a.name
        | a <- Ix.toList doc.assignments
        , taskId `elem` a.tasks
        ]
   in TaskAssignmentRefs names

assignmentRefsBanner :: SyncContext -> TaskId -> M.Component p TaskAssignmentRefs (ProjectedChange TaskAssignmentRefs)
assignmentRefsBanner r taskId =
  (M.component (TaskAssignmentRefs []) update' view')
    { M.subs = [subscribeWithProjection r (taskAssignmentRefsProjection taskId) id]
    }
  where
    update' change = M.modify $ \_ -> change.projection
    view' m
      | null m.assignmentNames = M.text ""
      | [AssignmentName single] <- m.assignmentNames =
          banner [M.text (C.translate' C.LblUsedInAssignment <> " " <> ms single)]
      | otherwise =
          banner
            [ M.text (C.translate' C.LblUsedInAssignments)
            , MH.ul_
                [class_ "list-disc list-inside mt-1"]
                [MH.li_ [] [M.text (ms n)] | AssignmentName n <- m.assignmentNames]
            ]
    banner content =
      MH.div_
        [class_ "rounded-lg border border-sky-200 bg-sky-50 p-3 text-sm text-sky-800"]
        content

taskDetailView
  :: SyncContext
  -> Task
  -> M.View p a
taskDetailView r task =
  MH.div_
    [class_ "space-y-4"]
    [ inlineComponent
        ("task-assignment-refs-" <> ms (show task.id))
        (assignmentRefsBanner r task.id)
    , inlineComponent
        ("task-detail-" <> ms (show task.id))
        (TaskComp.taskDetailedComponent r (TaskComp.TaskDetailedConfig task.id adminSettings))
    ]
  where
    adminSettings = TaskComp.defaultTaskDetailedSettings {TaskComp.enableGoTo = False, TaskComp.enableDelete = True}
