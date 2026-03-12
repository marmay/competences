-- | Task editor modal for creating/editing tasks inline from the assignment editor.
-- The "Add" button is now native to SearchSelect (via onCreate callback).
-- This module provides the modal that opens when a new task is created.
module Competences.Frontend.Component.Assignment.TaskSearchSelectWithAdd
  ( openTaskEditorModal
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..), TaskType (..))
import Competences.Document.Id (idToText)
import Competences.Document.Task (TaskId, TaskIdentifier (..), defaultTaskAttributes)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.TaskEditor.TaskDetailView (taskDetailView)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , ModalConfig (..)
  , ModalHeight (..)
  , ModalId (..)
  , ModalWidth (..)
  , SyncContext (..)
  , WindowChrome (..)
  , WindowMode
  , closeWindow
  , openFramedModalWith
  , subscribeDocument
  )
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH

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
        { chrome = WindowChrome (C.translate' C.LblEditSelfContainedTask) Icon.IcnTask
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

    -- Stub task for taskDetailView (it uses task.id and task.taskType for routing)
    stubTask = Task
      { id = taskId
      , identifier = TaskIdentifier ""
      , content = Nothing
      , taskType = SelfContained defaultTaskAttributes
      , attachments = []
      }

    view _m =
      MH.div_
        [class_ "p-4"]
        [taskDetailView r origin stubTask]
