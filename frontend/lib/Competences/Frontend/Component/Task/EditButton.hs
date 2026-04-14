-- | Standalone edit buttons for tasks and solutions.
-- Polymorphic — can be used in any parent component's view.
module Competences.Frontend.Component.Task.EditButton
  ( taskEditButton
  , solutionEditButton
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), SolutionsCommand (..), TasksCommand (..))
import Competences.Document (Lock (..), Solution (..), Task (..))
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Miso qualified as M
import Miso.String (ms)

-- | LockButton for editing a task. Opens the pin editor for the task.
taskEditButton :: SyncContext -> EntityOrigin -> Task -> M.View m a
taskEditButton r origin task =
  let wrap = wrapForOrigin origin
   in inlineComponent
        ("task-edit-btn-" <> ms (show task.id))
        (lockButtonComponent r
          (LockButtonConfig (TaskLock task.id) (wrap (Tasks (OnTasks (Modify task.id Lock)))) Button.IconOnlyS))

-- | LockButton for editing a solution. Opens the pin editor.
solutionEditButton :: SyncContext -> Solution -> M.View m a
solutionEditButton r sol =
  inlineComponent
    ("sol-edit-btn-" <> ms (show sol.id))
    (lockButtonComponent r
      (LockButtonConfig (SolutionLock sol.id) (Solutions (OnSolutions (Modify sol.id Lock))) Button.IconOnlyS))
