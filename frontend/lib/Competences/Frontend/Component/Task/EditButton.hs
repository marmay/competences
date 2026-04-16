-- | Standalone edit button for solutions.
module Competences.Frontend.Component.Task.EditButton
  ( solutionEditButton
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), SolutionsCommand (..))
import Competences.Document (Lock (..), Solution (..))
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Miso qualified as M
import Miso.String (ms)

solutionEditButton :: SyncContext -> Solution -> M.View m a
solutionEditButton r sol =
  inlineComponent
    ("sol-edit-btn-" <> ms (show sol.id))
    (lockButtonComponent r
      (LockButtonConfig (SolutionLock sol.id) (Solutions (OnSolutions (Modify sol.id Lock))) Button.IconOnlyS))
