-- | Standalone edit button for lesson notes.
module Competences.Frontend.Component.LessonNotes.EditButton
  ( lessonNotesEditButton
  )
where

import Competences.Command qualified as Cmd
import Competences.Command (EntityCommand (..), ModifyCommand (..))
import Competences.Command.LessonNotes (LessonNotesCommand (..))
import Competences.Document (LessonNotes (..), Lock (..))
import Competences.Frontend.Component.LockButton (LockButtonConfig (..), lockButtonComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Miso qualified as M
import Miso.String (ms)

lessonNotesEditButton :: SyncContext -> LessonNotes -> M.View m a
lessonNotesEditButton r ln =
  inlineComponent
    ("ln-edit-btn-" <> ms (show ln.id))
    ( lockButtonComponent
        r
        ( LockButtonConfig
            (LessonNotesLock ln.id)
            (Cmd.LessonNotes (OnLessonNotes (Modify ln.id Lock)))
            Button.IconOnlyS
        )
    )
