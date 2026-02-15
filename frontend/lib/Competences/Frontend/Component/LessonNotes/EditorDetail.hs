module Competences.Frontend.Component.LessonNotes.EditorDetail
  ( editorDetailView
  )
where

import Competences.Command (LessonNotesCommand (..))
import Competences.Command qualified as Cmd
import Competences.Command.Common qualified as EC
import Competences.Command.LessonNotes (LessonNotesPatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , LessonNotes (..)
  , Lock (..)
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.Common (entityPatchLens, entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.LessonSelector (lessonEditorField)
import Competences.Frontend.Component.Selector.MultiSelectItemSelector (multiSelectItemSelectorComponent, multiSelectItemViewerComponent)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Component (componentA)
import Data.Map.Strict qualified as Map
import Miso qualified as M
import Optics.Core ((&), (?~), (^.))

-- | Detail view for editing a lesson notes entry
editorDetailView
  :: SyncContext
  -> LessonNotes
  -> M.View (SD.Model LessonNotes mode) (SD.Action mode)
editorDetailView r ln =
  componentA
    ("lesson-notes-editor-" <> M.ms (show ln.id))
    []
    (TE.editorComponent lnEditor r)
  where
    lnEditable =
      TE.editable
        ( \d ->
            fmap
              (\ln' -> (ln', (d ^. #locks) Map.!? LessonNotesLock ln'.id))
              (Ix.getOne $ d.lessonNotes Ix.@= ln.id)
        )
        & (#modify ?~ (\ln' modify -> Cmd.LessonNotes $ OnLessonNotes (EC.Modify ln'.id modify)))
        & (#delete ?~ (\ln' -> Cmd.LessonNotes $ OnLessonNotes (EC.Delete ln'.id)))

    lnEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblLessonNotesEntries)
            id
        )
        lnEditable
        `TE.addNamedField` ( C.translate' C.LblLessonNotesDate
                           , TE.dayEditorField #date #date
                           )
        `TE.addNamedField` ( C.translate' C.LblLessonNotesTitle
                           , TE.textEditorField #title #title
                           )
        `TE.addNamedField` ( C.translate' C.LblLesson
                           , lessonEditorField r ("lesson-notes-editor-" <> M.ms (show ln.id) <> "-lesson")
                               (entityPatchTransformedLens #lessonId #lessonId id id)
                           )
        `TE.addNamedField` ( C.translate' C.LblLessonNotesItems
                           , itemsEditorField r
                           )

-- | Editor field for the items list using multi-select item selector
-- Viewer: read-only list of resource/task names
-- Editor: two comboboxes + reorderable list with action buttons
itemsEditorField
  :: SyncContext
  -> EditorField LessonNotes LessonNotesPatch f
itemsEditorField r =
  selectorEditorFieldWithViewer
    "lesson-notes-items"
    (entityPatchLens #items #items)
    (\ln stl -> multiSelectItemViewerComponent r ln.items stl)
    (\ln stl -> multiSelectItemSelectorComponent r ln.items stl)
