-- | Lesson-notes editor mounted in a pinned dialog.
module Competences.Frontend.Component.LessonNotes.PinEditor
  ( lessonNotesPinEditor
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
  , Resource (..)
  , Task (..)
  , lockOwner
  )
import Competences.Document.LessonNotes (LessonNoteItem (..), LessonNotesId)
import Competences.Document.Resource (ResourceIdentifier (..))
import Competences.Document.Task (taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor (Editable (..), editable, editor, addNamedField, editorComponent, dayEditorField, textEditorField)
import Competences.Frontend.Component.Editor.FormView (editorFormView')
import Competences.Frontend.Component.Editor.EditorField (EditorField)
import Competences.Frontend.Component.Editor.Types (Action, Model)
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.LessonSelector (lessonEditorField)
import Competences.Frontend.Component.Selector.SearchSelect (SearchSelectConfig (..), SelectionOrder (..), TagLayout (..), keywordsFilter)
import Competences.Frontend.Component.Selector.SearchSelectEditorField (searchSelectEditorField)
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager
  ( PinId
  , WindowMode
  , justLens
  , pinSaveStateLens
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Query.Resource qualified as QResource
import Competences.Query.Task qualified as QTask
import Data.Default (Default (..))
import Miso qualified as M
import Miso.String (ms)
import Optics.Core ((&), (?~))
import Optics.Core qualified as O

-- | Lesson-notes pin editor factory.
lessonNotesPinEditor
  :: SyncContext -> LessonNotesId -> PinId
  -> WindowMode -> Maybe (Model LessonNotes LessonNotesPatch Maybe)
  -> M.Component WM.Model (Model LessonNotes LessonNotesPatch Maybe) (Action LessonNotes LessonNotesPatch)
lessonNotesPinEditor r lnId pid _mode mSaved =
  (editorComponent lnEditor r mSaved def)
    { M.bindings =
        [ O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL justLens
        ]
    }
  where
    editorId = "lesson-notes-pin-editor-" <> ms (show lnId)

    lnEditable :: Editable Maybe LessonNotes LessonNotesPatch
    lnEditable =
      editable
        ( \d ->
            fmap
              (\ln -> (ln, lockOwner (LessonNotesLock ln.id) d))
              (Ix.getOne $ d.lessonNotes Ix.@= lnId)
        )
        & (#modify ?~ (\ln modify -> Cmd.LessonNotes $ OnLessonNotes (EC.Modify ln.id modify)))

    lnEditor =
      editor
        (editorFormView' (C.translate' C.LblLessonNotesEntries) id)
        lnEditable
        `addNamedField` ( C.translate' C.LblLessonNotesDate
                        , dayEditorField #date #date
                        )
        `addNamedField` ( C.translate' C.LblLessonNotesTitle
                        , textEditorField #title #title
                        )
        `addNamedField` ( C.translate' C.LblLesson
                        , lessonEditorField r (editorId <> "-lesson")
                            (entityPatchTransformedLens #lessonId #lessonId id id)
                        )
        `addNamedField` ( C.translate' C.LblLessonNotesItems
                        , itemsEditorField r
                        )

-- ============================================================================
-- Items field (migrated from former LessonNotes/EditorDetail.hs)
-- ============================================================================

data NoteItem = NoteResource !Resource | NoteTask !Task
  deriving (Eq, Show)

noteItemSearchConfig :: SearchSelectConfig NoteItem LessonNoteItem
noteItemSearchConfig =
  SearchSelectConfig
    { projectItems = \doc ->
        map NoteResource (QResource.allResources doc)
          <> map NoteTask (QTask.allTasksSorted doc)
    , itemId = \case
        NoteResource r' -> LessonResource r'.id
        NoteTask t -> LessonTask t.id
    , itemLabel = \case
        NoteResource r' -> let ResourceIdentifier x = r'.identifier in x
        NoteTask t -> taskDisplayName t
    , metaFilters =
        [ keywordsFilter ["material"] $ \case NoteResource _ -> True; _ -> False
        , keywordsFilter ["aufgabe"] $ \case NoteTask _ -> True; _ -> False
        ]
    , viewTag = \case
        NoteResource r' -> (Icon.IcnResources, ms $ let ResourceIdentifier x = r'.identifier in x)
        NoteTask t -> (Icon.IcnTask, ms $ taskDisplayName t)
    , placeholder = M.fromMisoString $ C.translate' C.LblSelectResources
    , selectionOrder = ManualReorder
    , tagLayout = TagsVertical
    , onCreate = Nothing
    }

itemsEditorField :: SyncContext -> EditorField LessonNotes LessonNotesPatch f
itemsEditorField r =
  searchSelectEditorField
    r
    "lesson-notes-items"
    noteItemSearchConfig
    (.items)
    (entityPatchTransformedLens #items #items noteItemToLessonNoteItem id)

noteItemToLessonNoteItem :: NoteItem -> LessonNoteItem
noteItemToLessonNoteItem = \case
  NoteResource r' -> LessonResource r'.id
  NoteTask t -> LessonTask t.id
