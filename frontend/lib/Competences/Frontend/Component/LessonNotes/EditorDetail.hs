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
  , Resource (..)
  , Task (..)
  )
import Competences.Document.LessonNotes (LessonNoteItem (..))
import Competences.Document.Resource (ResourceIdentifier (..))
import Competences.Document.Task (TaskIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField)
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.LessonSelector (lessonEditorField)
import Competences.Frontend.Component.Selector.SearchSelect (SearchSelectConfig (..), SelectionOrder (..), TagLayout (..), keywordsFilter)
import Competences.Frontend.Component.Selector.SearchSelectEditorField (searchSelectEditorField)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Query.Resource qualified as QResource
import Competences.Query.Task qualified as QTask
import Data.Map.Strict qualified as Map
import Miso qualified as M
import Optics.Core ((&), (?~), (^.))

-- | Detail view for editing a lesson notes entry
editorDetailView
  :: SyncContext
  -> LessonNotes
  -> M.View (SD.Model LessonNotes mode) (SD.Action mode)
editorDetailView r ln =
  inlineComponent
    ("lesson-notes-editor-" <> M.ms (show ln.id))
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

-- ============================================================================
-- NoteItem: union of Resource and Task for the combined selector
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
        NoteTask t -> let TaskIdentifier x = t.identifier in x
    , metaFilters =
        [ keywordsFilter ["material"] $ \case NoteResource _ -> True; _ -> False
        , keywordsFilter ["aufgabe"] $ \case NoteTask _ -> True; _ -> False
        ]
    , viewTag = \case
        NoteResource r' -> (Icon.IcnResources, M.ms $ let ResourceIdentifier x = r'.identifier in x)
        NoteTask t -> (Icon.IcnTask, M.ms $ let TaskIdentifier x = t.identifier in x)
    , placeholder = M.fromMisoString $ C.translate' C.LblSelectResources
    , selectionOrder = ManualReorder
    , tagLayout = TagsVertical
    }

-- | Editor field for the items list using SearchSelect
-- Viewer: comma-separated resource/task names
-- Editor: unified SearchSelect with @res/@aufg meta filters
itemsEditorField
  :: SyncContext
  -> EditorField LessonNotes LessonNotesPatch f
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
