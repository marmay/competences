-- | Self-contained task editor for pinned dialogs.
--
-- Creates an Editor component with a binding that persists its patches
-- to the parent model's pinSaveStates. On remount after dormancy, the
-- factory receives typed saved state directly (Dynamic cast handled by
-- AnyPinnedDialog).
module Competences.Frontend.Component.Task.PinEditor
  ( taskPinEditor
  )
where

import Competences.Command (Command (..), EntityCommand (..), TaskPatch (..), TasksCommand (..))
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), lockOwner)
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.Editor (Editable (..), editable, editor, addNamedField, editorComponent, textEditorField, richTextWithFilesEditorField, enumEditorField, boolEditorField, fileUploadEditorField)
import Competences.Frontend.Component.Editor.FormView (editorFormView')
import Competences.Frontend.Component.Editor.Types (Action, Model)
import Competences.Frontend.Component.Selector.Common (entityPatchLens)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Document.Task (TaskIdentifier (..))
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.TaskContent.RichContent (RichContent)
import Competences.Frontend.SyncContext.WindowManager
  ( WindowMode
  , PinId
  , justLens
  , pinSaveStateLens
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Iso', Lens', (&), (?~), iso, (%))
import Optics.Core qualified as O

-- | Task pin editor factory.
-- Receives optional saved state and creates a component with a binding
-- that persists patches to the parent model's pinSaveStates.
taskPinEditor
  :: SyncContext -> TaskId -> EntityOrigin -> PinId
  -> WindowMode -> Maybe (Model Task TaskPatch Maybe)
  -> M.Component WM.Model (Model Task TaskPatch Maybe) (Action Task TaskPatch)
taskPinEditor r taskId origin pid _mode mSaved =
  (editorComponent taskEditor r mSaved def)
        { M.bindings =
            [ O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL justLens
            ]
        }
  where
    wrap = wrapForOrigin origin
    editorId = "task-pin-editor-" <> ms (show taskId)

    taskEditable :: Editable Maybe Task TaskPatch
    taskEditable =
      editable
        ( \d ->
            let t = case origin of
                  Published -> Ix.getOne (d.tasks Ix.@= taskId)
                  Draft -> Ix.getOne (d.draftTasks Ix.@= taskId)
             in fmap (\t' -> (t', lockOwner (TaskLock t'.id) d)) t
        )
        & (#modify ?~ (\t modify -> wrap $ Tasks $ OnTasks (Modify t.id modify)))

    taskEditor =
      editor
        ( editorFormView'
            (C.translate' C.LblEditTask)
            id
        )
        taskEditable
        `addNamedField` ( C.translate' C.LblTaskIdentifier
                        , textEditorField identifierViewLens identifierPatchLens
                        )
        `addNamedField` ( C.translate' C.LblTaskTitle
                        , textEditorField titleViewLens titlePatchLens
                        )
        `addNamedField` ( C.translate' C.LblTaskContent
                        , richTextWithFilesEditorField r "content" contentViewLens contentPatchLens #attachments
                        )
        `addNamedField` ( C.translate' C.LblTaskPurposeLabel
                        , enumEditorField
                            (C.translate' . C.LblTaskPurpose)
                            #purpose
                            #purpose
                        )
        `addNamedField` ( C.translate' C.LblTaskPrimaryCompetences
                        , competenceLevelEditorField
                            r
                            (editorId <> "-primary-competences")
                            0
                            (entityPatchLens #primary #primary)
                        )
        `addNamedField` ( C.translate' C.LblTaskSecondaryCompetences
                        , competenceLevelEditorField
                            r
                            (editorId <> "-secondary-competences")
                            0
                            (entityPatchLens #secondary #secondary)
                        )
        `addNamedField` ( C.translate' C.LblTaskDisplayInResources
                        , boolEditorField #displayInResources #displayInResources
                        )
        `addNamedField` ( C.translate' C.LblAttachments
                        , fileUploadEditorField r #attachments #attachments
                        )

-- ---------------------------------------------------------------------------
-- Lenses for Task/TaskPatch fields
-- ---------------------------------------------------------------------------

taskIdentifierTextIso :: Iso' TaskIdentifier Text
taskIdentifierTextIso = iso (\(TaskIdentifier t) -> t) TaskIdentifier

changeTaskIdentifierTextIso :: Iso' (Change TaskIdentifier) (Change Text)
changeTaskIdentifierTextIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (TaskIdentifier a, TaskIdentifier b)) = Just (a, b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (TaskIdentifier a, TaskIdentifier b)

identifierViewLens :: Lens' Task Text
identifierViewLens = #identifier % taskIdentifierTextIso

identifierPatchLens :: Lens' TaskPatch (Change Text)
identifierPatchLens = #identifier % changeTaskIdentifierTextIso

titleViewLens :: Lens' Task Text
titleViewLens = #title

titlePatchLens :: Lens' TaskPatch (Change Text)
titlePatchLens = #title

contentIso :: Iso' (Maybe RichContent) RichContent
contentIso = iso (fromMaybe mempty) (\t -> if t == mempty then Nothing else Just t)

changeContentIso :: Iso' (Change (Maybe RichContent)) (Change RichContent)
changeContentIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (a, b)) = Just (fromMaybe mempty a, fromMaybe mempty b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (if a == mempty then Nothing else Just a, if b == mempty then Nothing else Just b)

contentViewLens :: Lens' Task RichContent
contentViewLens = #content % contentIso

contentPatchLens :: Lens' TaskPatch (Change RichContent)
contentPatchLens = #content % changeContentIso
