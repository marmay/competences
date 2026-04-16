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
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), lockOwner)
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.Editor (Editable (..), editable, editor, addNamedField, editorComponent, textEditorField, richTextWithFilesEditorField, enumEditorField, boolEditorField, fileUploadEditorField)
import Competences.Frontend.Component.Editor.FormView (editorFormView')
import Competences.Frontend.Component.Editor.Types (Action, Model, singlePatchLens)
import Competences.Frontend.Component.Selector.Common (entityPatchLens)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Frontend.Component.TaskEditor.Lenses
  ( identifierViewLens, identifierPatchLens
  , titleViewLens, titlePatchLens
  , contentViewLens, contentPatchLens
  )
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager
  ( WindowMode
  , PinId
  , pinSaveStateLens
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core ((&), (?~))
import Optics.Core qualified as O

-- | Task pin editor factory.
-- Receives optional saved state and creates a component with a binding
-- that persists patches to the parent model's pinSaveStates.
taskPinEditor
  :: SyncContext -> TaskId -> EntityOrigin -> PinId
  -> WindowMode -> Maybe TaskPatch
  -> M.Component WM.Model (Model Task TaskPatch Maybe) (Action Task TaskPatch)
taskPinEditor r taskId origin pid _mode mSaved =
  (editorComponent taskEditor r (fromMaybe def mSaved))
        { M.bindings =
            [ O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL singlePatchLens
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


