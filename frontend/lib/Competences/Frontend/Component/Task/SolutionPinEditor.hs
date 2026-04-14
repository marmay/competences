-- | Self-contained solution editor for pinned dialogs.
-- Shows a collapsible task preview above the solution form fields.
module Competences.Frontend.Component.Task.SolutionPinEditor
  ( solutionPinEditor
  )
where

import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Command.Solutions (SolutionPatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Solution (..), lockOwner)
import Competences.Document.Solution (SolutionId)
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Editor (Editable (..), editable, editor, addNamedField, editorComponent, enumEditorField, richTextEditorField)
import Competences.Frontend.Component.Editor.FormView (editorFormView)
import Competences.Frontend.Component.Editor.Types (Action, Model (..))
import Competences.Frontend.Component.Editor.View (EditorView, EditorViewData (..), EditorViewItem (..))
import Competences.Frontend.Component.Task.Component (TaskConfig (..), TaskDisplayMode (..), taskComponent)
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager
  ( WindowMode
  , PinId
  , inlineComponent
  , pinSaveStateLens
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Data.Default (Default (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((&), (?~))
import Optics.Core qualified as O

-- | Solution pin editor factory.
solutionPinEditor
  :: SyncContext -> SolutionId -> PinId
  -> WindowMode -> Maybe SolutionPatch
  -> M.Component WM.Model (Model Solution SolutionPatch Maybe) (Action Solution SolutionPatch)
solutionPinEditor r solId pid _mode mSaved =
  (editorComponent solEditor r (fromMaybe def mSaved))
    { M.bindings =
        [ O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL singlePatchLens
        ]
    }
  where
    solEditable :: Editable Maybe Solution SolutionPatch
    solEditable =
      editable
        ( \d ->
            let s = Ix.getOne (d.solutions Ix.@= solId)
             in fmap (\s' -> (s', lockOwner (SolutionLock s'.id) d)) s
        )
        & (#modify ?~ (\s modify -> Solutions $ OnSolutions (Modify s.id modify)))

    solEditor =
      editor
        (solEditorView r solId)
        solEditable
        `addNamedField` ( C.LblSolutionTypeLabel
                        , enumEditorField
                            (C.translate' . C.LblSolutionType)
                            #solutionType
                            #solutionType
                        )
        `addNamedField` ( C.LblSolutionContent
                        , richTextEditorField r.formulaCache "solution-content" #content #content
                        )

-- ============================================================================
-- Custom editor view with task preview
-- ============================================================================

-- | Editor view that renders a collapsible task preview above the form.
solEditorView :: SyncContext -> SolutionId -> EditorView Solution SolutionPatch Maybe C.Label
solEditorView r _solId viewData =
  let formView = editorFormView
        (Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem))
        (C.translate' C.LblEditSolution)
        C.translate'
        viewData
      -- Extract task ID from the solution item (if present)
      mTaskId :: Maybe TaskId
      mTaskId = do
        vi <- viewData.items
        let sol :: Solution = vi.item
        pure sol.taskId
   in MH.div_
        [class_ "space-y-4"]
        ( [ inlineComponent ("sol-task-preview-" <> ms (show taskId))
              (taskComponent r (TaskConfig taskId Published TaskPreview))
          | Just taskId <- [mTaskId]
          ]
            <> [formView]
        )

-- ============================================================================
-- Internal
-- ============================================================================

-- | Lens to extract the single patch value from the Editor model.
singlePatchLens :: O.Lens' (Model Solution SolutionPatch Maybe) (Maybe SolutionPatch)
singlePatchLens = O.lens getter setter
  where
    getter m = case Map.elems m.patches of
      [p] -> Just p
      _ -> Nothing
    setter m (Just p) = m {patches = Map.map (const p) m.patches}
    setter m Nothing = m
