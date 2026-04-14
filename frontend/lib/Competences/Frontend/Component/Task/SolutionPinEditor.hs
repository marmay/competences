-- | Self-contained solution editor for pinned dialogs.
module Competences.Frontend.Component.Task.SolutionPinEditor
  ( solutionPinEditor
  )
where

import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Command.Solutions (SolutionPatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Solution (..), lockOwner)
import Competences.Document.Solution (SolutionId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor (Editable (..), editable, editor, addNamedField, editorComponent, enumEditorField, richTextEditorField)
import Competences.Frontend.Component.Editor.FormView (editorFormView')
import Competences.Frontend.Component.Editor.Types (Action, Model (..))
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager
  ( WindowMode
  , PinId
  , pinSaveStateLens
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Data.Default (Default (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Miso qualified as M
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
        ( editorFormView'
            (C.translate' C.LblEditSolution)
            id
        )
        solEditable
        `addNamedField` ( C.translate' C.LblSolutionTypeLabel
                        , enumEditorField
                            (C.translate' . C.LblSolutionType)
                            #solutionType
                            #solutionType
                        )
        `addNamedField` ( C.translate' C.LblSolutionContent
                        , richTextEditorField r.formulaCache "solution-content" #content #content
                        )

-- | Lens to extract the single patch value from the Editor model.
singlePatchLens :: O.Lens' (Model Solution SolutionPatch Maybe) (Maybe SolutionPatch)
singlePatchLens = O.lens getter setter
  where
    getter m = case Map.elems m.patches of
      [p] -> Just p
      _ -> Nothing
    setter m (Just p) = m {patches = Map.map (const p) m.patches}
    setter m Nothing = m
