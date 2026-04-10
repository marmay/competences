module Competences.Frontend.Component.TaskEditor.SolutionEditorDetail
  ( editorDetailView
  , solutionInlineEditor
  )
where

import Competences.Command (Command (..), EntityCommand (..), SolutionsCommand (..))
import Competences.Command.Solutions (SolutionPatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Lock (..)
  , LockHolder (..)
  , Solution (..)
  )
import Competences.Document.Task (TaskId, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField (..), readOnlyField)
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext (..), subscribeDocument)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Text (text_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~), (?~), (^.))

-- | Detail view for editing a solution
editorDetailView
  :: SyncContext
  -> Solution
  -> M.View (SD.Model Solution mode) (SD.Action mode)
editorDetailView r solution =
  inlineComponent
    ("solution-editor-" <> M.ms (show solution.id))
    (TE.editorComponent solutionEditor r)
  where
    solutionEditable =
      TE.editable
        ( \d ->
            fmap
              (\s -> (s, fmap (.userId) $ (d ^. #locks) Map.!? SolutionLock s.id))
              (Ix.getOne $ d.solutions Ix.@= solution.id)
        )
        & (#modify ?~ (\s modify -> Solutions $ OnSolutions (Modify s.id modify)))
        & (#delete ?~ (\s -> Solutions $ OnSolutions (Delete s.id)))

    solutionEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblEditSolution)
            id
        )
        solutionEditable
        `TE.addNamedField` ( C.translate' C.LblSolutionTask
                           , taskReadOnlyField r
                           )
        `TE.addNamedField` ( C.translate' C.LblSolutionTypeLabel
                           , TE.enumEditorField
                               (C.translate' . C.LblSolutionType)
                               #solutionType
                               #solutionType
                           )
        `TE.addNamedField` ( C.translate' C.LblSolutionContent
                           , TE.richTextEditorField r.formulaCache "content" #content #content
                           )

-- | Read-only field showing the task identifier
-- Task can't be changed after solution creation
-- Uses a component that subscribes to document updates to resolve task names
taskReadOnlyField :: SyncContext -> EditorField Solution SolutionPatch f
taskReadOnlyField r =
  readOnlyField $ \solution ->
    inlineComponent
      ("task-display-" <> M.ms (show solution.taskId))
      (taskDisplayComponent r solution.taskId)

-- | Model for task display component
data TaskDisplayModel = TaskDisplayModel
  { taskName :: !M.MisoString
  }
  deriving (Eq, Generic, Show)

-- | Action for task display component
newtype TaskDisplayAction = TaskDisplayDocumentChanged DocumentChange
  deriving (Eq, Show)

-- | Component that displays a task name by subscribing to document updates
taskDisplayComponent :: SyncContext -> TaskId -> M.Component p TaskDisplayModel TaskDisplayAction
taskDisplayComponent r taskId =
  (M.component model update view')
    { M.subs = [subscribeDocument r TaskDisplayDocumentChanged]
    }
  where
    model = TaskDisplayModel { taskName = "" }

    update (TaskDisplayDocumentChanged (DocumentChange doc _)) =
      M.modify $ \m ->
        let name = case Ix.getOne (doc.tasks Ix.@= taskId) of
              Nothing -> "(Aufgabe nicht gefunden)"
              Just task -> M.ms (taskDisplayName task)
         in m & #taskName .~ name

    view' m =
      if m.taskName == ""
        then Typography.muted "..."
        else text_ m.taskName

-- | Inline editor for a solution - simpler version without the full header
-- This is used in TaskSolutionsList where we just need the edit fields
solutionInlineEditor
  :: SyncContext
  -> Solution
  -> M.View p a
solutionInlineEditor r solution =
  inlineComponent
    ("solution-inline-editor-" <> M.ms (show solution.id))
    (TE.editorComponent inlineEditor r)
  where
    solutionEditable =
      TE.editable
        ( \d ->
            fmap
              (\s -> (s, fmap (.userId) $ (d ^. #locks) Map.!? SolutionLock s.id))
              (Ix.getOne $ d.solutions Ix.@= solution.id)
        )
        & (#modify ?~ (\s modify -> Solutions $ OnSolutions (Modify s.id modify)))

    -- Inline editor without header or delete button
    inlineEditor =
      TE.editor
        (TE.editorFormViewInline id)  -- Use inline form view (no header)
        solutionEditable
        `TE.addNamedField` ( C.translate' C.LblSolutionTypeLabel
                           , TE.enumEditorField
                               (C.translate' . C.LblSolutionType)
                               #solutionType
                               #solutionType
                           )
        `TE.addNamedField` ( C.translate' C.LblSolutionContent
                           , TE.richTextEditorField r.formulaCache "content" #content #content
                           )
