module Competences.Frontend.Component.SelfContainedTaskEditor
  ( selfContainedTaskEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), TaskPatch (..), TasksCommand (..))
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), TaskType (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Task (TaskAttributes (..), TaskIdentifier (..), TaskPurpose (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Frontend.Component.Selector.Common (entityPatchLens)
import Competences.Frontend.Component.Selector.SelfContainedTaskSelector (selfContainedTaskSelectorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as TW
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Iso', Lens', iso, lens, (&), (%), (.~), (?~), (^.))

data Model = Model
  { task :: !(Maybe Task)
  }
  deriving (Eq, Generic, Show)

data Action

selfContainedTaskEditorComponent :: SyncDocumentRef -> M.Component p Model Action
selfContainedTaskEditorComponent r =
  M.component model update view
  where
    model = Model Nothing
    update _ = pure ()

    view m =
      V.sideMenu
        (V.componentA "task-editor-selection" [TW.tailwind [TW.HFull]] (selfContainedTaskSelectorComponent r #task))
        (V.componentA taskEditorId [TW.tailwind [TW.HFull]] (TE.editorComponent taskEditor r))
      where
        taskEditorId = "task-editor-editor-" <> maybe "empty" (M.ms . show . (.id)) m.task
        taskEditable =
          TE.editable
            ( \d -> do
                t <- m.task
                -- Verify it's a SelfContained task
                case t.taskType of
                  SelfContained _ ->
                    fmap
                      (\c -> (c, (d ^. #locks) Map.!? TaskLock c.id))
                      (Ix.getOne $ d.tasks Ix.@= t.id)
                  SubTask _ _ -> Nothing -- Not editable in this editor
            )
            & (#modify ?~ (\t modify -> Tasks $ OnTasks (Modify t.id modify)))
            & (#delete ?~ (\t -> Tasks $ OnTasks (Delete t.id)))
        taskEditor =
          TE.editor
            ( TE.editorFormView'
                (C.translate' C.LblEditEvidence)
                id
            )
            taskEditable
            `TE.addNamedField` ( C.translate' C.LblTaskIdentifier
                               , TE.textEditorField identifierViewLens identifierPatchLens
                               )
            `TE.addNamedField` ( C.translate' C.LblTaskContent
                               , TE.textEditorField contentViewLens contentPatchLens
                               )
            `TE.addNamedField` ( C.translate' C.LblTaskPurposeLabel
                               , TE.enumEditorField
                                   (C.translate' . C.LblTaskPurpose)
                                   purposeViewLens
                                   purposePatchLens
                               )
            `TE.addNamedField` ( C.translate' C.LblTaskPrimaryCompetences
                               , competenceLevelEditorField
                                   r
                                   "task-primary-competences"
                                   (entityPatchLens primaryViewLens primaryPatchLens)
                               )
            `TE.addNamedField` ( C.translate' C.LblTaskSecondaryCompetences
                               , competenceLevelEditorField
                                   r
                                   "task-secondary-competences"
                                   (entityPatchLens secondaryViewLens secondaryPatchLens)
                               )

-- Lenses for identifier (TaskIdentifier <-> Text conversion)
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

-- Lenses for content (Maybe Text <-> Text conversion, empty = Nothing)
contentIso :: Iso' (Maybe Text) Text
contentIso = iso
  (\case
      Nothing -> ""
      Just t -> t
  )
  (\t -> if t == "" then Nothing else Just t)

changeContentIso :: Iso' (Change (Maybe Text)) (Change Text)
changeContentIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (a, b)) = Just (maybe "" id a, maybe "" id b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (if a == "" then Nothing else Just a, if b == "" then Nothing else Just b)

contentViewLens :: Lens' Task Text
contentViewLens = #content % contentIso

contentPatchLens :: Lens' TaskPatch (Change Text)
contentPatchLens = #content % changeContentIso

-- Lenses for purpose (extract from TaskType)
purposeViewLens :: Lens' Task TaskPurpose
purposeViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SelfContained attrs -> attrs.purpose
      SubTask _ _ -> Practice -- fallback, shouldn't happen
    setter task newPurpose = case task.taskType of
      SelfContained attrs -> task & #taskType .~ SelfContained (attrs & #purpose .~ newPurpose)
      SubTask _ _ -> task -- Can't modify, shouldn't happen

purposePatchLens :: Lens' TaskPatch (Change TaskPurpose)
purposePatchLens = #purpose

-- Lenses for primary competences (extract from TaskType → SelfContained → TaskAttributes)
primaryViewLens :: Lens' Task [CompetenceLevelId]
primaryViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SelfContained attrs -> attrs.primary
      SubTask _ _ -> [] -- fallback, shouldn't happen
    setter task newPrimary = case task.taskType of
      SelfContained attrs -> task & #taskType .~ SelfContained (attrs & #primary .~ newPrimary)
      SubTask _ _ -> task -- Can't modify, shouldn't happen

primaryPatchLens :: Lens' TaskPatch (Change [CompetenceLevelId])
primaryPatchLens = #primary

-- Lenses for secondary competences (extract from TaskType → SelfContained → TaskAttributes)
secondaryViewLens :: Lens' Task [CompetenceLevelId]
secondaryViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SelfContained attrs -> attrs.secondary
      SubTask _ _ -> [] -- fallback, shouldn't happen
    setter task newSecondary = case task.taskType of
      SelfContained attrs -> task & #taskType .~ SelfContained (attrs & #secondary .~ newSecondary)
      SubTask _ _ -> task -- Can't modify, shouldn't happen

secondaryPatchLens :: Lens' TaskPatch (Change [CompetenceLevelId])
secondaryPatchLens = #secondary
