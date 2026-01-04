module Competences.Frontend.Component.TaskEditor.TaskDetailView
  ( taskDetailView
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
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Miso qualified as M
import Optics.Core (Iso', Lens', iso, lens, (&), (%), (.~), (?~), (^.))

-- | Detail view for editing a SelfContained task
taskDetailView
  :: SyncDocumentRef
  -> Task
  -> M.View p a
taskDetailView r task =
  V.component
    ("task-editor-" <> M.ms (show task.id))
    (TE.editorComponent taskEditor r)
  where
    taskEditorId = "task-editor-" <> M.ms (show task.id)

    taskEditable =
      TE.editable
        ( \d -> do
            -- Verify it's a SelfContained task
            case task.taskType of
              SelfContained _ ->
                fmap
                  (\c -> (c, (d ^. #locks) Map.!? TaskLock c.id))
                  (Ix.getOne $ d.tasks Ix.@= task.id)
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
                               (taskEditorId <> "-primary-competences")
                               (entityPatchLens primaryViewLens primaryPatchLens)
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskSecondaryCompetences
                           , competenceLevelEditorField
                               r
                               (taskEditorId <> "-secondary-competences")
                               (entityPatchLens secondaryViewLens secondaryPatchLens)
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskDisplayInResources
                           , TE.boolEditorField displayInResourcesViewLens displayInResourcesPatchLens
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
    fwd (Just (a, b)) = Just (fromMaybe "" a, fromMaybe "" b)
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

-- Lenses for displayInResources (extract from TaskType → SelfContained → TaskAttributes)
displayInResourcesViewLens :: Lens' Task Bool
displayInResourcesViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SelfContained attrs -> attrs.displayInResources
      SubTask _ _ -> True -- fallback, shouldn't happen
    setter task newValue = case task.taskType of
      SelfContained attrs -> task & #taskType .~ SelfContained (attrs & #displayInResources .~ newValue)
      SubTask _ _ -> task -- Can't modify, shouldn't happen

displayInResourcesPatchLens :: Lens' TaskPatch (Change Bool)
displayInResourcesPatchLens = #displayInResources
