module Competences.Frontend.Component.TaskEditor.TaskDetailView
  ( taskDetailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), TaskPatch (..), TasksCommand (..))
import Competences.Frontend.Component.Draft (EntityOrigin (..), retargetForDraft)
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), TaskType (..), User)
import Competences.Document.Assignment (Assignment (..), AssignmentName (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Task (TaskAttributes (..), TaskId, TaskIdentifier (..), TaskPurpose (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Frontend.Component.Selector.Common (entityPatchLens)
import Competences.Frontend.Component.TaskEditor.TaskSolutionsList (taskSolutionsListComponent)
import Competences.Frontend.SyncContext (ProjectedChange (..), SyncContext (..), subscribeWithProjection)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Tailwind (class_)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Competences.TaskContent.RichContent (RichContent)
import Optics.Core (Iso', Lens', iso, lens, (&), (%), (.~), (?~), (^.))

-- | Lightweight projection: assignment names referencing a given task
data TaskAssignmentRefs = TaskAssignmentRefs
  { assignmentNames :: ![AssignmentName]
  }
  deriving (Eq, Generic, Show)

-- | Projection function: filter doc.assignments for those containing this taskId
taskAssignmentRefsProjection :: TaskId -> Document -> Maybe User -> TaskAssignmentRefs
taskAssignmentRefsProjection taskId doc _mUser =
  let names =
        [ a.name
        | a <- Ix.toList doc.assignments
        , taskId `elem` a.tasks
        ]
   in TaskAssignmentRefs names

-- | Component: subscribes to projection, renders banner showing which assignments reference a task
assignmentRefsBanner :: SyncContext -> TaskId -> M.Component p TaskAssignmentRefs (ProjectedChange TaskAssignmentRefs)
assignmentRefsBanner r taskId =
  (M.component (TaskAssignmentRefs []) update' view')
    { M.subs = [subscribeWithProjection r (taskAssignmentRefsProjection taskId) id]
    }
  where
    update' change = M.modify $ \_ -> change.projection
    view' m
      | null m.assignmentNames = M.text ""
      | [AssignmentName single] <- m.assignmentNames =
          banner [M.text (C.translate' C.LblUsedInAssignment <> " " <> ms single)]
      | otherwise =
          banner
            [ M.text (C.translate' C.LblUsedInAssignments)
            , MH.ul_
                [class_ "list-disc list-inside mt-1"]
                [MH.li_ [] [M.text (ms n)] | AssignmentName n <- m.assignmentNames]
            ]
    banner content =
      MH.div_
        [class_ "rounded-lg border border-sky-200 bg-sky-50 p-3 text-sm text-sky-800"]
        content

-- | Detail view for editing a SelfContained task
-- Includes the task editor form and a solutions list below it
taskDetailView
  :: SyncContext
  -> EntityOrigin
  -> Task
  -> M.View p a
taskDetailView r origin task =
  MH.div_
    [class_ "space-y-6"]
    [ inlineComponent
        ("task-assignment-refs-" <> M.ms (show task.id))
        (assignmentRefsBanner r task.id)
    , inlineComponent
        ("task-editor-" <> M.ms (show task.id))
        (TE.editorComponent taskEditor r)
    , inlineComponent
        ("task-solutions-" <> M.ms (show task.id))
        (taskSolutionsListComponent r task.id)
    ]
  where
    taskEditorId = "task-editor-" <> M.ms (show task.id)

    wrap = case origin of
      Published -> id
      Draft -> retargetForDraft

    taskEditable =
      TE.editable
        ( \d -> do
            -- Verify it's a SelfContained task — check both real and draft collections
            case task.taskType of
              SelfContained _ ->
                let mTask = case origin of
                      Published -> Ix.getOne $ d.tasks Ix.@= task.id
                      Draft -> Ix.getOne $ d.draftTasks Ix.@= task.id
                 in fmap (\c -> (c, (d ^. #locks) Map.!? TaskLock c.id)) mTask
              SubTask _ _ -> Nothing -- Not editable in this editor
        )
        & (#modify ?~ (\t modify -> wrap $ Tasks $ OnTasks (Modify t.id modify)))
        & (#delete ?~ (\t -> wrap $ Tasks $ OnTasks (Delete t.id)))

    taskEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblEditSelfContainedTask)
            id
        )
        taskEditable
        `TE.addNamedField` ( C.translate' C.LblTaskIdentifier
                           , TE.textEditorField identifierViewLens identifierPatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskContent
                           , TE.richTextEditorField r.formulaCache "content" contentViewLens contentPatchLens
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
                               0  -- minResults=0: tasks can have no competences
                               (entityPatchLens primaryViewLens primaryPatchLens)
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskSecondaryCompetences
                           , competenceLevelEditorField
                               r
                               (taskEditorId <> "-secondary-competences")
                               0  -- minResults=0: tasks can have no competences
                               (entityPatchLens secondaryViewLens secondaryPatchLens)
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskDisplayInResources
                           , TE.boolEditorField displayInResourcesViewLens displayInResourcesPatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblAttachments
                           , TE.fileUploadEditorField r (#attachments) (#attachments)
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

-- Lenses for content (Maybe RichContent <-> RichContent conversion, empty = Nothing)
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
