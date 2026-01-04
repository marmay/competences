module Competences.Frontend.Component.TaskEditor.TaskGroupDetailView
  ( taskGroupDetailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), SubTaskPatch (..), TaskGroupPatch (..), TasksCommand (..))
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), TaskGroup (..), TaskType (..), emptyDocument)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskAttributesOverride (..)
  , TaskGroupIdentifier (..)
  , TaskId
  , TaskIdentifier (..)
  , TaskPurpose (..)
  , getTasksInGroup
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField)
import Competences.Frontend.Component.Selector.Common (entityPatchLens)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Optics.Core (Iso', Lens', iso, (&), (%), (?~), (^.))

-- | Detail view for editing a TaskGroup and its SubTasks
taskGroupDetailView
  :: SyncDocumentRef
  -> TaskGroup
  -> M.View p a
taskGroupDetailView r group =
  V.component
    ("group-editor-" <> M.ms (show group.id))
    (taskGroupEditorComponent r group)

-- | Internal model for the TaskGroup editor component
data TaskGroupEditorModel = TaskGroupEditorModel
  { currentDocument :: !Document
  , subTasks :: ![Task]  -- SubTasks in this group
  }
  deriving (Eq, Generic, Show)

data TaskGroupEditorAction
  = UpdateDocument !DocumentChange
  | CreateSubTask
  | DeleteSubTask !TaskId
  | UpdateSubTaskIdentifier !TaskId !Text
  | UpdateSubTaskContent !TaskId !Text
  deriving (Eq, Show)

-- | The TaskGroup editor component with SubTask management
taskGroupEditorComponent :: SyncDocumentRef -> TaskGroup -> M.Component p TaskGroupEditorModel TaskGroupEditorAction
taskGroupEditorComponent r group =
  (M.component model update view')
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = TaskGroupEditorModel
      { currentDocument = emptyDocument
      , subTasks = []
      }

    update (UpdateDocument dc) = M.modify $ \m ->
      let doc = dc.document
          subTasks' = getTasksInGroup group.id doc.tasks
       in m { currentDocument = doc, subTasks = subTasks' }

    update CreateSubTask = M.io_ $ do
      taskId <- nextId r
      let newSubTask = Task
            { id = taskId
            , identifier = TaskIdentifier ""
            , content = Nothing
            , taskType = SubTask group.id emptyOverride
            }
      modifySyncDocument r $ Tasks (OnSubTasks (Create newSubTask))
      where
        emptyOverride = TaskAttributesOverride Nothing Nothing Nothing Nothing

    update (DeleteSubTask taskId) = M.io_ $ do
      modifySyncDocument r $ Tasks (OnSubTasks (Delete taskId))

    update (UpdateSubTaskIdentifier taskId newIdent) = M.io_ $ do
      let patch = SubTaskPatch
            { identifier = Just (TaskIdentifier "", TaskIdentifier newIdent)
            , content = Nothing
            , primary = Nothing
            , secondary = Nothing
            , purpose = Nothing
            , displayInResources = Nothing
            }
      modifySyncDocument r $ Tasks (OnSubTasks (Modify taskId (Release patch)))

    update (UpdateSubTaskContent taskId newContent) = M.io_ $ do
      let contentVal = if newContent == "" then Nothing else Just newContent
      let patch = SubTaskPatch
            { identifier = Nothing
            , content = Just (Nothing, contentVal)
            , primary = Nothing
            , secondary = Nothing
            , purpose = Nothing
            , displayInResources = Nothing
            }
      modifySyncDocument r $ Tasks (OnSubTasks (Modify taskId (Release patch)))

    view' m =
      M.div_
        [class_ "space-y-6"]
        [ viewGroupEditor m
        , viewSubTasksSection m
        ]

    viewGroupEditor _m =
      V.component
        ("task-group-form-" <> ms (show group.id))
        (TE.editorComponent taskGroupEditor r)

    taskGroupEditorId = "task-group-editor-" <> ms (show group.id)

    taskGroupEditable =
      TE.editable
        ( \d ->
            fmap
              (\g -> (g, (d ^. #locks) Map.!? TaskGroupLock g.id))
              (Ix.getOne $ d.taskGroups Ix.@= group.id)
        )
        & (#modify ?~ (\g modify -> Tasks $ OnTaskGroups (Modify g.id modify)))
        & (#delete ?~ (\g -> Tasks $ OnTaskGroups (Delete g.id)))

    taskGroupEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblEditTaskGroup)
            id
        )
        taskGroupEditable
        `TE.addNamedField` ( C.translate' C.LblTaskIdentifier
                           , TE.textEditorField groupIdentifierViewLens groupIdentifierPatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskGroupContentBefore
                           , TE.textEditorField contentBeforeViewLens contentBeforePatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskGroupContentAfter
                           , TE.textEditorField contentAfterViewLens contentAfterPatchLens
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
                               (taskGroupEditorId <> "-primary-competences")
                               (entityPatchLens primaryViewLens primaryPatchLens)
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskSecondaryCompetences
                           , competenceLevelEditorField
                               r
                               (taskGroupEditorId <> "-secondary-competences")
                               (entityPatchLens secondaryViewLens secondaryPatchLens)
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskDisplayInResources
                           , TE.boolEditorField displayInResourcesViewLens displayInResourcesPatchLens
                           )

    viewSubTasksSection m =
      let isLocked = Map.member (TaskGroupLock group.id) m.currentDocument.locks
       in M.div_
            [class_ "border-t pt-4"]
            [ M.div_
                [class_ "flex items-center justify-between mb-4"]
                [ Typography.h3 (C.translate' C.LblSubTasks)
                , if isLocked
                    then Button.buttonSecondary (C.translate' C.LblAddSubTask)
                           & Button.withIcon IcnAdd
                           & Button.withClick CreateSubTask
                           & Button.renderButton
                    else M.text ""
                ]
            , if null m.subTasks
                then Typography.muted (C.translate' C.LblNoSubTasks)
                else M.div_ [class_ "space-y-2"] (map (viewSubTaskRow isLocked) m.subTasks)
            ]

    viewSubTaskRow isLocked task =
      let TaskIdentifier ident = task.identifier
          contentText = fromMaybe "" task.content
       in if isLocked
            then viewSubTaskRowEditable task ident contentText
            else viewSubTaskRowReadOnly ident contentText

    viewSubTaskRowEditable task ident contentText =
      M.div_
        [class_ "flex items-center gap-2 p-2 bg-muted/30 rounded"]
        [ M.div_
            [class_ "flex-1 flex items-center gap-2"]
            [ Input.defaultInput
                & Input.withValue (ms ident)
                & Input.withPlaceholder (C.translate' C.LblTaskIdentifier)
                & Input.withOnInput (\v -> UpdateSubTaskIdentifier task.id (M.fromMisoString v))
                & Input.renderInput
            , Input.defaultInput
                & Input.withValue (ms contentText)
                & Input.withPlaceholder (C.translate' C.LblTaskContent)
                & Input.withOnInput (\v -> UpdateSubTaskContent task.id (M.fromMisoString v))
                & Input.renderInput
            ]
        , Button.buttonDestructive ""
            & Button.withIcon IcnDelete
            & Button.withClick (DeleteSubTask task.id)
            & Button.renderButton
        ]

    viewSubTaskRowReadOnly ident contentText =
      M.div_
        [class_ "flex items-center gap-2 p-2 bg-muted/30 rounded"]
        [ M.span_ [class_ "text-sm font-medium"] [M.text (ms ident)]
        , if contentText == ""
            then M.text ""
            else M.span_ [class_ "text-sm text-muted-foreground"] [M.text (ms contentText)]
        ]

-- Lenses for TaskGroup identifier
groupIdentifierTextIso :: Iso' TaskGroupIdentifier Text
groupIdentifierTextIso = iso (\(TaskGroupIdentifier t) -> t) TaskGroupIdentifier

changeGroupIdentifierTextIso :: Iso' (Change TaskGroupIdentifier) (Change Text)
changeGroupIdentifierTextIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (TaskGroupIdentifier a, TaskGroupIdentifier b)) = Just (a, b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (TaskGroupIdentifier a, TaskGroupIdentifier b)

groupIdentifierViewLens :: Lens' TaskGroup Text
groupIdentifierViewLens = #identifier % groupIdentifierTextIso

groupIdentifierPatchLens :: Lens' TaskGroupPatch (Change Text)
groupIdentifierPatchLens = #identifier % changeGroupIdentifierTextIso

-- Lenses for contentBefore (Maybe Text <-> Text)
maybeTextIso :: Iso' (Maybe Text) Text
maybeTextIso = iso
  (\case
      Nothing -> ""
      Just t -> t
  )
  (\t -> if t == "" then Nothing else Just t)

changeMaybeTextIso :: Iso' (Change (Maybe Text)) (Change Text)
changeMaybeTextIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (a, b)) = Just (fromMaybe "" a, fromMaybe "" b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (if a == "" then Nothing else Just a, if b == "" then Nothing else Just b)

contentBeforeViewLens :: Lens' TaskGroup Text
contentBeforeViewLens = #contentBefore % maybeTextIso

contentBeforePatchLens :: Lens' TaskGroupPatch (Change Text)
contentBeforePatchLens = #contentBefore % changeMaybeTextIso

contentAfterViewLens :: Lens' TaskGroup Text
contentAfterViewLens = #contentAfter % maybeTextIso

contentAfterPatchLens :: Lens' TaskGroupPatch (Change Text)
contentAfterPatchLens = #contentAfter % changeMaybeTextIso

-- Lenses for default purpose
purposeViewLens :: Lens' TaskGroup TaskPurpose
purposeViewLens = #defaultTaskAttributes % #purpose

purposePatchLens :: Lens' TaskGroupPatch (Change TaskPurpose)
purposePatchLens = #purpose

-- Lenses for default primary competences
primaryViewLens :: Lens' TaskGroup [CompetenceLevelId]
primaryViewLens = #defaultTaskAttributes % #primary

primaryPatchLens :: Lens' TaskGroupPatch (Change [CompetenceLevelId])
primaryPatchLens = #primary

-- Lenses for default secondary competences
secondaryViewLens :: Lens' TaskGroup [CompetenceLevelId]
secondaryViewLens = #defaultTaskAttributes % #secondary

secondaryPatchLens :: Lens' TaskGroupPatch (Change [CompetenceLevelId])
secondaryPatchLens = #secondary

-- Lenses for default displayInResources
displayInResourcesViewLens :: Lens' TaskGroup Bool
displayInResourcesViewLens = #defaultTaskAttributes % #displayInResources

displayInResourcesPatchLens :: Lens' TaskGroupPatch (Change Bool)
displayInResourcesPatchLens = #displayInResources
