module Competences.Frontend.Component.TaskEditor.TaskGroupDetailView
  ( taskGroupDetailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), SubTaskPatch (..), TaskGroupPatch (..), TasksCommand (..))
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), Task (..), TaskGroup (..), TaskType (..), emptyDocument)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskAttributesOverride (..)
  , TaskGroupId
  , TaskGroupIdentifier (..)
  , TaskId
  , TaskIdentifier (..)
  , TaskPurpose (..)
  , getTasksInGroup
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField (..), mkFieldLens)
import Competences.Frontend.Component.Editor.Types qualified as TE
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelEditorField, competenceLevelSelectorComponent)
import Competences.Frontend.Component.Selector.Common (entityPatchLens, selectorTransformedLens)
import Competences.Frontend.Component.Selector.MultiStageSelector (MultiStageSelectorStyle (..))
import Competences.Frontend.View.Component (componentA)
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
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property (checked_, id_, type_)
import Miso.String (ms)
import Optics.Core (Iso', Lens', iso, lens, (&), (%), (.~), (?~), (^.))

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
      modifySyncDocument r $ Tasks (OnSubTasks (CreateAndLock newSubTask))
      where
        emptyOverride = TaskAttributesOverride Nothing Nothing Nothing Nothing

    view' m =
      M.div_
        [class_ "space-y-6"]
        [ viewGroupEditor
        , viewSubTasksSection m
        ]

    viewGroupEditor =
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
      M.div_
        [class_ "border-t pt-4"]
        [ M.div_
            [class_ "flex items-center justify-between mb-4"]
            [ Typography.h3 (C.translate' C.LblSubTasks)
            , Button.buttonSecondary (C.translate' C.LblAddSubTask)
                & Button.withIcon IcnAdd
                & Button.withClick CreateSubTask
                & Button.renderButton
            ]
        , if null m.subTasks
            then Typography.muted (C.translate' C.LblNoSubTasks)
            else M.div_ [class_ "space-y-4"] (map viewSubTaskEditor m.subTasks)
        ]

    -- Each SubTask gets its own Editor component
    viewSubTaskEditor task =
      V.component
        ("subtask-editor-" <> ms (show task.id))
        (TE.editorComponent (subTaskEditor group.id task.id) r)

    subTaskEditorId taskId = "subtask-editor-" <> ms (show taskId)

    -- SubTask editor definition
    subTaskEditor :: TaskGroupId -> TaskId -> TE.Editor Task SubTaskPatch Maybe M.MisoString
    subTaskEditor _groupId taskId =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblEditSubTask)
            id
        )
        (subTaskEditable taskId)
        `TE.addNamedField` ( C.translate' C.LblTaskIdentifier
                           , TE.textEditorField subTaskIdentifierViewLens subTaskIdentifierPatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskContent
                           , TE.textEditorField subTaskContentViewLens subTaskContentPatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskPurposeLabel
                           , TE.enumEditorField
                               translatePurposeOverride
                               subTaskPurposeViewLens
                               subTaskPurposePatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskDisplayInResources
                           , TE.enumEditorField
                               translateDisplayOverride
                               subTaskDisplayInResourcesViewLens
                               subTaskDisplayInResourcesPatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskPrimaryCompetences
                           , competenceOverrideEditorField
                               r
                               (subTaskEditorId taskId <> "-primary")
                               subTaskPrimaryViewLens
                               subTaskPrimaryPatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblTaskSecondaryCompetences
                           , competenceOverrideEditorField
                               r
                               (subTaskEditorId taskId <> "-secondary")
                               subTaskSecondaryViewLens
                               subTaskSecondaryPatchLens
                           )

    translatePurposeOverride :: PurposeOverride -> M.MisoString
    translatePurposeOverride InheritPurpose = C.translate' C.LblInherit
    translatePurposeOverride OverridePractice = C.translate' (C.LblTaskPurpose Practice)
    translatePurposeOverride OverrideAssessment = C.translate' (C.LblTaskPurpose Assessment)

    translateDisplayOverride :: DisplayOverride -> M.MisoString
    translateDisplayOverride InheritDisplay = C.translate' C.LblInherit
    translateDisplayOverride DisplayYes = C.translate' C.LblYes
    translateDisplayOverride DisplayNo = C.translate' C.LblNo

    -- Editable definition for a specific SubTask
    subTaskEditable :: TaskId -> TE.Editable Maybe Task SubTaskPatch
    subTaskEditable taskId =
      TE.editable
        ( \d ->
            fmap
              (\t -> (t, (d ^. #locks) Map.!? TaskLock t.id))
              (Ix.getOne $ d.tasks Ix.@= taskId)
        )
        & (#modify ?~ (\t modify -> Tasks $ OnSubTasks (Modify t.id modify)))
        & (#delete ?~ (\t -> Tasks $ OnSubTasks (Delete t.id)))

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

-- ============================================================================
-- SubTask Lenses
-- ============================================================================

-- SubTask identifier lens (reuses TaskIdentifier iso)
taskIdentifierTextIso :: Iso' TaskIdentifier Text
taskIdentifierTextIso = iso (\(TaskIdentifier t) -> t) TaskIdentifier

changeTaskIdentifierTextIso :: Iso' (Change TaskIdentifier) (Change Text)
changeTaskIdentifierTextIso = iso fwd bwd
  where
    fwd Nothing = Nothing
    fwd (Just (TaskIdentifier a, TaskIdentifier b)) = Just (a, b)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (TaskIdentifier a, TaskIdentifier b)

subTaskIdentifierViewLens :: Lens' Task Text
subTaskIdentifierViewLens = #identifier % taskIdentifierTextIso

subTaskIdentifierPatchLens :: Lens' SubTaskPatch (Change Text)
subTaskIdentifierPatchLens = #identifier % changeTaskIdentifierTextIso

-- SubTask content lens (Maybe Text -> Text)
subTaskContentViewLens :: Lens' Task Text
subTaskContentViewLens = #content % maybeTextIso

subTaskContentPatchLens :: Lens' SubTaskPatch (Change Text)
subTaskContentPatchLens = #content % changeMaybeTextIso

-- ============================================================================
-- Override Enums for SubTask Editors
-- ============================================================================

-- | Purpose override enum: Inherit from group or override with a specific value
data PurposeOverride
  = InheritPurpose
  | OverridePractice
  | OverrideAssessment
  deriving (Bounded, Enum, Eq, Show)

purposeOverrideIso :: Iso' (Maybe TaskPurpose) PurposeOverride
purposeOverrideIso = iso toOverride fromOverride
  where
    toOverride Nothing = InheritPurpose
    toOverride (Just Practice) = OverridePractice
    toOverride (Just Assessment) = OverrideAssessment
    fromOverride InheritPurpose = Nothing
    fromOverride OverridePractice = Just Practice
    fromOverride OverrideAssessment = Just Assessment

changePurposeOverrideIso :: Iso' (Change (Maybe TaskPurpose)) (Change PurposeOverride)
changePurposeOverrideIso = iso fwd bwd
  where
    toO = purposeOverrideIso
    fwd Nothing = Nothing
    fwd (Just (a, b)) = Just (a ^. toO, b ^. toO)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (fromOverride a, fromOverride b)
    fromOverride InheritPurpose = Nothing
    fromOverride OverridePractice = Just Practice
    fromOverride OverrideAssessment = Just Assessment

-- | Display in resources override enum: Inherit from group or override with Yes/No
data DisplayOverride
  = InheritDisplay
  | DisplayYes
  | DisplayNo
  deriving (Bounded, Enum, Eq, Show)

displayOverrideIso :: Iso' (Maybe Bool) DisplayOverride
displayOverrideIso = iso toOverride fromOverride
  where
    toOverride Nothing = InheritDisplay
    toOverride (Just True) = DisplayYes
    toOverride (Just False) = DisplayNo
    fromOverride InheritDisplay = Nothing
    fromOverride DisplayYes = Just True
    fromOverride DisplayNo = Just False

changeDisplayOverrideIso :: Iso' (Change (Maybe Bool)) (Change DisplayOverride)
changeDisplayOverrideIso = iso fwd bwd
  where
    toO = displayOverrideIso
    fwd Nothing = Nothing
    fwd (Just (a, b)) = Just (a ^. toO, b ^. toO)
    bwd Nothing = Nothing
    bwd (Just (a, b)) = Just (fromOverride a, fromOverride b)
    fromOverride InheritDisplay = Nothing
    fromOverride DisplayYes = Just True
    fromOverride DisplayNo = Just False

-- SubTask purpose override lens (via PurposeOverride enum)
subTaskPurposeViewLens :: Lens' Task PurposeOverride
subTaskPurposeViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SubTask _ override -> override.purpose ^. purposeOverrideIso
      SelfContained _ -> InheritPurpose
    setter task newVal = case task.taskType of
      SubTask gid override ->
        let maybeVal = case newVal of
              InheritPurpose -> Nothing
              OverridePractice -> Just Practice
              OverrideAssessment -> Just Assessment
         in task & #taskType .~ SubTask gid (override & #purpose .~ maybeVal)
      SelfContained _ -> task

subTaskPurposePatchLens :: Lens' SubTaskPatch (Change PurposeOverride)
subTaskPurposePatchLens = #purpose % changePurposeOverrideIso

-- SubTask displayInResources override lens (via DisplayOverride enum)
subTaskDisplayInResourcesViewLens :: Lens' Task DisplayOverride
subTaskDisplayInResourcesViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SubTask _ override -> override.displayInResources ^. displayOverrideIso
      SelfContained _ -> InheritDisplay
    setter task newVal = case task.taskType of
      SubTask gid override ->
        let maybeVal = case newVal of
              InheritDisplay -> Nothing
              DisplayYes -> Just True
              DisplayNo -> Just False
         in task & #taskType .~ SubTask gid (override & #displayInResources .~ maybeVal)
      SelfContained _ -> task

subTaskDisplayInResourcesPatchLens :: Lens' SubTaskPatch (Change DisplayOverride)
subTaskDisplayInResourcesPatchLens = #displayInResources % changeDisplayOverrideIso

-- ============================================================================
-- Competence Override Editor Field
-- ============================================================================

-- | Override enum for competences: Inherit or Override
data CompetenceOverrideMode
  = InheritCompetences
  | OverrideCompetences
  deriving (Bounded, Enum, Eq, Show)

-- | Get the override mode from Maybe [CompetenceLevelId]
getOverrideMode :: Maybe [CompetenceLevelId] -> CompetenceOverrideMode
getOverrideMode Nothing = InheritCompetences
getOverrideMode (Just _) = OverrideCompetences

-- | Editor field for Maybe [CompetenceLevelId] with checkbox + selector
competenceOverrideEditorField
  :: SyncDocumentRef
  -> M.MisoString
  -> Lens' Task (Maybe [CompetenceLevelId])
  -> Lens' SubTaskPatch (Change (Maybe [CompetenceLevelId]))
  -> EditorField Task SubTaskPatch Maybe
competenceOverrideEditorField r key viewLens patchLens =
  EditorField
    { viewer = competenceOverrideViewer viewLens
    , editor = competenceOverrideEditor r key viewLens patchLens
    }

competenceOverrideViewer
  :: Lens' Task (Maybe [CompetenceLevelId])
  -> Task
  -> M.View (TE.Model Task SubTaskPatch Maybe) (TE.Action Task SubTaskPatch)
competenceOverrideViewer viewLens task =
  case task ^. viewLens of
    Nothing -> V.text_ (C.translate' C.LblInherit)
    Just [] -> V.text_ (C.translate' C.LblNoCompetences)
    Just competences ->
      M.span_ [] [V.text_ (M.ms $ show (length competences) <> " Kompetenzen")]

competenceOverrideEditor
  :: SyncDocumentRef
  -> M.MisoString
  -> Lens' Task (Maybe [CompetenceLevelId])
  -> Lens' SubTaskPatch (Change (Maybe [CompetenceLevelId]))
  -> Bool
  -> Task
  -> SubTaskPatch
  -> M.View (TE.Model Task SubTaskPatch Maybe) (TE.Action Task SubTaskPatch)
competenceOverrideEditor r key viewLens patchLens refocusTarget original patch =
  M.div_
    [class_ "space-y-2"]
    [ -- Checkbox to toggle override mode
      M.label_
        [class_ "flex items-center gap-2 cursor-pointer"]
        [ M.input_
            ( [ type_ "checkbox"
              , checked_ isOverriding
              , M.onClick toggleOverride
              ]
                <> if refocusTarget then [id_ "refocusTarget"] else []
            )
        , V.text_ (C.translate' C.LblOverrideCompetences)
        ]
    , -- Show selector when overriding
      if isOverriding
        then
          componentA
            (key <> "-selector")
            []
            ( competenceLevelSelectorComponent
                r
                (\_ -> currentCompetences)
                MultiStageSelectorEnabled
                (selectorTransformedLens id id (mkFieldLens wrappedViewLens wrappedPatchLens original))
            )
        else M.div_ [] []
    ]
  where
    -- Current value from patch or original
    currentMaybe :: Maybe [CompetenceLevelId]
    currentMaybe = case patch ^. patchLens of
      Just (_, after) -> after
      Nothing -> original ^. viewLens

    currentCompetences :: [CompetenceLevelId]
    currentCompetences = fromMaybe [] currentMaybe

    isOverriding :: Bool
    isOverriding = getOverrideMode currentMaybe == OverrideCompetences

    -- Toggle between inherit and override
    toggleOverride :: TE.Action Task SubTaskPatch
    toggleOverride =
      let newValue = if isOverriding then Nothing else Just []
       in TE.UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, newValue))

    -- Wrapped lenses that work with the list inside Maybe
    wrappedViewLens :: Lens' Task [CompetenceLevelId]
    wrappedViewLens = lens getter setter
      where
        getter t = fromMaybe [] (t ^. viewLens)
        setter t v = t & viewLens .~ Just v

    wrappedPatchLens :: Lens' SubTaskPatch (Change [CompetenceLevelId])
    wrappedPatchLens = lens getter setter
      where
        getter p = case p ^. patchLens of
          Nothing -> Nothing
          Just (before, after) -> Just (fromMaybe [] before, fromMaybe [] after)
        setter p Nothing = p & patchLens .~ Nothing
        setter p (Just (before, after)) = p & patchLens ?~ (Just before, Just after)

-- SubTask primary competences override lens
subTaskPrimaryViewLens :: Lens' Task (Maybe [CompetenceLevelId])
subTaskPrimaryViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SubTask _ override -> override.primary
      SelfContained _ -> Nothing
    setter task newVal = case task.taskType of
      SubTask gid override ->
        task & #taskType .~ SubTask gid (override & #primary .~ newVal)
      SelfContained _ -> task

subTaskPrimaryPatchLens :: Lens' SubTaskPatch (Change (Maybe [CompetenceLevelId]))
subTaskPrimaryPatchLens = #primary

-- SubTask secondary competences override lens
subTaskSecondaryViewLens :: Lens' Task (Maybe [CompetenceLevelId])
subTaskSecondaryViewLens = lens getter setter
  where
    getter task = case task.taskType of
      SubTask _ override -> override.secondary
      SelfContained _ -> Nothing
    setter task newVal = case task.taskType of
      SubTask gid override ->
        task & #taskType .~ SubTask gid (override & #secondary .~ newVal)
      SelfContained _ -> task

subTaskSecondaryPatchLens :: Lens' SubTaskPatch (Change (Maybe [CompetenceLevelId]))
subTaskSecondaryPatchLens = #secondary
