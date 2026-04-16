module Competences.Frontend.Component.Assignment.EditorDetail
  ( editorDetailView
  )
where

import Competences.Command (AssignmentPatch (..), AssignmentsCommand (..), Command (..), EntityCommand (..), PublishData (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Document (..)
  , Lock (..)
  , User (..)
  , emptyDocument
  , lockOwner
  )
import Competences.Document.Task (Task (..), TaskId, TaskIdentifier (..), defaultTask, taskDisplayName)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.EvaluatorDetail (pinAssignmentEvaluator)
import Competences.Frontend.Component.Assignment.PinEditor (nameViewLens, namePatchLens, userSearchConfig)
import Competences.Frontend.Component.Draft (EntityOrigin (..), retargetForDraft)
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.ExportButton (exportButtonComponent)
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.SearchSelect (SearchSelectConfig (..), SelectionOrder (..), TagLayout (..))
import Competences.Frontend.Component.Assignment.TaskSearchSelectWithAdd (openTaskEditorModal)
import Competences.Frontend.Component.RenumberModal (RenumberTaskInfo (..), openRenumberModal)
import Competences.Frontend.Component.Selector.SearchSelectEditorField (searchSelectEditorField)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Import.Export (exportAssignment)
import Competences.Query.Task (getTaskOrDraft)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Data.Default (def)
import Optics.Core ((&), (?~), (.~))

-- ============================================================================
-- Wrapper Model and Actions
-- ============================================================================

data EditorModel = EditorModel
  { document :: !Document
  , origin :: !EntityOrigin
  }
  deriving (Eq, Generic, Show)

data EditorAction
  = DocumentUpdated !DocumentChange
  | PinEvaluation
  | PublishAssignment
  | OpenRenumberModal
  deriving (Eq, Show)

-- | Detail view for editing an assignment
-- The mode type parameter allows this to work with any mode type
editorDetailView
  :: SyncContext
  -> Assignment
  -> M.View (SD.Model Assignment mode) (SD.Action mode)
editorDetailView r assignment =
  inlineComponent
    ("assignment-editor-wrapper-" <> M.ms (show assignment.id))
    (editorWrapperComponent r assignment)

editorWrapperComponent :: SyncContext -> Assignment -> M.Component p EditorModel EditorAction
editorWrapperComponent r assignment =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel = EditorModel {document = emptyDoc, origin = Published}

    emptyDoc = emptyDocument

    -- Determine origin from document: if assignment.id is in draftAssignments, it's a Draft
    detectOrigin :: Document -> EntityOrigin
    detectOrigin doc = case Ix.getOne (doc.draftAssignments Ix.@= assignment.id) of
      Just _ -> Draft
      Nothing -> Published

    update (DocumentUpdated dc) = M.modify $ \m ->
      m & #document .~ dc.document
        & #origin .~ detectOrigin dc.document

    update PinEvaluation = M.io_ $ pinAssignmentEvaluator r assignment

    update OpenRenumberModal = do
      m <- M.get
      M.io_ $ do
        let doc = m.document
            mAssignment = case m.origin of
              Published -> Ix.getOne (doc.assignments Ix.@= assignment.id)
              Draft -> Ix.getOne (doc.draftAssignments Ix.@= assignment.id)
            taskIds = maybe [] (.tasks) mAssignment
            infos =
              [ RenumberTaskInfo
                  { taskId = t.id
                  , identifier = t.identifier
                  , title = t.title
                  , isMultiAssignment = Ix.size (doc.assignments Ix.@= tid) + Ix.size (doc.draftAssignments Ix.@= tid) > 1
                  , origin = if isJust (Ix.getOne (doc.draftTasks Ix.@= tid)) then Draft else Published
                  }
              | tid <- taskIds
              , Just t <- [getTaskOrDraft doc tid]
              ]
        openRenumberModal r infos

    update PublishAssignment = do
      m <- M.get
      M.io_ $ do
        let doc = m.document
            -- Get the current assignment from draft collection
            mDraftAssignment = Ix.getOne (doc.draftAssignments Ix.@= assignment.id)
            -- Get all draft tasks referenced by this assignment
            draftTaskIds = maybe [] (.tasks) mDraftAssignment
            draftTasks = [t | t <- Ix.toList doc.draftTasks, t.id `elem` draftTaskIds]
        modifySyncDocument r $ Publish PublishData
          { tasks = draftTasks
          , assignment = mDraftAssignment
          }

    view m =
      Layout.vFlow Layout.gapM
        [ inlineComponent
            ("assignment-editor-" <> M.ms (show assignment.id) <> "-" <> M.ms (show m.origin))
            (TE.editorComponent (assignmentEditor m.origin) r def)
        , MH.div_
            [class_ "flex justify-end gap-2"]
            ( [ Button.outline $ Button.button (Icon.IcnReorder, C.LblRenumberTasks) OpenRenumberModal
              , Button.outline $ Button.button (Icon.IcnApply, C.LblEvaluateAssignment) PinEvaluation
              , inlineComponent
                  ("export-btn-" <> M.ms (show assignment.id))
                  (exportButtonComponent (\m' -> exportAssignment (m.origin == Draft) m'.document assignment))
              ]
              <> [ Button.primary $ Button.button (Icon.IcnApply, C.LblPublishAssignment) PublishAssignment
                 | m.origin == Draft
                 ]
            )
        ]

    assignmentEditorId = "assignment-editor-" <> M.ms (show assignment.id)

    assignmentEditable origin' =
      let wrap = case origin' of
            Published -> id
            Draft -> retargetForDraft
       in TE.editable
            ( \d ->
                let mAssignment = case origin' of
                      Published -> Ix.getOne $ d.assignments Ix.@= assignment.id
                      Draft -> Ix.getOne $ d.draftAssignments Ix.@= assignment.id
                 in fmap (\c -> (c, lockOwner (AssignmentLock c.id) d)) mAssignment
            )
            & (#modify ?~ (\a modify -> wrap $ Assignments $ OnAssignments (Modify a.id modify)))
            & (#delete ?~ (\a -> wrap $ Assignments $ OnAssignments (Delete a.id)))

    assignmentEditor origin' =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblEditAssignment)
            id
        )
        (assignmentEditable origin')
        `TE.addNamedField` ( C.translate' C.LblAssignmentName
                           , TE.textEditorField nameViewLens namePatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblAssignmentDescription
                           , TE.richTextEditorField r.formulaCache "description" #description #description
                           )
        `TE.addNamedField` ( C.translate' C.LblAssignmentDate
                           , TE.dayEditorField #assignmentDate #assignmentDate
                           )
        `TE.addNamedField` ( C.translate' C.LblActivityType
                           , TE.enumEditorField
                               (C.translate' . C.LblActivityTypeDescription)
                               #activityType
                               #activityType
                           )
        `TE.addNamedField` ( C.translate' C.LblStudents
                           , searchSelectEditorField
                               r
                               (assignmentEditorId <> "-users")
                               userSearchConfig
                               (Set.toList . (.studentIds))
                               (entityPatchTransformedLens #studentIds #studentIds (.id) Set.fromList)
                           )
        `TE.addNamedField` ( C.translate' C.LblAssignmentTasks
                           , searchSelectEditorField
                               r
                               (assignmentEditorId <> "-tasks")
                               (taskSearchConfig r origin')
                               (.tasks)
                               (entityPatchTransformedLens #tasks #tasks (.id) id)
                           )
        `TE.addNamedField` ( C.translate' C.LblGroupSubmissionAllowed
                           , TE.boolEditorField #groupSubmissionAllowed #groupSubmissionAllowed
                           )


-- ============================================================================
-- SearchSelect configs
-- ============================================================================

taskSearchConfig :: SyncContext -> EntityOrigin -> SearchSelectConfig Task TaskId
taskSearchConfig r origin =
  SearchSelectConfig
    { projectItems = \doc ->
        let real = Ix.toAscList (Proxy @TaskIdentifier) doc.tasks
            draft = Ix.toAscList (Proxy @TaskIdentifier) doc.draftTasks
         in case origin of
              Published -> real
              Draft -> sortOn (\t -> let TaskIdentifier x = t.identifier in x) (real <> draft)
    , itemId = (.id)
    , itemLabel = taskDisplayName
    , metaFilters = []
    , viewTag = \t -> (Icon.IcnTask, M.ms $ taskDisplayName t)
    , placeholder = M.fromMisoString $ C.translate' C.LblSelectTasks
    , selectionOrder = ManualReorder
    , tagLayout = TagsInline
    , onCreate = Just $ do
        taskId <- nextId r
        let newTask = defaultTask taskId
            wrap = case origin of
              Published -> id
              Draft -> retargetForDraft
        modifySyncDocument r $ wrap $ Tasks (OnTasks (CreateAndLock newTask))
        openTaskEditorModal r origin taskId
        pure taskId
    }

