-- | Wrapper around SearchSelect that adds an "Add Task" button for creating
-- new tasks inline from the assignment editor. When "Add Task" is clicked,
-- a new empty task is created (matching the assignment's origin), added to
-- the selection, and opened in a modal editor.
module Competences.Frontend.Component.Assignment.TaskSearchSelectWithAdd
  ( taskSearchSelectWithAddEditorField
  )
where

import Competences.Command (Command (..), EntityCommand (..), TasksCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..), TaskType (..))
import Competences.Document.Id (idToText)
import Competences.Document.Task (TaskId, TaskIdentifier (..), defaultTaskAttributes)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), retargetForDraft)
import Competences.Frontend.Component.Editor.EditorField (EditorField (..), mkFieldLens)
import Competences.Frontend.Component.Editor.View (refocusTargetString)
import Competences.Frontend.Component.Selector.Common
  ( EntityPatchTransformedLens (..)
  , SelectorTransformedLens
  , mkSelectorBinding
  , selectorTransformedLens
  )
import Competences.Frontend.Component.Selector.SearchSelect
  ( SearchSelectConfig
  , searchSelectComponent
  , searchSelectViewerComponent
  )
import Competences.Frontend.Component.TaskEditor.TaskDetailView (taskDetailView)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , ModalConfig (..)
  , ModalHeight (..)
  , ModalId (..)
  , ModalWidth (..)
  , SyncContext (..)
  , WindowChrome (..)
  , WindowMode
  , closeWindow
  , inlineComponent
  , inlineComponentAttrs
  , modifySyncDocument
  , nextId
  , openFramedModalWith
  , subscribeDocument
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)
import Optics.Core ((&), (.~))
import Optics.Core qualified as O

-- ============================================================================
-- EditorField factory
-- ============================================================================

-- | Create an EditorField that wraps SearchSelect with an "Add Task" button.
-- The viewer uses the standard SearchSelect viewer (no add button).
-- The editor embeds SearchSelect plus an "Add Task" button that creates a new
-- task and opens a modal editor for it.
taskSearchSelectWithAddEditorField
  :: (Ord entity, Default patch)
  => SyncContext
  -> MisoString
  -> EntityOrigin
  -> SearchSelectConfig Task TaskId
  -> (entity -> [TaskId])
  -> EntityPatchTransformedLens entity patch [] Task [] TaskId
  -> EditorField entity patch ef
taskSearchSelectWithAddEditorField r k origin cfg extractIds eptl =
  let mkLens = mkFieldLens eptl.viewLens eptl.patchLens
      l' a = selectorTransformedLens eptl.transform eptl.embed (mkLens a)
   in EditorField
        { viewer = \a ->
            inlineComponent (k <> "-viewer")
              (searchSelectViewerComponent r cfg (extractIds a) (l' a))
        , editor = \refocusTarget a _ ->
            inlineComponentAttrs (k <> "-editor") (refocusTargetAttr refocusTarget)
              (wrapperComponent r k origin cfg (extractIds a) (l' a))
        }

refocusTargetAttr :: Bool -> [M.Attribute action]
refocusTargetAttr True = [MP.id_ refocusTargetString]
refocusTargetAttr False = []

-- ============================================================================
-- Wrapper component: SearchSelect + Add button
-- ============================================================================

newtype WrapperModel = WrapperModel
  { selectedIds :: [TaskId]
  }
  deriving (Eq, Generic, Show)

data WrapperAction
  = AddNewTask
  deriving (Eq, Show)

-- | Component wrapping SearchSelect with an "Add Task" button.
-- The binding syncs selectedIds with the parent editor model via stub Tasks.
-- The SearchSelect child resolves actual Task objects from the document.
wrapperComponent
  :: forall p
   . SyncContext
  -> MisoString
  -> EntityOrigin
  -> SearchSelectConfig Task TaskId
  -> [TaskId]
  -> SelectorTransformedLens p [] Task [] TaskId
  -> M.Component p WrapperModel WrapperAction
wrapperComponent r k origin cfg initIds parentBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding parentBinding wrapperEntitiesLens]
    }
  where
    model = WrapperModel {selectedIds = initIds}

    -- Virtual lens: expose selectedIds as [Task] for the parent binding.
    -- The binding only reads .id from each Task (via transform), so stub
    -- tasks with just the ID are sufficient.
    wrapperEntitiesLens :: O.Lens' WrapperModel [Task]
    wrapperEntitiesLens = O.lens getter setter
      where
        getter m = map mkStubTask m.selectedIds
        setter m newTasks = m & #selectedIds .~ map (.id) newTasks

    mkStubTask :: TaskId -> Task
    mkStubTask tid = Task
      { id = tid
      , identifier = TaskIdentifier ""
      , content = Nothing
      , taskType = SelfContained defaultTaskAttributes
      , attachments = []
      }

    update AddNewTask = M.io_ $ do
      taskId <- nextId r
      let newTask = Task
            { id = taskId
            , identifier = TaskIdentifier ""
            , content = Nothing
            , taskType = SelfContained defaultTaskAttributes
            , attachments = []
            }
          wrap = case origin of
            Published -> id
            Draft -> retargetForDraft
      -- Create the task (not locked - the inner editor handles lock/release)
      modifySyncDocument r $ wrap $ Tasks (OnTasks (Create newTask))
      -- Open modal editor
      openTaskEditorModal r origin taskId

    view m =
      MH.div_
        [class_ "space-y-2"]
        [ inlineComponent (k <> "-search-select")
            (searchSelectComponent r cfg m.selectedIds childBinding)
        , Button.secondarySm $ Button.button (Icon.IcnAdd, C.LblAddTask) AddNewTask
        ]

    -- Child binding: SearchSelect writes [Task], we store [TaskId]
    childBinding :: SelectorTransformedLens WrapperModel [] Task [] TaskId
    childBinding = selectorTransformedLens (.id) id #selectedIds

-- ============================================================================
-- Task editor modal
-- ============================================================================

newtype ModalModel = ModalModel
  { saved :: Bool
  }
  deriving (Eq, Generic, Show)

data ModalAction
  = ModalDocumentChanged !DocumentChange
  | CloseAndDelete
  | CloseAndKeep
  deriving (Eq, Show)

-- | Open a modal containing the task detail editor.
openTaskEditorModal :: SyncContext -> EntityOrigin -> TaskId -> IO ()
openTaskEditorModal r origin taskId =
  let cfg = ModalConfig
        { chrome = WindowChrome (C.translate' C.LblEditSelfContainedTask) Icon.IcnTask
        , modalId = ModalId ("new-task-" <> idToText taskId)
        , width = ModalWide
        , height = ModalFull
        , pinnable = Nothing
        }
   in openFramedModalWith r.windowManager cfg (taskEditorModalComponent r origin taskId)

taskEditorModalComponent
  :: SyncContext
  -> EntityOrigin
  -> TaskId
  -> WindowMode
  -> M.Component p ModalModel ModalAction
taskEditorModalComponent r origin taskId wm =
  (M.component model update view)
    { M.subs = [subscribeDocument r ModalDocumentChanged]
    }
  where
    model = ModalModel {saved = False}

    wrap = case origin of
      Published -> id
      Draft -> retargetForDraft

    update (ModalDocumentChanged dc) = do
      -- Track whether the task has been saved with a non-empty identifier
      let doc = dc.document
          mTask = case origin of
            Published -> Ix.getOne $ doc.tasks Ix.@= taskId
            Draft -> Ix.getOne $ doc.draftTasks Ix.@= taskId
      case mTask of
        Just t | let TaskIdentifier ident = t.identifier, ident /= "" ->
          M.modify $ #saved .~ True
        _ -> pure ()

    update CloseAndDelete = M.io_ $ do
      modifySyncDocument r $ wrap $ Tasks (OnTasks (Delete taskId))
      closeWindow wm

    update CloseAndKeep = M.io_ $ closeWindow wm

    -- Stub task for taskDetailView (it uses task.id and task.taskType for routing)
    stubTask = Task
      { id = taskId
      , identifier = TaskIdentifier ""
      , content = Nothing
      , taskType = SelfContained defaultTaskAttributes
      , attachments = []
      }

    view m =
      MH.div_
        [class_ "flex flex-col h-full"]
        [ MH.div_
            [class_ "flex-1 overflow-y-auto p-4"]
            [taskDetailView r origin stubTask]
        , MH.div_
            [class_ "flex justify-end gap-2 p-4 border-t border-border"]
            [ Button.destructiveSm $ Button.button (Icon.IcnCancel, C.LblDelete) CloseAndDelete
            , if m.saved
                then Button.primarySm $ Button.button (Icon.IcnApply, C.LblApply) CloseAndKeep
                else Button.outlineSm $ Button.button (Icon.IcnApply, C.LblApply) CloseAndKeep
            ]
        ]
