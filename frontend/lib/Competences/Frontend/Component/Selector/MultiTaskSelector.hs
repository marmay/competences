module Competences.Frontend.Component.Selector.MultiTaskSelector
  ( multiTaskEditorField
  , searchableMultiTaskEditorField
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Task (..))
import Competences.Document.Task (TaskId, TaskIdentifier (..))
import Data.Proxy (Proxy (..))
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), SelectorTransformedLens (..), mkSelectorBinding)
import Competences.Frontend.Component.Selector.ListSelector qualified as L
import Competences.Frontend.Component.Selector.SearchableListSelector qualified as SL
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncContext, isInitialUpdate, subscribeDocument)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (Default)
import Data.Foldable (toList)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~), (^.))
import Optics.Core qualified as O

-- ============================================================================
-- TASK SELECTOR CONFIG
-- ============================================================================

-- | Get all tasks from the document (both self-contained and subtasks), sorted by identifier
listAllTasks :: Document -> [Task]
listAllTasks d = Ix.toAscList (Proxy @TaskIdentifier) d.tasks

-- | Show task identifier for display
showTask :: Task -> M.MisoString
showTask t =
  let TaskIdentifier ident = t.identifier
   in M.ms ident

-- | Create config for task selector
-- Note: showSelectAll is disabled because selecting all tasks doesn't make sense
toListSelectorConfig :: (Task -> Bool) -> L.ListSelectorConfig Task f
toListSelectorConfig isInitial =
  (L.listSelectorConfig listAllTasks showTask)
    { L.isInitialValue = isInitial
    , L.showSelectAll = False
    }

-- ============================================================================
-- SEARCHABLE MULTI-TASK EDITOR FIELD
-- ============================================================================

-- | Searchable multi-task selector component
searchableMultiTaskSelectorComponent
  :: SyncContext
  -> (Task -> Bool)  -- ^ Is task initially selected?
  -> SelectorTransformedLens p [] Task f t
  -> M.Component p (SL.SearchableModel Task) (SL.SearchableAction Task)
searchableMultiTaskSelectorComponent r isInitial =
  SL.searchableMultiSelectorComponent r (toListSelectorConfig isInitial)

-- | Searchable multi-task editor field for use in editors
-- Uses a read-only viewer (comma-separated identifiers with count) and searchable combobox for editing
searchableMultiTaskEditorField
  :: (Default patch, Ord entity)
  => SyncContext
  -> M.MisoString
  -> EntityPatchTransformedLens entity patch [] TaskId [] TaskId
  -> EditorField entity patch f'
searchableMultiTaskEditorField r k eptl =
  let config e =
        let initialTaskIds = Set.fromList (toList $ e ^. eptl.viewLens)
         in \task -> task.id `Set.member` initialTaskIds
   in selectorEditorFieldWithViewer
        k
        (taskIdToTaskLens eptl)
        (\e -> selectedTasksViewerComponent r (config e))
        (\e -> searchableMultiTaskSelectorComponent r (config e))

-- | Transform a TaskId lens to a Task lens for use with selectors
taskIdToTaskLens
  :: EntityPatchTransformedLens entity patch [] TaskId [] TaskId
  -> EntityPatchTransformedLens entity patch [] Task [] TaskId
taskIdToTaskLens eptl =
  EntityPatchTransformedLens
    { viewLens = eptl.viewLens
    , patchLens = eptl.patchLens
    , transform = (.id)
    , embed = id
    }

-- ============================================================================
-- SELECTED TASKS VIEWER (Read-only display)
-- ============================================================================

-- | Model for the selected tasks viewer
data SelectedTasksViewerModel = SelectedTasksViewerModel
  { possibleValues :: ![Task]
  , selectedValues :: ![Task]
  }
  deriving (Eq, Generic, Show)

-- | Action for the selected tasks viewer
newtype SelectedTasksViewerAction = ViewerUpdateDocument DocumentChange
  deriving (Eq, Show)

-- | Component that displays selected tasks as comma-separated text with count
-- Used as the viewer in editor fields (read-only display)
selectedTasksViewerComponent
  :: SyncContext
  -> (Task -> Bool)
  -> SelectorTransformedLens p [] Task f t
  -> M.Component p SelectedTasksViewerModel SelectedTasksViewerAction
selectedTasksViewerComponent r isInitial lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding (O.castOptic #selectedValues)]
    , M.subs = [subscribeDocument r ViewerUpdateDocument]
    }
  where
    model =
      SelectedTasksViewerModel
        { possibleValues = []
        , selectedValues = []
        }

    update (ViewerUpdateDocument (DocumentChange d info)) =
      M.modify $ \m ->
        let listSelectorCfg = toListSelectorConfig isInitial
            newPossibleValues = listSelectorCfg.listValues d
            newSelectedValues =
              if isInitialUpdate info
                then filter listSelectorCfg.isInitialValue newPossibleValues
                else filter (`Set.member` Set.fromList newPossibleValues) m.selectedValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValues .~ newSelectedValues)

    view m = viewSelectedTasks m.selectedValues

-- | Render a list of tasks as comma-separated identifiers with count in brackets
-- Empty list shows translated placeholder text
viewSelectedTasks :: [Task] -> M.View m a
viewSelectedTasks tasks =
  case tasks of
    [] -> Typography.muted (C.translate' C.LblNoTasksSelected)
    _ ->
      let names = Text.intercalate ", " (map taskIdentifierText tasks)
          count = Text.pack $ " (" <> show (length tasks) <> ")"
       in MH.span_ [] [M.text (M.ms $ names <> count)]
  where
    taskIdentifierText t = let TaskIdentifier ident = t.identifier in ident

-- ============================================================================
-- LEGACY (for backwards compatibility during migration)
-- ============================================================================

-- | Legacy editor field - now delegates to searchable version
multiTaskEditorField
  :: (Default patch, Ord entity)
  => SyncContext
  -> M.MisoString
  -> EntityPatchTransformedLens entity patch [] TaskId [] TaskId
  -> EditorField entity patch f
multiTaskEditorField = searchableMultiTaskEditorField
