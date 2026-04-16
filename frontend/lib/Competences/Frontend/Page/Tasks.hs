module Competences.Frontend.Page.Tasks
  ( tasksPage
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Task (..))
import Competences.Document.Task (TaskIxs)
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Selector.TaskSelector
  ( SelectedTask (..)
  , taskSelectorComponent
  )
import Competences.Frontend.Component.TaskEditor.TaskDetailView (taskDetailView)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.DefaultSelection qualified as QDefault
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Router qualified as M

-- | Model for the unified task editor
data Model = Model
  { selected :: !(Maybe SelectedTask)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- | Action for the unified task editor
data Action
  = ToggleSidebar
  deriving (Eq, Show)

-- | Unified task editor component.
-- When a 'TaskId' is provided (from URL deep link), that task is selected
-- on initial load. Otherwise falls back to smart default (first by identifier).
tasksPage :: SyncContext -> Maybe TaskId -> M.Component p Model Action
tasksPage r mTaskId =
  M.component model update view'
  where
    model = Model Nothing True

    update ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

    onSelect = Just (\st -> M.pushURI (M.toURI (ManageTasks (Just st.task.id))))

    selectionFn = Just $ \tasks draftIds ->
      case mTaskId of
        Just tid ->
          -- Deep link: select the specific task
          case Ix.getOne (tasks Ix.@= tid) of
            Just t -> Just (mkSelected t draftIds)
            Nothing -> smartDefault tasks draftIds
        Nothing -> smartDefault tasks draftIds

    smartDefault :: Ix.IxSet TaskIxs Task -> Set TaskId -> Maybe SelectedTask
    smartDefault tasks draftIds =
      mkSelected <$> QDefault.defaultTask tasks <*> pure draftIds

    mkSelected :: Task -> Set TaskId -> SelectedTask
    mkSelected t draftIds =
      SelectedTask
        (if Set.member t.id draftIds then Draft else Published)
        t

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        (inlineComponentAttrs "task-selector" [class_ "h-full"] $ taskSelectorComponent r selectionFn onSelect #selected)
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just st) =
      taskDetailView r st.origin st.task
