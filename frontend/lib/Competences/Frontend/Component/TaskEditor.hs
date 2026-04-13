module Competences.Frontend.Component.TaskEditor
  ( taskEditorComponent
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.TaskSelector
  ( SelectedTask (..)
  , taskSelectorComponent
  )
import Competences.Frontend.Component.TaskEditor.TaskDetailView (taskDetailView)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M

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

-- | Unified task editor component
-- Shows Tasks in a selector, with detail view for editing
taskEditorComponent :: SyncContext -> M.Component p Model Action
taskEditorComponent r =
  M.component model update view'
  where
    model = Model Nothing True

    update ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        (inlineComponentAttrs "task-selector" [class_ "h-full"] $ taskSelectorComponent r #selected)
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just st) =
      taskDetailView r st.origin st.task
