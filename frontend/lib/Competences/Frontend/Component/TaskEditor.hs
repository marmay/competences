module Competences.Frontend.Component.TaskEditor
  ( taskEditorComponent
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.TaskOrGroupSelector
  ( TaskOrGroup (..)
  , taskOrGroupSelectorComponent
  )
import Competences.Frontend.Component.TaskEditor.TaskDetailView (taskDetailView)
import Competences.Frontend.Component.TaskEditor.TaskGroupDetailView (taskGroupDetailView)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M

-- | Model for the unified task editor
data Model = Model
  { selected :: !(Maybe TaskOrGroup)
  }
  deriving (Eq, Generic, Show)

-- | Action for the unified task editor (minimal - selection is handled via bindings)
data Action
  = NoOp
  deriving (Eq, Show)

-- | Unified task editor component
-- Shows Tasks and TaskGroups in a single selector, with type-appropriate detail views
taskEditorComponent :: SyncContext -> M.Component p Model Action
taskEditorComponent r =
  M.component model update view'
  where
    model = Model Nothing

    update NoOp = pure ()

    view' m =
      Layout.sideMenu
        (inlineComponentAttrs "task-or-group-selector" [class_ "h-full"] $ taskOrGroupSelectorComponent r #selected)
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just (SelectableTask task)) =
      taskDetailView r task
    detailView (Just (SelectableGroup group)) =
      taskGroupDetailView r group
