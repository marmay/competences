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
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Typography qualified as Typography
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
taskEditorComponent :: SyncDocumentRef -> M.Component p Model Action
taskEditorComponent r =
  M.component model update view'
  where
    model = Model Nothing

    update NoOp = pure ()

    view' m =
      V.sideMenu
        (V.component "task-or-group-selector" $ taskOrGroupSelectorComponent r #selected)
        (detailView m.selected)

    detailView Nothing =
      Typography.muted (C.translate' C.LblPleaseSelectItem)
    detailView (Just (SelectableTask task)) =
      taskDetailView r task
    detailView (Just (SelectableGroup group)) =
      taskGroupDetailView r group
