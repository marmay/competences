module Competences.Frontend.Component.ResourceEditor
  ( resourceEditorComponent
  )
where

import Competences.Document (Resource (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Resource.EditorDetail (editorDetailView)
import Competences.Frontend.Component.Selector.ResourceSelector (resourceSelectorComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Component (componentA)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import GHC.Generics (Generic)
import Miso qualified as M

-- | Model for the resource editor page
data Model = Model
  { selected :: !(Maybe Resource)
  }
  deriving (Eq, Generic, Show)

-- | Action for the resource editor page
data Action
  = NoOp
  deriving (Eq, Show)

-- | Resource editor page component
resourceEditorComponent :: SyncContext -> M.Component p Model Action
resourceEditorComponent r =
  M.component model update view'
  where
    model = Model Nothing

    update NoOp = pure ()

    view' m =
      Layout.sideMenu
        (componentA "resource-selector" [class_ "h-full"] $ resourceSelectorComponent r #selected)
        (detailView m.selected)

    detailView Nothing =
      Typography.muted (C.translate' C.LblPleaseSelectItem)
    detailView (Just resource) =
      editorDetailView r resource
