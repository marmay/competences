-- | Resource page: sidebar selector + detail view.
-- The detail pane mounts 'resourceDetailedComponent' so resources render
-- identically on this page and elsewhere.
module Competences.Frontend.Component.ResourceEditor
  ( resourceEditorComponent
  )
where

import Competences.Document (Resource (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Resource.Detailed
  ( ResourceDetailedConfig (..)
  , defaultResourceDetailedSettings
  , resourceDetailedComponent
  )
import Competences.Frontend.Component.Selector.ResourceSelector (resourceSelectorComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)

data Model = Model
  { selected :: !(Maybe Resource)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleSidebar
  deriving (Eq, Show)

resourceEditorComponent :: SyncContext -> M.Component p Model Action
resourceEditorComponent r =
  M.component model update view'
  where
    model = Model Nothing True

    update ToggleSidebar = M.modify $ \m -> m {sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        (inlineComponentAttrs "resource-selector" [class_ "h-full"] $ resourceSelectorComponent r #selected)
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just resource) =
      inlineComponent
        ("resource-detail-" <> ms (show resource.id))
        (resourceDetailedComponent r (ResourceDetailedConfig resource.id defaultResourceDetailedSettings))
