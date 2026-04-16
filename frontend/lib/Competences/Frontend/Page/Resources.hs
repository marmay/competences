-- | Resource page: sidebar selector + detail view.
-- The detail pane mounts 'resourceDetailedComponent' so resources render
-- identically on this page and elsewhere.
module Competences.Frontend.Page.Resources
  ( resourcesPage
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Resource (..))
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Resource.Detailed
  ( ResourceDetailedConfig (..)
  , ResourceDetailedSettings (..)
  , defaultResourceDetailedSettings
  , resourceDetailedComponent
  )
import Competences.Frontend.Component.Selector.ResourceSelector (resourceSelectorComponent)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Router qualified as M
import Miso.String (ms)

data Model = Model
  { selected :: !(Maybe Resource)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleSidebar
  deriving (Eq, Show)

resourcesPage :: SyncContext -> Maybe ResourceId -> M.Component p Model Action
resourcesPage r mResId =
  M.component model update view'
  where
    model = Model Nothing True

    selectionFn = fmap (\rid allRes -> Ix.getOne (allRes Ix.@= rid)) mResId
    onSelect = Just (\res -> M.pushURI (M.toURI (ManageResources (Just res.id))))

    update ToggleSidebar = M.modify $ \m -> m {sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        (inlineComponentAttrs "resource-selector" [class_ "h-full"] $ resourceSelectorComponent r selectionFn onSelect #selected)
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just resource) =
      let adminSettings = defaultResourceDetailedSettings {enableGoTo = False, enableDelete = True}
       in inlineComponent
            ("resource-detail-" <> ms (show resource.id))
            (resourceDetailedComponent r (ResourceDetailedConfig resource.id adminSettings))
