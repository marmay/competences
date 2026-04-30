module Competences.Frontend.Page.Assignments
  ( assignmentsPage
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), User (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.Detailed (RenderStyle (..), viewerComponent)
import Competences.Frontend.Component.Selector.AssignmentSelector (assignmentSelectorComponent)
import Competences.Frontend.SyncContext (SyncContext (..), SyncDocumentEnv (..))
import Competences.Frontend.SyncContext.WindowManager (inlineComponentAttrs, inlineComponentWith)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.DefaultSelection qualified as QDefault
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)

data Model = Model
  { selected :: !(Maybe Assignment)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleSidebar
  deriving (Eq, Show)

assignmentsPage
  :: SyncContext
  -> User
  -> Maybe AssignmentId
  -> M.Component p Model Action
assignmentsPage r user mAssignmentId =
  M.component model update view'
  where
    model = Model Nothing True

    selectionFn = case mAssignmentId of
      Just aid -> Just (\allAssignments -> Ix.getOne (allAssignments Ix.@= aid))
      Nothing -> Just (QDefault.defaultAssignment r.env.currentDay)

    update ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        ( inlineComponentAttrs "assignment-selector" [class_ "h-full"] $
            assignmentSelectorComponent r selectionFn #selected
        )
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just assignment) =
      inlineComponentWith
        ("assignment-detail-" <> ms (show assignment.id))
        (viewerComponent r user assignment Standalone)
