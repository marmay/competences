module Competences.Frontend.Page.Assignments
  ( assignmentsPage
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), User (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.WithOrigin (WithOrigin (..))
import Competences.Frontend.Component.Assignment.Detailed (RenderStyle (..), viewerComponent)
import Competences.Frontend.Component.Assignment.ListSelector (assignmentListSelectorComponent)
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.SyncContext (SyncContext (..), SyncDocumentEnv (..), syncDocumentEnv)
import Competences.Frontend.SyncContext.WindowManager (inlineComponentAttrs, inlineComponentWith)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.DefaultSelection qualified as QDefault
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)

data Model = Model
  { selected :: !(Maybe (WithOrigin Assignment))
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

    -- Look up the deep-linked assignment in the projection's IxSet
    -- (which contains @WithOrigin Assignment@ values).
    initialPickFn = case mAssignmentId of
      Just aid ->
        Just $ \xs -> Ix.getOne (xs Ix.@= aid)
      Nothing ->
        Just $ \xs ->
          let today = (syncDocumentEnv r).currentDay
              published = [w.value | w <- Ix.toList xs, w.origin == Published]
              ixs = Ix.fromList published
           in case QDefault.defaultAssignment today ixs of
                Just a ->
                  Ix.getOne (xs Ix.@= a.id)
                Nothing -> Nothing

    update ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        ( inlineComponentAttrs "assignment-selector" [class_ "h-full"] $
            assignmentListSelectorComponent r initialPickFn #selected
        )
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just w) =
      inlineComponentWith
        ("assignment-detail-" <> ms (show w.value.id))
        (viewerComponent r user w.value Standalone)
