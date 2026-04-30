-- | CompetenceGrid page: list selector on the left, viewer on the
-- right. Alternate views (Edit / Assess / Grade) are opened as pins
-- via the viewer's EntityMenu, mirroring the pattern used by
-- Tasks/Resources/Assignments.
module Competences.Frontend.Page.CompetenceGrid
  ( competenceGridPage
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (CompetenceGrid (..))
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.User (isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.ListSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridListSelectorComponent
  )
import Competences.Frontend.Component.CompetenceGrid.Viewer (viewerDetailView)
import Competences.Frontend.SyncContext (SyncContext, SyncDocumentEnv (..), syncDocumentEnv)
import Competences.Frontend.SyncContext.WindowManager (inlineComponentAttrs)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.DefaultSelection qualified as QDefault
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH

data Model = Model
  { selected :: !(Maybe CompetenceGrid)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleSidebar
  deriving (Eq, Show)

competenceGridPage
  :: SyncContext
  -> Maybe CompetenceGridId
  -- ^ Deep-linked grid (URL parameter), if any.
  -> M.Component p Model Action
competenceGridPage r mGridId =
  M.component model update view'
  where
    model = Model Nothing True

    update ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

    style =
      if isTeacher (syncDocumentEnv r).connectedUser
        then CompetenceGridSelectorViewAndCreateStyle
        else CompetenceGridSelectorViewOnlyStyle

    initialPickFn xs = case mGridId of
      Just gid -> Ix.getOne (xs Ix.@= gid) <|> QDefault.defaultCompetenceGrid xs
      Nothing -> QDefault.defaultCompetenceGrid xs

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        ( inlineComponentAttrs "competence-grid-selector" [class_ "h-full"] $
            competenceGridListSelectorComponent r style (Just initialPickFn) #selected
        )
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just grid) =
      MH.div_
        [class_ "h-full w-full"]
        [ MH.div_ [class_ "portrait-hide h-full w-full"]
            [viewerDetailView r grid]
        , MH.div_
            [class_ "hidden portrait-show items-center justify-center h-full w-full"]
            [ MH.div_
                [class_ "flex flex-col items-center gap-4 text-muted-foreground"]
                [ Icon.iconS Icon.XLarge Icon.IcnCompetenceGrid
                , MH.span_ [class_ "text-sm text-center max-w-64"] [M.text (C.translate' C.LblRotateDevice)]
                ]
            ]
        ]
