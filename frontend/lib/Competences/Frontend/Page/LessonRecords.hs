-- | Lesson records (Schulübung) list + detail page. The left pane mounts
-- 'lessonRecordsListSelectorComponent' (which owns the subscription and
-- exposes the chosen 'LessonRow' via a binding); the right pane mounts
-- 'lessonDetailedComponent' in student mode for the selected lesson.
module Competences.Frontend.Page.LessonRecords
  ( lessonRecordsPage
  )
where

import Competences.Document.Lesson (LessonId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Lesson.Detailed
  ( LessonDetailedConfig (..)
  , LessonDetailedMode (..)
  , lessonDetailedComponent
  )
import Competences.Frontend.Component.LessonRecords.ListSelector (LessonRow (..), lessonRecordsListSelectorComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)

data Model = Model
  { selected :: !(Maybe LessonRow)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleSidebar
  deriving (Eq, Show)

lessonRecordsPage :: SyncContext -> Maybe LessonId -> M.Component p Model Action
lessonRecordsPage r mSelected =
  M.component model update' view'
  where
    model = Model Nothing True

    update' ToggleSidebar = M.modify $ \m -> m{sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        ( inlineComponentAttrs "lesson-record-selector" [class_ "h-full"] $
            lessonRecordsListSelectorComponent r mSelected #selected
        )
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just row) =
      inlineComponent
        ("lesson-record-detail-" <> ms (show row.lessonId))
        (lessonDetailedComponent r (LessonDetailedConfig row.lessonId StudentMode))
