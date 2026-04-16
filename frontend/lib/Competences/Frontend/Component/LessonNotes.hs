-- | Lesson notes page: selector + detail view.
-- The detail pane mounts 'lessonNotesDetailedComponent'; editing goes
-- through the pin editor.
module Competences.Frontend.Component.LessonNotes
  ( lessonNotesComponent
  )
where

import Competences.Document (LessonNotes (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.LessonNotes.Detailed
  ( LessonNotesDetailedConfig (..)
  , defaultLessonNotesDetailedSettings
  , lessonNotesDetailedComponent
  )
import Competences.Frontend.Component.Selector.LessonNotesSelector (lessonNotesSelectorComponent)
import Competences.Frontend.SyncContext (SyncContext (..), SyncDocumentEnv (..))
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.DefaultSelection qualified as QDefault
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)

data Model = Model
  { selected :: !(Maybe LessonNotes)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleSidebar
  deriving (Eq, Show)

lessonNotesComponent
  :: SyncContext
  -> Bool
  -- ^ Whether the user can create new lesson notes
  -> M.Component p Model Action
lessonNotesComponent r canCreate =
  M.component model update view'
  where
    model = Model Nothing True

    update ToggleSidebar = M.modify $ \m -> m {sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        ( inlineComponentAttrs "lesson-notes-selector" [class_ "h-full"] $
            lessonNotesSelectorComponent r canCreate
              (Just $ QDefault.defaultLessonNotes r.env.currentDay)
              #selected
        )
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just ln) =
      inlineComponent
        ("lesson-notes-detail-" <> ms (show ln.id))
        (lessonNotesDetailedComponent r (LessonNotesDetailedConfig ln.id defaultLessonNotesDetailedSettings))
