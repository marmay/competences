-- | Lesson notes page: selector + detail view.
-- The detail pane mounts 'lessonNotesDetailedComponent'; editing goes
-- through the pin editor.
module Competences.Frontend.Page.LessonNotes
  ( lessonNotesPage
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (LessonNotes (..))
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Entity.Assembly (renderResolvedItem)
import Competences.Frontend.Component.LessonNotes.Detailed
  ( LessonNotesDetailedConfig (..)
  , LessonNotesDetailedSettings (..)
  , defaultLessonNotesDetailedSettings
  , lessonNotesDetailedComponent
  )
import Competences.Frontend.Component.Selector.LessonNotesSelector (lessonNotesSelectorComponent)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext (..), SyncDocumentEnv (..))
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Query.DefaultSelection qualified as QDefault
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Router qualified as M
import Miso.String (ms)

data Model = Model
  { selected :: !(Maybe LessonNotes)
  , sidebarOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleSidebar
  deriving (Eq, Show)

lessonNotesPage
  :: SyncContext
  -> Bool
  -- ^ Whether the user can create new lesson notes
  -> Maybe LessonNotesId
  -- ^ Deep link: pre-select this lesson notes entry
  -> M.Component p Model Action
lessonNotesPage r canCreate mLnId =
  M.component model update view'
  where
    model = Model Nothing True

    selectionFn = case mLnId of
      Just lnid -> Just (\allNotes -> Ix.getOne (allNotes Ix.@= lnid))
      Nothing -> Just (QDefault.defaultLessonNotes r.env.currentDay)
    onSelect = Just (\ln -> M.pushURI (M.toURI (ManageLessonNotes (Just ln.id))))

    update ToggleSidebar = M.modify $ \m -> m {sidebarOpen = not m.sidebarOpen}

    view' m =
      Layout.collapsibleSideMenu
        m.sidebarOpen
        ToggleSidebar
        ( inlineComponentAttrs "lesson-notes-selector" [class_ "h-full"] $
            lessonNotesSelectorComponent r canCreate selectionFn onSelect #selected
        )
        (detailView m.selected)

    detailView Nothing =
      Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
    detailView (Just ln) =
      -- Full editor affordances: while release A's gateway migration
      -- is pending, teachers need to resolve orphans / duplicates from
      -- this view. Once all records are cleared, the "Archiv" menu
      -- just ends up empty.
      let legacySettings = defaultLessonNotesDetailedSettings {enableGoTo = False, enableDelete = True}
       in inlineComponent
            ("lesson-notes-detail-" <> ms (show ln.id))
            (lessonNotesDetailedComponent renderResolvedItem r (LessonNotesDetailedConfig ln.id legacySettings))

