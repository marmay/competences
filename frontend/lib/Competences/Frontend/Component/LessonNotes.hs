module Competences.Frontend.Component.LessonNotes
  ( lessonNotesComponent
  , LessonNotesMode (..)
  )
where

import Competences.Document (LessonNotes (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.LessonNotes.EditorDetail (editorDetailView)
import Competences.Frontend.Component.LessonNotes.ViewerDetail (viewerDetailView)
import Competences.Frontend.Component.Selector.LessonNotesSelector (lessonNotesSelectorComponent)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty)
import Miso qualified as M

-- | Mode for the lesson notes component
data LessonNotesMode = LessonNotesEdit | LessonNotesView
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Lesson notes component using SelectorDetail pattern
-- Teachers: Edit (default) and View modes
-- Students: View mode only
lessonNotesComponent
  :: SyncContext
  -> LessonNotesMode
  -> NonEmpty LessonNotesMode
  -> Bool
  -- ^ Whether the user can create new lesson notes
  -> M.Component p (SD.Model LessonNotes LessonNotesMode) (SD.Action LessonNotesMode)
lessonNotesComponent r defaultMode availableModes canCreate =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "lesson-notes"
      , SD.selectorComponent = lessonNotesSelectorComponent r canCreate
      , SD.detailView = \mode ln -> case mode of
          LessonNotesEdit -> editorDetailView r ln
          LessonNotesView -> viewerDetailView r ln
      , SD.modeLabel = \case
          LessonNotesEdit -> C.translate' C.LblEdit
          LessonNotesView -> C.translate' C.LblView
      , SD.modeIcon = \case
          LessonNotesEdit -> Just Icon.IcnEdit
          LessonNotesView -> Just Icon.IcnView
      , SD.availableModes = availableModes
      , SD.defaultMode = defaultMode
      , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
      }
