-- | Embedding helpers for the detailed lesson-notes view.
--
-- Depends only on the Fragment layer, so safe to import from any entity's
-- component module without creating cycles.
module Competences.Frontend.Component.LessonNotes.Detailed.Embed
  ( updateLessonNotesDetailed
  , renderLessonNotesGroup
  )
where

import Competences.Command qualified as Cmd
import Competences.Command (EntityCommand (..), ModifyCommand (..))
import Competences.Command.LessonNotes (LessonNotesCommand (..))
import Competences.Document (LessonNotes (..))
import Competences.Frontend.Fragment.LessonNotes.Detailed qualified as V
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext, PinViewerRequest (..), modifySyncDocument, requestViewerPin)
import Data.Set qualified as Set
import Miso qualified as M
import Miso.Router qualified as M
import Miso.String (ms)
import Optics.Core (Lens', (%), (%~), (.~))

-- | Embeddable update: pass a lens at the parent's 'LessonNotesDetailedState'.
updateLessonNotesDetailed
  :: Lens' model V.LessonNotesDetailedState
  -> SyncContext
  -> (V.LessonNotesDetailedAction -> action)
  -> V.LessonNotesDetailedAction
  -> M.Effect parent model action
updateLessonNotesDetailed stateLens r _lift = go
  where
    go (V.MenuEdit lnid) = do
      dismiss
      M.io_ $ modifySyncDocument r $ Cmd.LessonNotes (OnLessonNotes (Modify lnid Lock))
    go (V.MenuPin ln) = do
      dismiss
      M.io_ $ requestViewerPin r (PinLessonNotesViewer ln)
    go (V.MenuGoTo lnid) = do
      dismiss
      M.io_ $ M.pushURI (M.toURI (ManageLessonNotes (Just lnid)))
    go (V.MenuDelete lnid) = do
      dismiss
      M.io_ $ modifySyncDocument r $ Cmd.LessonNotes (OnLessonNotes (Delete lnid))
    go action = M.modify (stateLens %~ V.updateLessonNotesDetailedPure action)

    dismiss = M.modify (stateLens % #menuDismissed .~ True)

-- | Render a lesson-notes group as a collapsible disclosure.
-- Body is caller-supplied (e.g. items with relevance annotations).
renderLessonNotesGroup
  :: V.LessonNotesDetailedState
  -> [M.View m a]
  -> M.View m a
  -> (V.LessonNotesDetailedAction -> a)
  -> LessonNotes
  -> M.View m a
renderLessonNotesGroup state annotations body liftAction ln =
  V.lessonNotesDisclosureView
    (liftAction (V.ToggleLessonNotes ln.id))
    (ms ln.title)
    annotations
    (not $ Set.member ln.id state.expandedLessonNotes)
    body
