-- | Embedding helpers for the detailed lesson-notes view.
--
-- Depends only on the Fragment layer, so safe to import from any entity's
-- component module without creating cycles.
module Competences.Frontend.Component.LessonNotes.Detailed.Embed
  ( updateLessonNotesDetailed
  , renderLessonNotesGroup
  )
where

import Competences.Document (LessonNotes (..))
import Competences.Frontend.Fragment.LessonNotes.Detailed qualified as V
import Competences.Frontend.SyncContext (SyncContext)
import Data.Set qualified as Set
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Lens', (%~))

-- | Embeddable update: pass a lens at the parent's 'LessonNotesDetailedState'.
updateLessonNotesDetailed
  :: Lens' model V.LessonNotesDetailedState
  -> SyncContext
  -> (V.LessonNotesDetailedAction -> action)
  -> V.LessonNotesDetailedAction
  -> M.Effect parent model action
updateLessonNotesDetailed stateLens _r _lift action =
  M.modify (stateLens %~ V.updateLessonNotesDetailedPure action)

-- | Render a lesson-notes group as a collapsible disclosure.
-- Body is caller-supplied (e.g. items with relevance annotations).
renderLessonNotesGroup
  :: V.LessonNotesDetailedState
  -> [M.View m a]
  -- ^ Header annotations (edit button, open-modal button)
  -> M.View m a
  -- ^ Pre-rendered body
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
