-- | Modal and pin entry points for the lesson-notes viewer.
--
-- Both helpers mount the shared 'lessonNotesDetailedComponent'. The viewer
-- pin uses a @"lesson-notes-ref-"@ key to coexist with the editor pin
-- (which uses @"lesson-notes-"@ via LockWatching).
module Competences.Frontend.Component.LessonNotes.ViewerDetail
  ( pinLessonNotesViewer
  , openLessonNotesModal
  )
where

import Competences.Document (LessonNotes (..))
import Competences.Document.Id (idToText)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Entity.Assembly (renderResolvedItem)
import Competences.Frontend.Component.LessonNotes.Detailed
  ( LessonNotesDetailedConfig (..)
  , defaultLessonNotesDetailedSettings
  , lessonNotesDetailedComponent
  )
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager
  ( ModalConfig (..)
  , ModalId (..)
  , ModalHeight (..)
  , ModalWidth (..)
  , PinCategory (..)
  , PinMeta (..)
  , SortAtom (..)
  , SortKey (..)
  , WindowChrome (..)
  , openFramedModalWith
  , pinDialogWith
  )
import Competences.Frontend.View.Icon qualified as Icon
import Miso.String (ms)

openLessonNotesModal :: SyncContext -> LessonNotes -> IO ()
openLessonNotesModal r ln =
  openFramedModalWith
    r.windowManager
    ( ModalConfig
        { chrome = WindowChrome (ms ln.title) Icon.IcnLessonNotes Nothing
        , modalId = ModalId ("lesson-notes-" <> idToText ln.id)
        , width = ModalWide
        , height = ModalFull
        , pinnable = Just ()
        }
    )
    (\_mode -> lessonNotesDetailedComponent renderResolvedItem r cfg)
  where
    cfg = LessonNotesDetailedConfig ln.id defaultLessonNotesDetailedSettings

pinLessonNotesViewer :: SyncContext -> LessonNotes -> IO ()
pinLessonNotesViewer r ln =
  let chrome = WindowChrome (ms ln.title) Icon.IcnLessonNotes Nothing
      meta = PinMeta
        { key = "lesson-notes-ref-" <> idToText ln.id
        , category = PinCatLessonNotes
        , sortKey = SortKey [SortAtom ln.date, SortAtom ln.title, SortAtom ln.id]
        , context = Just (C.formatDayShort ln.date)
        }
      cfg = LessonNotesDetailedConfig ln.id defaultLessonNotesDetailedSettings
   in pinDialogWith r.windowManager meta chrome
        (\_ (_ :: Maybe ()) -> lessonNotesDetailedComponent renderResolvedItem r cfg)

