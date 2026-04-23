{-# LANGUAGE CPP #-}

-- | Teacher-only annotation commands. Lockless and idempotent
-- (upsert / delete by id) — the parent 'Lesson' editor manages
-- consistency.
--
-- Audience: 'AudienceTeachers'. Students never receive these
-- commands or see the underlying entity in their projection.
module Competences.Command.TeachingNotes
  ( TeachingNotesCommand (..)
  , handleTeachingNotesCommand
  )
where

import Competences.Command.Common
  ( AffectedUsers (..)
  , CommandContext (..)
  , UpdateResult
  , requireTeacher
  )
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Document.TeachingNote (TeachingNote (..), TeachingNoteId)
import Competences.Document.User (UserRole (..))
import Competences.TaskContent.RichContent (RichContent)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import GHC.Generics (Generic)
import Optics.Core ((%~), (&), (^.))

data TeachingNotesCommand
  = -- | Upsert: create the note if it doesn't exist, else replace its
    --   content. Idempotent.
    SetTeachingNote !TeachingNoteId !RichContent
  | -- | Delete the note. No-op if absent.
    DeleteTeachingNote !TeachingNoteId
  deriving (Eq, Generic, Show)

instance Binary TeachingNotesCommand

#ifdef WITH_AESON
instance FromJSON TeachingNotesCommand
instance ToJSON TeachingNotesCommand
#endif

handleTeachingNotesCommand :: CommandContext -> TeachingNotesCommand -> Document -> UpdateResult
handleTeachingNotesCommand cmdCtx cmd d = do
  requireTeacher cmdCtx.userId d
  case cmd of
    SetTeachingNote nid content ->
      let note = TeachingNote {id = nid, content = content}
          d' = d & #teachingNotes %~ IxSet.insert note . IxSet.deleteIx nid
       in pure (d', teacherAudience d')
    DeleteTeachingNote nid ->
      let d' = d & #teachingNotes %~ IxSet.deleteIx nid
       in pure (d', teacherAudience d')

teacherAudience :: Document -> AffectedUsers
teacherAudience d =
  AffectedUsers $ map (.id) $ filter ((== Teacher) . (.role)) $ Ix.toList (d ^. #users)
