{-# LANGUAGE CPP #-}

-- | Teacher-only annotation attached to a 'Lesson' or 'LessonPhase'
-- via a 'privateNoteRef'.
--
-- Externalised so that 'Lesson' itself can be fully public (visible
-- to students with no field stripping), satisfying the projection
-- law that broadcast patches apply identically against the teacher's
-- and the student's local document.
--
-- Audience: 'AudienceTeachers' — students never receive
-- 'TeachingNote' commands and their projection contains an empty
-- 'teachingNotes' set.
module Competences.Document.TeachingNote
  ( TeachingNote (..)
  , TeachingNoteId
  , TeachingNoteIxs
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Common.BinaryOrphans ()
import Competences.Document.Id (Id)
import Competences.TaskContent.RichContent (RichContent)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.List (singleton)
import GHC.Generics (Generic)

type TeachingNoteId = Id TeachingNote

data TeachingNote = TeachingNote
  { id :: !TeachingNoteId
  , content :: !RichContent
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON TeachingNote

instance ToJSON TeachingNote
#endif

instance Binary TeachingNote

type TeachingNoteIxs = '[TeachingNoteId]

instance Ix.Indexable TeachingNoteIxs TeachingNote where
  indices = Ix.ixList (Ix.ixFun $ singleton . (.id))
