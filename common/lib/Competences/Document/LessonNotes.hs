{-# LANGUAGE CPP #-}

module Competences.Document.LessonNotes
  ( -- * IDs
    LessonNotesId
    -- * LessonNotes
  , LessonNotes (..)
  , LessonNotesIxs
  , mkLessonNotes
  )
where

import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.Resource (ResourceId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- | ID for a LessonNotes entry.
type LessonNotesId = Id LessonNotes

-- | A lesson notes entry — a dated, ordered collection of resources
-- that represents materials used in a particular lesson.
data LessonNotes = LessonNotes
  { id :: !LessonNotesId
  , date :: !Day
  -- ^ When the lesson happened
  , lessonId :: !(Maybe LessonId)
  -- ^ Optional link to a planning Lesson
  , title :: !Text
  -- ^ E.g. "Gleichungen lösen – Einführung"
  , resources :: ![ResourceId]
  -- ^ Ordered list of resources used in this lesson
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON LessonNotes

instance ToJSON LessonNotes
#endif

instance Binary LessonNotes

-- | IxSet indices for LessonNotes.
-- Indexed by LessonNotesId (unique), Day (chronological), LessonId (link to lesson).
type LessonNotesIxs = '[LessonNotesId, Day, LessonId]

instance Ix.Indexable LessonNotesIxs LessonNotes where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.date))
      (Ix.ixFun $ \ln -> maybeToList ln.lessonId)

-- | Create a new empty lesson notes entry with the given ID and date.
mkLessonNotes :: LessonNotesId -> Day -> LessonNotes
mkLessonNotes lnId day = LessonNotes
  { id = lnId
  , date = day
  , lessonId = Nothing
  , title = ""
  , resources = []
  }
