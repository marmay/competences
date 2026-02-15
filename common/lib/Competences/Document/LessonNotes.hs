{-# LANGUAGE CPP #-}

module Competences.Document.LessonNotes
  ( -- * IDs
    LessonNotesId
    -- * LessonNoteItem
  , LessonNoteItem (..)
    -- * LessonNotes
  , LessonNotes (..)
  , LessonNotesIxs
  , mkLessonNotes
  )
where

import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Task (TaskId)
#ifdef WITH_AESON
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
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

-- | An item in a lesson notes entry — either a resource or a task.
data LessonNoteItem
  = LessonResource !ResourceId
  | LessonTask !TaskId
  deriving (Eq, Generic, Ord, Show)

instance Binary LessonNoteItem

#ifdef WITH_AESON
instance ToJSON LessonNoteItem where
  toJSON (LessonResource rid) = object ["resource" .= rid]
  toJSON (LessonTask tid) = object ["task" .= tid]

instance FromJSON LessonNoteItem where
  parseJSON = withObject "LessonNoteItem" $ \v ->
    (LessonResource <$> v .: "resource")
      <|> (LessonTask <$> v .: "task")
#endif

-- | A lesson notes entry — a dated, ordered collection of resources and tasks
-- that represents materials used in a particular lesson.
data LessonNotes = LessonNotes
  { id :: !LessonNotesId
  , date :: !Day
  -- ^ When the lesson happened
  , lessonId :: !(Maybe LessonId)
  -- ^ Optional link to a planning Lesson
  , title :: !Text
  -- ^ E.g. "Gleichungen lösen – Einführung"
  , items :: ![LessonNoteItem]
  -- ^ Ordered list of resources and tasks used in this lesson
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance ToJSON LessonNotes

instance FromJSON LessonNotes where
  parseJSON = withObject "LessonNotes" $ \v ->
    LessonNotes
      <$> v .: "id"
      <*> v .: "date"
      <*> v .: "lessonId"
      <*> v .: "title"
      <*> (v .:? "items" >>= \case
            Just is -> pure is
            Nothing -> map LessonResource <$> v .:? "resources" .!= [])
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
  , items = []
  }
