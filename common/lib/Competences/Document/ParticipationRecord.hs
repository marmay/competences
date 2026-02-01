{-# LANGUAGE CPP #-}

module Competences.Document.ParticipationRecord
  ( ParticipationRecordId
  , ParticipationRecord (..)
  , ParticipationRecordIxs
  , ParticipationType (..)
  , ParticipationLevel (..)
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:), (.:?), (.!=))
#endif
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Text (Text)
import GHC.Generics (Generic)

type ParticipationRecordId = Id ParticipationRecord

-- | Predefined forms of student participation during a lesson.
data ParticipationType
  = -- | Student actively participates during whole-class phases
    ActivelyParticipates
  | -- | Student actively collaborates with a peer group
    ActivelyCollaborates
  | -- | Student refuses to work
    RefusesToWork
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON ParticipationType

instance ToJSON ParticipationType
#endif

instance Binary ParticipationType

-- | Quality level within a participation category.
-- Each 'ParticipationType' has two levels with category-specific meanings:
--
-- * ActivelyParticipates: Level1 = Gut, Level2 = Herausragend
-- * ActivelyCollaborates: Level1 = Gut, Level2 = Herausragend
-- * RefusesToWork: Level1 = Bemüht sich nicht, Level2 = Verweigert
data ParticipationLevel
  = ParticipationLevel1
  | ParticipationLevel2
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON ParticipationLevel

instance ToJSON ParticipationLevel
#endif

instance Binary ParticipationLevel

-- | Per-student per-lesson participation record.
-- Top-level entity for cross-lesson querying (student history).
-- At most one per (lessonId, userId, participationType).
data ParticipationRecord = ParticipationRecord
  { id :: !ParticipationRecordId
  , lessonId :: !LessonId
  , userId :: !UserId
  , participationType :: !ParticipationType
  , level :: !ParticipationLevel
  , remark :: !(Maybe Text)
  }
  deriving (Eq, Generic, Ord, Show)

type ParticipationRecordIxs = '[ParticipationRecordId, LessonId, UserId, ParticipationType]

instance Ix.Indexable ParticipationRecordIxs ParticipationRecord where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.lessonId))
      (Ix.ixFun $ singleton . (.userId))
      (Ix.ixFun $ singleton . (.participationType))

#ifdef WITH_AESON
instance FromJSON ParticipationRecord where
  parseJSON = withObject "ParticipationRecord" $ \v ->
    ParticipationRecord
      <$> v .: "id"
      <*> v .: "lessonId"
      <*> v .: "userId"
      <*> v .: "participationType"
      <*> v .:? "level" .!= ParticipationLevel1
      <*> v .:? "remark" .!= Nothing

instance ToJSON ParticipationRecord
#endif

instance Binary ParticipationRecord
