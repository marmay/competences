{-# LANGUAGE CPP #-}

module Competences.Document.ParticipationRecord
  ( ParticipationRecordId
  , ParticipationRecord (..)
  , ParticipationRecordIxs
  , ParticipationType (..)
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
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

-- | Per-student per-lesson participation record.
-- Top-level entity for cross-lesson querying (student history).
-- At most one per (lessonId, userId, participationType).
data ParticipationRecord = ParticipationRecord
  { id :: !ParticipationRecordId
  , lessonId :: !LessonId
  , userId :: !UserId
  , participationType :: !ParticipationType
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
instance FromJSON ParticipationRecord

instance ToJSON ParticipationRecord
#endif

instance Binary ParticipationRecord
