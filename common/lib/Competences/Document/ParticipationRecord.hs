{-# LANGUAGE CPP #-}

module Competences.Document.ParticipationRecord
  ( ParticipationRecordId
  , ParticipationRecord (..)
  , ParticipationRecordIxs
  , ParticipationType (..)
  , ParticipationLevel (..)
  , allParticipationTypes
  , allParticipationLevels
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id)
import Competences.Document.Lesson (LessonId)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, withText, (.:), (.:?), (.!=))
#endif
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Text (Text)
import GHC.Generics (Generic)

type ParticipationRecordId = Id ParticipationRecord

-- | Predefined forms of student participation during a lesson.
data ParticipationType
  = -- | Student actively participates during whole-class phases (Mitarbeit)
    Participation
  | -- | Student actively collaborates with a peer group (Kollaboration)
    Collaboration
  | -- | Student shows poor work ethic (Arbeit: Unbemüht / Verweigernd)
    PoorWorkEthic
  deriving (Eq, Generic, Ord, Show)

allParticipationTypes :: [ParticipationType]
allParticipationTypes = [Participation, Collaboration, PoorWorkEthic]

#ifdef WITH_AESON
instance FromJSON ParticipationType where
  parseJSON = withText "ParticipationType" $ \case
    -- New names
    "Participation" -> pure Participation
    "Collaboration" -> pure Collaboration
    "PoorWorkEthic" -> pure PoorWorkEthic
    -- Legacy names (backward compat)
    "ActivelyParticipates" -> pure Participation
    "ActivelyCollaborates" -> pure Collaboration
    "RefusesToWork" -> pure PoorWorkEthic
    other -> fail $ "Unknown ParticipationType: " <> show other

instance ToJSON ParticipationType where
  toJSON Participation = String "Participation"
  toJSON Collaboration = String "Collaboration"
  toJSON PoorWorkEthic = String "PoorWorkEthic"
#endif

instance Binary ParticipationType

-- | Quality level within a participation category.
-- Each 'ParticipationType' has two levels with category-specific meanings:
--
-- * Participation: Level1 = Gut, Level2 = Herausragend
-- * Collaboration: Level1 = Gut, Level2 = Herausragend
-- * PoorWorkEthic: Level1 = Unbemüht, Level2 = Verweigernd
data ParticipationLevel
  = ParticipationLevel1
  | ParticipationLevel2
  deriving (Eq, Generic, Ord, Show)

allParticipationLevels :: [ParticipationLevel]
allParticipationLevels = [ParticipationLevel1, ParticipationLevel2]

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
