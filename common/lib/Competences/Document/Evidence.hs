{-# OPTIONS_GHC -Wno-orphans #-}

module Competences.Document.Evidence
  ( Evidence (..)
  , EvidenceId
  , EvidenceIxs
  , SocialForm (..)
  , Ability (..)
  , ActivityType (..)
  , ActivityTasks (..)
  , Observation (..)
  , ObservationId
  , ObservationRemark (..)
  , mkEvidence
  , socialForms
  , abilities
  , activityTypes
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id, nilId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)
import qualified Data.Set as Set

type EvidenceId = Id Evidence
type ObservationId = Id Observation

-- | Whether a competence is demonstrated as part of a group or
-- individually.
data SocialForm
  = -- | Competence is demonstrated as part of a group.
    Group
  | -- | Competence is demonstrated individually.
    Individual
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Whether the competence was demonstrated self-reliantly,
-- with some support or not yet at all.
data Ability
  = -- | Competence was demonstrated self-reliantly.
    SelfReliant
  | -- | Competence was demonstrated self-reliantly with some silly
    -- mistakes; but a high level of understanding was demonstrated.
    SelfReliantWithSillyMistakes
  | -- | Competence was demonstrated with some support, like
    -- giving a hint or correcting a minor mistake.
    WithSupport
  | -- | Competence was not successfully demonstrated, either
    -- because the student did not try, did not have the correct
    -- idea or they made a significant mistake.
    NotYet
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

-- | Different kinds of activities during which a student can demonstrate
-- they are competent.
data ActivityType
  = Conversation
    -- ^ A conversation with a teacher.
  | Exam
    -- ^ A written or oral exam.
  | SchoolExercise
    -- ^ Exercising in school.
  | HomeExercise
    -- ^ Home exercise.
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

newtype ActivityTasks = ActivityTasks Text
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Binary, FromJSON, ToJSON)

newtype ObservationRemark = ObservationRemark Text
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Binary, FromJSON, ToJSON)

data Observation = Observation
  { id :: !ObservationId
  , competenceLevelId :: !CompetenceLevelId
  , socialForm :: !SocialForm
  , ability :: !Ability
  }
  deriving (Eq, Generic, Ord, Show)

data Evidence = Evidence
  { id :: !EvidenceId
  , userIds :: !(Set.Set UserId)
  , activityType :: !ActivityType
  , date :: !Day
  , activityTasks :: !ActivityTasks
  , observations :: !(Ix.IxSet ObservationIxs Observation)
  }
  deriving (Eq, Generic, Ord, Show)

mkEvidence :: EvidenceId -> Day -> Evidence
mkEvidence eId date = do
  nilEvidence
    { id = eId
    , date = date
    }

nilEvidence :: Evidence
nilEvidence = Evidence
  { id = nilId
  , userIds = Set.empty
  , activityType = SchoolExercise
  , date = fromGregorian 2025 1 1
  , activityTasks = ActivityTasks ""
  , observations = Ix.empty
  }

socialForms :: [SocialForm]
socialForms = [minBound .. maxBound]

abilities :: [Ability]
abilities = [minBound .. maxBound]

activityTypes :: [ActivityType]
activityTypes = [minBound .. maxBound]

type EvidenceIxs = '[EvidenceId, UserId, Day, CompetenceLevelId]

instance Ix.Indexable EvidenceIxs Evidence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ Set.toList . (.userIds))
      (Ix.ixFun $ singleton . (.date))
      (Ix.ixFun $ map (.competenceLevelId) . Ix.toList . (.observations))

type ObservationIxs = '[ObservationId, CompetenceLevelId, SocialForm, Ability]

instance Ix.Indexable ObservationIxs Observation where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.competenceLevelId))
      (Ix.ixFun $ singleton . (.socialForm))
      (Ix.ixFun $ singleton . (.ability))

instance FromJSON SocialForm

instance ToJSON SocialForm

instance Binary SocialForm

instance FromJSON Ability

instance ToJSON Ability

instance Binary Ability

instance FromJSON ActivityType

instance ToJSON ActivityType

instance Binary ActivityType

instance FromJSON Evidence

instance ToJSON Evidence

instance Binary Evidence

instance FromJSON Observation

instance ToJSON Observation

instance Binary Observation

instance Binary Day
