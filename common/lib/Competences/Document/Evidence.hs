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
  , ObservationIxs
  , ObservationRemark (..)
  , mkEvidence
  , socialForms
  , abilities
  , activityTypes
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Common.IxSet qualified as Ix
import Competences.Document.ActivityType (ActivityType (..), activityTypes)
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id, nilId)
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)

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
  , userId :: !(Maybe UserId)
  , activityType :: !ActivityType
  , date :: !Day
  , tasks :: ![TaskId]
    -- ^ Task references (new format)
  , oldTasks :: !Text
    -- ^ Legacy text-based tasks (for gradual migration from activityTasks)
  , observations :: !(Ix.IxSet ObservationIxs Observation)
  , assignmentId :: !(Maybe AssignmentId)
    -- ^ Optional link to assignment this evidence was created from
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
  , userId = Nothing
  , activityType = SchoolExercise
  , date = fromGregorian 2025 1 1
  , tasks = []
  , oldTasks = ""
  , observations = Ix.empty
  , assignmentId = Nothing
  }

socialForms :: [SocialForm]
socialForms = [minBound .. maxBound]

abilities :: [Ability]
abilities = [minBound .. maxBound]

-- Note: activityTypes is re-exported from ActivityType module

type EvidenceIxs = '[EvidenceId, UserId, Day, CompetenceLevelId, AssignmentId]

instance Ix.Indexable EvidenceIxs Evidence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ maybeToList . (.userId))
      (Ix.ixFun $ singleton . (.date))
      (Ix.ixFun $ map (.competenceLevelId) . Ix.toList . (.observations))
      (Ix.ixFun $ maybe [] singleton . (.assignmentId))

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

instance FromJSON Evidence where
  parseJSON = withObject "Evidence" $ \v -> do
    -- Read new format fields
    tasksList <- v .:? "tasks" .!= []
    -- Migrate old activityTasks to oldTasks
    legacyTasks <- v .:? "activityTasks"
    oldTasksValue <- case legacyTasks of
          Nothing -> v .:? "oldTasks" .!= ""
          Just (ActivityTasks t) -> pure t
    Evidence
      <$> v .: "id"
      <*> v .:? "userId"
      <*> v .: "activityType"
      <*> v .: "date"
      <*> pure tasksList
      <*> pure oldTasksValue
      <*> fmap Ix.fromList (v .: "observations")
      <*> v .:? "assignmentId" .!= Nothing

instance ToJSON Evidence where
  toJSON e =
    object
      [ "id" .= e.id
      , "userId" .= e.userId
      , "activityType" .= e.activityType
      , "date" .= e.date
      , "tasks" .= e.tasks
      , "oldTasks" .= e.oldTasks
      , "observations" .= Ix.toList e.observations
      , "assignmentId" .= e.assignmentId
      ]

instance Binary Evidence

instance FromJSON Observation

instance ToJSON Observation

instance Binary Observation
