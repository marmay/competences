{-# LANGUAGE CPP #-}

module Competences.Document.Evidence
  ( Evidence (..)
  , EvidenceId
  , EvidenceIxs
  , TaskEvaluations
  , SocialForm (..)
  , Ability (..)
  , ActivityType (..)
  , ActivityTasks (..)
  , Observation (..)
  , ObservationId
  , ObservationIxs
  , ObservationRemark (..)
  , TaskRemark (..)
  , mkEvidence
  , socialForms
  , abilities
  , activityTypes
  , taskRemarks
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Common.IxSet qualified as Ix
import Competences.Document.ActivityType (ActivityType (..), activityTypes)
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id, nilId)
import Competences.Document.Lesson (LessonId)
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
import Competences.TaskContent.RichContent (RichContent)
#ifdef WITH_AESON
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), Object, Value, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Aeson.Types (Parser)
#endif
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set (Set)
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

-- | Qualitative remark on a student's work for a specific task.
data TaskRemark
  = -- | Outstanding work
    Exceptional
  | -- | Careless work
    Sloppy
  | -- | Mastery not clearly demonstrated, lacks explanation/structure
    Lacking
  deriving (Bounded, Enum, Eq, Generic, Ord, Read, Show)

taskRemarks :: [TaskRemark]
taskRemarks = [minBound .. maxBound]

newtype ActivityTasks = ActivityTasks Text
  deriving (Eq, Generic, Ord, Show)
#ifdef WITH_AESON
  deriving newtype (Binary, FromJSON, ToJSON)
#else
  deriving newtype (Binary)
#endif

newtype ObservationRemark = ObservationRemark Text
  deriving (Eq, Generic, Ord, Show)
#ifdef WITH_AESON
  deriving newtype (Binary, FromJSON, ToJSON)
#else
  deriving newtype (Binary)
#endif

data Observation = Observation
  { id :: !ObservationId
  , competenceLevelId :: !CompetenceLevelId
  , socialForm :: !SocialForm
  , ability :: !Ability
  }
  deriving (Eq, Generic, Ord, Show)

-- | Per-task competence evaluations. Maps each competence level to its
-- evaluated ability. Empty map means the task has no per-competence data
-- (e.g. legacy evidences or those created outside the assignment evaluator).
type TaskEvaluations = Map CompetenceLevelId Ability

data Evidence = Evidence
  { id :: !EvidenceId
  , userId :: !(Maybe UserId)
  , activityType :: !ActivityType
  , date :: !Day
  , tasks :: !(Map TaskId TaskEvaluations)
    -- ^ Tasks in this evidence, each with optional per-competence evaluations.
    -- Key presence indicates the task is part of the evidence.
    -- Empty inner map means no per-task breakdown is stored.
  , oldTasks :: !Text
    -- ^ Legacy text-based tasks (for gradual migration from activityTasks)
  , observations :: !(Ix.IxSet ObservationIxs Observation)
  , taskRemarks :: !(Map TaskId (Set TaskRemark))
    -- ^ Per-task qualitative remarks (e.g. sloppy, exceptional)
  , taskNotes :: !(Map TaskId RichContent)
    -- ^ Per-task rich text notes (correction tips, sub-task feedback)
  , assignmentId :: !(Maybe AssignmentId)
    -- ^ Optional link to assignment this evidence was created from
  , lessonId :: !(Maybe LessonId)
    -- ^ Optional link to lesson this evidence was collected during
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
  , tasks = Map.empty
  , oldTasks = ""
  , observations = Ix.empty
  , taskRemarks = Map.empty
  , taskNotes = Map.empty
  , assignmentId = Nothing
  , lessonId = Nothing
  }

socialForms :: [SocialForm]
socialForms = [minBound .. maxBound]

abilities :: [Ability]
abilities = [minBound .. maxBound]

-- Note: activityTypes is re-exported from ActivityType module

type EvidenceIxs = '[EvidenceId, UserId, Day, CompetenceLevelId, AssignmentId, LessonId, TaskId]

instance Ix.Indexable EvidenceIxs Evidence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ maybeToList . (.userId))
      (Ix.ixFun $ singleton . (.date))
      (Ix.ixFun $ map (.competenceLevelId) . Ix.toList . (.observations))
      (Ix.ixFun $ maybe [] singleton . (.assignmentId))
      (Ix.ixFun $ maybe [] singleton . (.lessonId))
      (Ix.ixFun $ Map.keys . (.tasks))

type ObservationIxs = '[ObservationId, CompetenceLevelId, SocialForm, Ability]

instance Ix.Indexable ObservationIxs Observation where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.competenceLevelId))
      (Ix.ixFun $ singleton . (.socialForm))
      (Ix.ixFun $ singleton . (.ability))

#ifdef WITH_AESON
instance FromJSON SocialForm

instance ToJSON SocialForm
#endif

instance Binary SocialForm

#ifdef WITH_AESON
instance FromJSON Ability

instance ToJSON Ability
#endif

instance Binary Ability

#ifdef WITH_AESON
instance FromJSON TaskRemark

instance ToJSON TaskRemark
#endif

instance Binary TaskRemark

#ifdef WITH_AESON
instance FromJSON Evidence where
  parseJSON = withObject "Evidence" $ \v -> do
    -- Parse tasks: try new map format, fall back to old list format
    tasksMap <- parseTasksNewFormat v <|> parseTasksOldFormat v
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
      <*> pure tasksMap
      <*> pure oldTasksValue
      <*> fmap Ix.fromList (v .: "observations")
      <*> v .:? "taskRemarks" .!= Map.empty
      <*> v .:? "taskNotes" .!= Map.empty
      <*> v .:? "assignmentId" .!= Nothing
      <*> v .:? "lessonId" .!= Nothing

-- | Parse new format: "tasks" is a JSON object {taskId: [{competenceLevelId, ability}]}
parseTasksNewFormat :: Object -> Parser (Map TaskId TaskEvaluations)
parseTasksNewFormat v = do
  raw <- v .: "tasks" :: Parser (Map TaskId [Value])
  Map.traverseWithKey (\_ entries -> Map.fromList <$> mapM parseEvalEntry entries) raw

-- | Parse old format: "tasks" is [TaskId] (no per-competence data)
parseTasksOldFormat :: Object -> Parser (Map TaskId TaskEvaluations)
parseTasksOldFormat v = do
  taskIds <- v .:? "tasks" .!= ([] :: [TaskId])
  pure $ Map.fromList [ (tid, Map.empty) | tid <- taskIds ]

-- | Parse a single {competenceLevelId, ability} entry (new format)
parseEvalEntry :: Value -> Parser (CompetenceLevelId, Ability)
parseEvalEntry = withObject "TaskEvalEntry" $ \o ->
  (,) <$> o .: "competenceLevelId" <*> o .: "ability"

instance ToJSON Evidence where
  toJSON e =
    object
      [ "id" .= e.id
      , "userId" .= e.userId
      , "activityType" .= e.activityType
      , "date" .= e.date
      , "tasks" .= Map.map evalsToJSON e.tasks
      , "oldTasks" .= e.oldTasks
      , "observations" .= Ix.toList e.observations
      , "taskRemarks" .= e.taskRemarks
      , "taskNotes" .= e.taskNotes
      , "assignmentId" .= e.assignmentId
      , "lessonId" .= e.lessonId
      ]
    where
      evalsToJSON :: TaskEvaluations -> Value
      evalsToJSON evals = toJSON
        [ object ["competenceLevelId" .= clid, "ability" .= ab]
        | (clid, ab) <- Map.toList evals
        ]
#endif

instance Binary Evidence

#ifdef WITH_AESON
instance FromJSON Observation

instance ToJSON Observation
#endif

instance Binary Observation
