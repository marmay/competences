module Competences.Document.Task
  ( -- * IDs
    TaskId
  , TaskGroupId
    -- * Identifiers
  , TaskIdentifier (..)
  , TaskGroupIdentifier (..)
    -- * Task Purpose
  , TaskPurpose (..)
  , taskPurposes
    -- * Task Attributes
  , TaskAttributes (..)
  , TaskAttributesOverride (..)
  , defaultTaskAttributes
    -- * Task Group
  , TaskGroup (..)
  , TaskGroupIxs
    -- * Task
  , Task (..)
  , TaskType (..)
  , TaskIxs
    -- * Helper Functions
  , getTaskAttributes
  , getTaskContent
  , isResourceTask
  , getTaskPrimaryCompetences
  , getTaskSecondaryCompetences
  , getTaskAllCompetences
  , getTasksInGroup
  , taskGroupId
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import Data.List (singleton, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | ID for a Task.
type TaskId = Id Task

-- | ID for a TaskGroup.
type TaskGroupId = Id TaskGroup

-- | Human-readable identifier for a Task.
newtype TaskIdentifier = TaskIdentifier Text
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Binary, FromJSON, ToJSON)

-- | Human-readable identifier for a TaskGroup.
newtype TaskGroupIdentifier = TaskGroupIdentifier Text
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (Binary, FromJSON, ToJSON)

-- | Purpose of a task: practice or assessment.
data TaskPurpose
  = -- | Helps develop competence, but not sufficient alone to prove achievement.
    Practice
  | -- | Clearly demonstrates competence has been achieved.
    Assessment
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance FromJSON TaskPurpose where
  parseJSON = \case
    "Practice" -> pure Practice
    "Assessment" -> pure Assessment
    _ -> fail "Invalid TaskPurpose"

instance ToJSON TaskPurpose where
  toJSON Practice = "Practice"
  toJSON Assessment = "Assessment"

instance Binary TaskPurpose

-- | All task purposes
taskPurposes :: [TaskPurpose]
taskPurposes = [minBound .. maxBound]

-- | Core attributes that define what a task tests and how it's displayed.
data TaskAttributes = TaskAttributes
  { primary :: ![CompetenceLevelId]
    -- ^ Competences that this task primarily tests for.
  , secondary :: ![CompetenceLevelId]
    -- ^ Competences that may be tested by this task.
  , purpose :: !TaskPurpose
    -- ^ Practice (develops competence) or Assessment (proves competence).
  , displayInResources :: !Bool
    -- ^ Whether to show this task in resource view for its primary competences.
    -- Separate from purpose: controls timing (e.g., hide exam until after test date).
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TaskAttributes where
  parseJSON = withObject "TaskAttributes" $ \v ->
    TaskAttributes
      <$> v .: "primary"
      <*> v .: "secondary"
      <*> v .: "purpose"
      <*> v .: "displayInResources"

instance ToJSON TaskAttributes where
  toJSON attrs =
    object
      [ "primary" .= attrs.primary
      , "secondary" .= attrs.secondary
      , "purpose" .= attrs.purpose
      , "displayInResources" .= attrs.displayInResources
      ]

instance Binary TaskAttributes

-- | Override attributes for subtasks (Nothing = inherit from group).
data TaskAttributesOverride = TaskAttributesOverride
  { primary :: !(Maybe [CompetenceLevelId])
    -- ^ Nothing = inherit from group, Just x = use value x.
  , secondary :: !(Maybe [CompetenceLevelId])
    -- ^ Nothing = inherit from group, Just x = use value x.
  , purpose :: !(Maybe TaskPurpose)
    -- ^ Nothing = inherit from group, Just x = use value x.
  , displayInResources :: !(Maybe Bool)
    -- ^ Nothing = inherit from group, Just x = use value x.
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TaskAttributesOverride where
  parseJSON = withObject "TaskAttributesOverride" $ \v ->
    TaskAttributesOverride
      <$> v .:? "primary"
      <*> v .:? "secondary"
      <*> v .:? "purpose"
      <*> v .:? "displayInResources"

instance ToJSON TaskAttributesOverride where
  toJSON override =
    object
      [ "primary" .= override.primary
      , "secondary" .= override.secondary
      , "purpose" .= override.purpose
      , "displayInResources" .= override.displayInResources
      ]

instance Binary TaskAttributesOverride

-- | Organizational unit for related tasks with shared defaults.
data TaskGroup = TaskGroup
  { id :: !TaskGroupId
  , identifier :: !TaskGroupIdentifier
    -- ^ Human-readable identifier (e.g., "Book-Chapter-1.2").
    -- User is responsible for uniqueness.
  , defaultTaskAttributes :: !TaskAttributes
    -- ^ Default attributes inherited by subtasks.
  , contentBefore :: !(Maybe Text)
    -- ^ Content displayed before subtask content (when rendering).
  , contentAfter :: !(Maybe Text)
    -- ^ Content displayed after subtask content (when rendering).
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TaskGroup where
  parseJSON = withObject "TaskGroup" $ \v ->
    TaskGroup
      <$> v .: "id"
      <*> v .: "identifier"
      <*> v .: "defaultTaskAttributes"
      <*> v .:? "contentBefore"
      <*> v .:? "contentAfter"

instance ToJSON TaskGroup where
  toJSON group =
    object
      [ "id" .= group.id
      , "identifier" .= group.identifier
      , "defaultTaskAttributes" .= group.defaultTaskAttributes
      , "contentBefore" .= group.contentBefore
      , "contentAfter" .= group.contentAfter
      ]

instance Binary TaskGroup

-- | Task type: self-contained or part of a group.
data TaskType
  = -- | Standalone task with own complete attributes.
    SelfContained !TaskAttributes
  | -- | Task belonging to a group, inheriting defaults with optional overrides.
    SubTask !TaskGroupId !TaskAttributesOverride
  deriving (Eq, Generic, Ord, Show)

instance FromJSON TaskType where
  parseJSON = withObject "TaskType" $ \v -> do
    tag <- v .: "tag"
    case tag :: Text of
      "SelfContained" -> SelfContained <$> v .: "attributes"
      "SubTask" -> SubTask <$> v .: "groupId" <*> v .: "override"
      _ -> fail "Invalid TaskType tag"

instance ToJSON TaskType where
  toJSON (SelfContained attrs) =
    object
      [ "tag" .= ("SelfContained" :: Text)
      , "attributes" .= attrs
      ]
  toJSON (SubTask groupId override) =
    object
      [ "tag" .= ("SubTask" :: Text)
      , "groupId" .= groupId
      , "override" .= override
      ]

instance Binary TaskType

-- | Atomic unit of work, either standalone or part of a group.
data Task = Task
  { id :: !TaskId
  , identifier :: !TaskIdentifier
    -- ^ Human-readable identifier (e.g., "Book-1.2.3.a", "Worksheet-15-Task-2").
    -- User is responsible for uniqueness.
  , content :: !(Maybe Text)
    -- ^ Inline task content (if provided).
    -- Nothing = reference-only task (students look up by identifier).
    -- Just text = task content shown inline.
  , taskType :: !TaskType
  }
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v ->
    Task
      <$> v .: "id"
      <*> v .: "identifier"
      <*> v .:? "content"
      <*> v .: "taskType"

instance ToJSON Task where
  toJSON task =
    object
      [ "id" .= task.id
      , "identifier" .= task.identifier
      , "content" .= task.content
      , "taskType" .= task.taskType
      ]

instance Binary Task

-- | IxSet indices for TaskGroup.
type TaskGroupIxs = '[TaskGroupId, TaskGroupIdentifier]

instance IxSet.Indexable TaskGroupIxs TaskGroup where
  indices =
    IxSet.ixList
      (IxSet.ixFun $ singleton . (.id))
      (IxSet.ixFun $ singleton . (.identifier))

-- | IxSet indices for Task.
type TaskIxs = '[TaskId, TaskIdentifier, Maybe TaskGroupId]

instance IxSet.Indexable TaskIxs Task where
  indices =
    IxSet.ixList
      (IxSet.ixFun $ singleton . (.id))
      (IxSet.ixFun $ singleton . (.identifier))
      (IxSet.ixFun $ singleton . taskGroupId)

-- | Extract TaskGroupId from a Task (if it's a SubTask).
taskGroupId :: Task -> Maybe TaskGroupId
taskGroupId task = case task.taskType of
  SelfContained _ -> Nothing
  SubTask gid _ -> Just gid

-- Helper Functions

-- | Get resolved attributes for any task.
-- For SubTask, this requires the TaskGroups IxSet to look up the group.
-- Note: This will error if the TaskGroup is not found.
getTaskAttributes :: Ix.IxSet TaskGroupIxs TaskGroup -> Task -> TaskAttributes
getTaskAttributes taskGroups task = case task.taskType of
  SelfContained attrs -> attrs
  SubTask groupId override ->
    case Ix.getOne (Ix.getEQ groupId taskGroups) of
      Nothing -> error $ "TaskGroup not found: " <> show groupId
      Just group ->
        let defaults = group.defaultTaskAttributes
         in TaskAttributes
              { primary = fromMaybe defaults.primary override.primary
              , secondary = fromMaybe defaults.secondary override.secondary
              , purpose = fromMaybe defaults.purpose override.purpose
              , displayInResources = fromMaybe defaults.displayInResources override.displayInResources
              }

-- | Get composed content for any task.
-- For SubTask, this composes: [contentBefore] [task.content] [contentAfter].
getTaskContent :: Ix.IxSet TaskGroupIxs TaskGroup -> Task -> Maybe Text
getTaskContent taskGroups task = case task.taskType of
  SelfContained _ -> task.content
  SubTask groupId _ ->
    case Ix.getOne (Ix.getEQ groupId taskGroups) of
      Nothing -> task.content -- fallback if group not found
      Just group ->
        let before = fromMaybe "" group.contentBefore
            taskContent = fromMaybe "" task.content
            after = fromMaybe "" group.contentAfter
            composed = before <> taskContent <> after
         in if T.null composed then Nothing else Just composed

-- | Check if task should be displayed in resources.
isResourceTask :: Ix.IxSet TaskGroupIxs TaskGroup -> Task -> Bool
isResourceTask taskGroups task = (getTaskAttributes taskGroups task).displayInResources

-- | Get primary competences for a task.
getTaskPrimaryCompetences :: Ix.IxSet TaskGroupIxs TaskGroup -> Task -> [CompetenceLevelId]
getTaskPrimaryCompetences taskGroups task = (getTaskAttributes taskGroups task).primary

-- | Get secondary competences for a task.
getTaskSecondaryCompetences :: Ix.IxSet TaskGroupIxs TaskGroup -> Task -> [CompetenceLevelId]
getTaskSecondaryCompetences taskGroups task = (getTaskAttributes taskGroups task).secondary

-- | Get all competences (primary + secondary) for a task.
getTaskAllCompetences :: Ix.IxSet TaskGroupIxs TaskGroup -> Task -> [CompetenceLevelId]
getTaskAllCompetences taskGroups task =
  let attrs = getTaskAttributes taskGroups task
   in attrs.primary <> attrs.secondary

-- | Get all tasks in a group, sorted by identifier.
getTasksInGroup :: TaskGroupId -> Ix.IxSet TaskIxs Task -> [Task]
getTasksInGroup groupId tasks =
  sortOn (.identifier) $
    Ix.toList $
      Ix.getEQ (Just groupId) tasks

-- | Default TaskAttributes for new tasks
defaultTaskAttributes :: TaskAttributes
defaultTaskAttributes =
  TaskAttributes
    { primary = []
    , secondary = []
    , purpose = Practice
    , displayInResources = True
    }
