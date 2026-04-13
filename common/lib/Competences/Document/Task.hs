{-# LANGUAGE CPP #-}

module Competences.Document.Task
  ( -- * IDs
    TaskId
    -- * Identifiers
  , TaskIdentifier (..)
    -- * Task Purpose
  , TaskPurpose (..)
  , taskPurposes
    -- * Task
  , Task (..)
  , TaskIxs
    -- * Helper Functions
  , isResourceTask
  , getTaskPrimaryCompetences
  , getTaskSecondaryCompetences
  , getTaskAllCompetences
  , taskDisplayName
  , defaultTask
  )
where

import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.FileRef (FileRef)
import Competences.Document.Id (Id)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Maybe (fromMaybe)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import Data.List (singleton)
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | ID for a Task.
type TaskId = Id Task

-- | Human-readable identifier for a Task.
newtype TaskIdentifier = TaskIdentifier Text
  deriving (Eq, Generic, Ord, Show)
#ifdef WITH_AESON
  deriving newtype (Binary, FromJSON, ToJSON)
#else
  deriving newtype (Binary)
#endif

-- | Purpose of a task: practice or assessment.
data TaskPurpose
  = -- | Helps develop competence, but not sufficient alone to prove achievement.
    Practice
  | -- | Clearly demonstrates competence has been achieved.
    Assessment
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON TaskPurpose where
  parseJSON = \case
    "Practice" -> pure Practice
    "Assessment" -> pure Assessment
    _ -> fail "Invalid TaskPurpose"

instance ToJSON TaskPurpose where
  toJSON Practice = "Practice"
  toJSON Assessment = "Assessment"
#endif

instance Binary TaskPurpose

-- | All task purposes
taskPurposes :: [TaskPurpose]
taskPurposes = [minBound .. maxBound]

-- | Atomic unit of work with competence associations.
data Task = Task
  { id :: !TaskId
  , identifier :: !TaskIdentifier
    -- ^ Human-readable identifier (e.g., "Book-1.2.3.a", "Worksheet-15-Task-2").
    -- User is responsible for uniqueness.
  , title :: !Text
    -- ^ Descriptive title (e.g., "Quadratische Gleichungen").
    -- Empty string means no title.
  , content :: !(Maybe RichContent)
    -- ^ Inline task content (if provided).
    -- Nothing = reference-only task (students look up by identifier).
    -- Just text = task content shown inline.
  , primary :: ![CompetenceLevelId]
    -- ^ Competences that this task primarily tests for.
  , secondary :: ![CompetenceLevelId]
    -- ^ Competences that may be tested by this task.
  , purpose :: !TaskPurpose
    -- ^ Practice (develops competence) or Assessment (proves competence).
  , displayInResources :: !Bool
    -- ^ Whether to show this task in resource view for its primary competences.
  , attachments :: ![FileRef]
    -- ^ Files attached to this task, referenced from markdown via file: URLs.
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> do
    -- Support old format with taskType wrapper
    mTaskType <- v .:? "taskType"
    case mTaskType of
      Just tt -> parseOldFormat v tt
      Nothing -> parseNewFormat v
    where
      parseNewFormat v =
        Task
          <$> v .: "id"
          <*> v .: "identifier"
          <*> v .:? "title" .!= ""
          <*> v .:? "content"
          <*> v .:? "primary" .!= []
          <*> v .:? "secondary" .!= []
          <*> v .:? "purpose" .!= Practice
          <*> v .:? "displayInResources" .!= True
          <*> v .:? "attachments" .!= []

      -- Migrate old SelfContained format; SubTask uses defaults (migrated at Document level)
      parseOldFormat v tt = do
        tid <- v .: "id"
        ident <- v .: "identifier"
        ttl <- v .:? "title" .!= ""
        cnt <- v .:? "content"
        attach <- v .:? "attachments" .!= []
        tag <- tt .: "tag"
        case tag :: Text of
          "SelfContained" -> do
            attrs <- tt .: "attributes"
            prim <- attrs .: "primary"
            sec <- attrs .: "secondary"
            purp <- attrs .: "purpose"
            disp <- attrs .: "displayInResources"
            pure $ Task tid ident ttl cnt prim sec purp disp attach
          _ -> do
            -- SubTask: extract override values, using defaults where missing/null.
            -- Full resolution with group defaults happens at Document migration level.
            (prim, sec, purp, disp) <- parseOverride =<< tt .: "override"
            pure $ Task tid ident ttl cnt prim sec purp disp attach

      -- Parse an old TaskAttributesOverride object.
      -- Each field is Maybe a (Nothing = inherit from group, which we map to defaults).
      parseOverride = withObject "TaskAttributesOverride" $ \ov -> do
        prim <- maybe [] (fromMaybe []) <$> ov .:? "primary"
        sec <- maybe [] (fromMaybe []) <$> ov .:? "secondary"
        purp <- maybe Practice (fromMaybe Practice) <$> ov .:? "purpose"
        disp <- maybe True (fromMaybe True) <$> ov .:? "displayInResources"
        pure (prim, sec, purp, disp)

instance ToJSON Task where
  toJSON task =
    object
      [ "id" .= task.id
      , "identifier" .= task.identifier
      , "title" .= task.title
      , "content" .= task.content
      , "primary" .= task.primary
      , "secondary" .= task.secondary
      , "purpose" .= task.purpose
      , "displayInResources" .= task.displayInResources
      , "attachments" .= task.attachments
      ]
#endif

instance Binary Task

-- | IxSet indices for Task.
type TaskIxs = '[TaskId, TaskIdentifier]

instance IxSet.Indexable TaskIxs Task where
  indices =
    IxSet.ixList
      (IxSet.ixFun $ singleton . (.id))
      (IxSet.ixFun $ singleton . (.identifier))

-- Helper Functions

-- | Check if task should be displayed in resources.
isResourceTask :: Task -> Bool
isResourceTask task = task.displayInResources

-- | Get primary competences for a task.
getTaskPrimaryCompetences :: Task -> [CompetenceLevelId]
getTaskPrimaryCompetences = (.primary)

-- | Get secondary competences for a task.
getTaskSecondaryCompetences :: Task -> [CompetenceLevelId]
getTaskSecondaryCompetences = (.secondary)

-- | Get all competences (primary + secondary) for a task.
getTaskAllCompetences :: Task -> [CompetenceLevelId]
getTaskAllCompetences task = task.primary <> task.secondary

-- | Display name for a task: "identifier — title" or just identifier.
taskDisplayName :: Task -> Text
taskDisplayName task =
  let TaskIdentifier ident = task.identifier
      base = if T.null ident then "(Unbenannt)" else ident
   in if T.null task.title then base else base <> " \x2014 " <> task.title

-- | Default task for creation (needs an ID to be provided).
defaultTask :: TaskId -> Task
defaultTask tid =
  Task
    { id = tid
    , identifier = TaskIdentifier ""
    , title = ""
    , content = Nothing
    , primary = []
    , secondary = []
    , purpose = Practice
    , displayInResources = True
    , attachments = []
    }
