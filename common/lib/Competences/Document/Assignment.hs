{-# LANGUAGE CPP #-}

module Competences.Document.Assignment
  ( Assignment (..)
  , AssignmentId
  , AssignmentName (..)
  , AssignmentIxs
  , mkAssignment
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Id (Id)
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:), (.:?), (.!=))
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed (Indexable (..), ixFun, ixList)
import Data.Set (Set)
import Data.Set qualified as Set
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- | Type alias for Assignment ID
type AssignmentId = Id Assignment

-- | Name of an assignment (newtype wrapper for type safety)
newtype AssignmentName = AssignmentName Text
  deriving stock (Eq, Ord, Show, Generic)
#ifdef WITH_AESON
  deriving newtype (FromJSON, ToJSON, Binary)
#else
  deriving newtype (Binary)
#endif

-- | An assignment represents a collection of tasks given to students
data Assignment = Assignment
  { id :: !AssignmentId
  , name :: !AssignmentName
  , description :: !RichContent
    -- ^ Description/instructions for the assignment (supports LaTeX math syntax)
  , assignmentDate :: !Day
  , activityType :: !ActivityType
  , studentIds :: !(Set UserId)
  , tasks :: ![TaskId]
  , groupSubmissionAllowed :: !Bool
  }
  deriving (Eq, Generic, Ord, Show)

#ifdef WITH_AESON
-- JSON instances
instance FromJSON Assignment where
  parseJSON = withObject "Assignment" $ \v ->
    -- Old JSON with lessonId field is silently ignored (not requested)
    Assignment
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "description" .!= mempty  -- Default to empty for backward compatibility
      <*> v .: "assignmentDate"
      <*> v .: "activityType"
      <*> v .: "studentIds"
      <*> v .: "tasks"
      <*> v .:? "groupSubmissionAllowed" .!= False

instance ToJSON Assignment
#endif

instance Binary Assignment

-- | Index types for Assignment
-- Indexed by:
-- - AssignmentId (primary key lookup)
-- - UserId (query assignments for a specific student)
-- - Day (filter by date range)
-- - TaskId (find assignments containing a specific task)
type AssignmentIxs = '[AssignmentId, UserId, Day, TaskId]

-- | Make an assignment indexable
instance Indexable AssignmentIxs Assignment where
  indices =
    ixList
      (ixFun (\a -> [a.id]))
      (ixFun (\a -> Set.toList a.studentIds))
      (ixFun (\a -> [a.assignmentDate]))
      (ixFun (\a -> a.tasks))

-- | Helper to create an assignment with default values
mkAssignment :: AssignmentId -> AssignmentName -> Day -> Assignment
mkAssignment aid aname date =
  Assignment
    { id = aid
    , name = aname
    , description = mempty
    , assignmentDate = date
    , activityType = SchoolExercise
    , studentIds = mempty
    , tasks = []
    , groupSubmissionAllowed = False
    }
