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
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:), (.:?), (.!=))
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
  deriving newtype (FromJSON, ToJSON, Binary)

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
  }
  deriving (Eq, Generic, Ord, Show)

-- JSON and Binary instances
instance FromJSON Assignment where
  parseJSON = withObject "Assignment" $ \v ->
    Assignment
      <$> v .: "id"
      <*> v .: "name"
      <*> v .:? "description" .!= mempty  -- Default to empty for backward compatibility
      <*> v .: "assignmentDate"
      <*> v .: "activityType"
      <*> v .: "studentIds"
      <*> v .: "tasks"

instance ToJSON Assignment
instance Binary Assignment

-- | Index types for Assignment
-- Indexed by:
-- - AssignmentId (primary key lookup)
-- - UserId (query assignments for a specific student)
-- - Day (filter by date range)
type AssignmentIxs = '[AssignmentId, UserId, Day]

-- | Make an assignment indexable
instance Indexable AssignmentIxs Assignment where
  indices =
    ixList
      (ixFun (\a -> [a.id]))
      (ixFun (\a -> Set.toList a.studentIds))
      (ixFun (\a -> [a.assignmentDate]))

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
    }
