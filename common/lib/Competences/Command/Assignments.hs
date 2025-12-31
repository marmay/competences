module Competences.Command.Assignments
  ( AssignmentsCommand (..)
  , AssignmentPatch (..)
  , handleAssignmentsCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.Assignment
  ( Assignment (..)
  , AssignmentName
  )
import Competences.Document.Evidence (ActivityType)
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (^.))

-- | Patch for modifying an Assignment
data AssignmentPatch = AssignmentPatch
  { name :: !(Change AssignmentName)
  , assignmentDate :: !(Change Day)
  , activityType :: !(Change ActivityType)
  , studentIds :: !(Change (Set UserId))
  , tasks :: !(Change [TaskId])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Assignments context
data AssignmentsCommand
  = OnAssignments !(EntityCommand Assignment AssignmentPatch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON AssignmentPatch
instance ToJSON AssignmentPatch
instance Binary AssignmentPatch

instance FromJSON AssignmentsCommand
instance ToJSON AssignmentsCommand
instance Binary AssignmentsCommand

-- Default instance
instance Default AssignmentPatch where
  def =
    AssignmentPatch
      { name = Nothing
      , assignmentDate = Nothing
      , activityType = Nothing
      , studentIds = Nothing
      , tasks = Nothing
      }

-- | Apply a patch to an Assignment, checking for conflicts
applyAssignmentPatch :: Assignment -> AssignmentPatch -> Either Text Assignment
applyAssignmentPatch assignment patch =
  inContext "Assignment" assignment $
    patchField' @"name" patch
      >=> patchField' @"assignmentDate" patch
      >=> patchField' @"activityType" patch
      >=> patchField' @"studentIds" patch
      >=> patchField' @"tasks" patch

-- | Handle an Assignments context command
handleAssignmentsCommand :: UserId -> AssignmentsCommand -> Document -> UpdateResult
handleAssignmentsCommand userId (OnAssignments c) =
  interpretEntityCommand assignmentContext userId c
  where
    assignmentContext =
      mkEntityCommandContext
        #assignments
        #id
        AssignmentLock
        applyAssignmentPatch
        affectedUsersForAssignment

    -- Affected users: all teachers + assigned students
    affectedUsersForAssignment :: Assignment -> Document -> AffectedUsers
    affectedUsersForAssignment a d =
      AffectedUsers $
        map (.id) $
          IxSet.toList (d ^. #users)
            & filter
              ( \u ->
                  u.id `Set.member` a.studentIds || u.role == Teacher
              )
