{-# LANGUAGE CPP #-}

module Competences.Command.Assignments
  ( AssignmentsCommand (..)
  , AssignmentPatch (..)
  , handleAssignmentsCommand
  , applyAssignmentPatch
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), UpdateResult, inContext, patchField')
import Competences.Command.Interpret (EntityCommandContext (..), interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.Assignment
  ( Assignment (..)
  , AssignmentId
  , AssignmentName
  )
import Competences.Document.Evidence (ActivityType)
import Competences.Document.Task (TaskId)
import Competences.Document.Session (SessionId)
import Competences.Document.User (UserId)
import Control.Monad (unless, (>=>))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Default (Default (..))
import Data.IxSet.Typed qualified as IxSet
import Data.Set (Set)
import Data.Set qualified as Set
import Competences.TaskContent.RichContent (RichContent)
import Data.Text (Text, pack)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (^.))

-- | Patch for modifying an Assignment
data AssignmentPatch = AssignmentPatch
  { name :: !(Change AssignmentName)
  , description :: !(Change RichContent)
  , assignmentDate :: !(Change Day)
  , activityType :: !(Change ActivityType)
  , studentIds :: !(Change (Set UserId))
  , tasks :: !(Change [TaskId])
  , groupSubmissionAllowed :: !(Change Bool)
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Assignments context
data AssignmentsCommand
  = OnAssignments !(EntityCommand Assignment AssignmentPatch)
  deriving (Eq, Generic, Show)

instance Binary AssignmentPatch
#ifdef WITH_AESON
instance FromJSON AssignmentPatch
instance ToJSON AssignmentPatch
#endif

instance Binary AssignmentsCommand
#ifdef WITH_AESON
instance FromJSON AssignmentsCommand
instance ToJSON AssignmentsCommand
#endif

-- Default instance
instance Default AssignmentPatch where
  def =
    AssignmentPatch
      { name = Nothing
      , description = Nothing
      , assignmentDate = Nothing
      , activityType = Nothing
      , studentIds = Nothing
      , tasks = Nothing
      , groupSubmissionAllowed = Nothing
      }

-- | Apply a patch to an Assignment, checking for conflicts
applyAssignmentPatch :: Assignment -> AssignmentPatch -> Either Text Assignment
applyAssignmentPatch assignment patch =
  inContext "Assignment" assignment $
    patchField' @"name" patch
      >=> patchField' @"description" patch
      >=> patchField' @"assignmentDate" patch
      >=> patchField' @"activityType" patch
      >=> patchField' @"studentIds" patch
      >=> patchField' @"tasks" patch
      >=> patchField' @"groupSubmissionAllowed" patch

-- | Validate that no lessons or evidences reference this assignment
validateAssignmentNotReferenced :: Document -> AssignmentId -> Either Text ()
validateAssignmentNotReferenced doc aid = do
  let referencingLessons = IxSet.toList $ doc.lessons IxSet.@= aid
  unless (null referencingLessons) $
    Left $ "Assignment is referenced by " <> pack (show (length referencingLessons)) <> " lesson(s)"
  let referencingEvidences = IxSet.toList $ doc.evidences IxSet.@= aid
  unless (null referencingEvidences) $
    Left $ "Assignment is referenced by " <> pack (show (length referencingEvidences)) <> " evidence(s)"

-- | Handle an Assignments context command
handleAssignmentsCommand :: UserId -> SessionId -> AssignmentsCommand -> Document -> UpdateResult
handleAssignmentsCommand userId sid (OnAssignments c) d = case c of
  Delete aid -> do
    validateAssignmentNotReferenced d aid
    (d', a) <- assignmentContext.delete aid d
    pure (d', assignmentContext.affectedUsers a d)
  _ ->
    interpretEntityCommand assignmentContext userId sid c d
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
    affectedUsersForAssignment a doc =
      AffectedUsers $
        map (.id) $
          IxSet.toList (doc ^. #users)
            & filter
              ( \u ->
                  u.id `Set.member` a.studentIds || u.role == Teacher
              )
