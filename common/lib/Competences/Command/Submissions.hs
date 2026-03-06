{-# LANGUAGE CPP #-}

module Competences.Command.Submissions
  ( SubmissionsCommand (..)
  , SubmissionPatch (..)
  , handleSubmissionsCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand (..), ModifyCommand (..), UpdateResult, patchField')
import Competences.Command.Interpret (doLock, doRelease)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..), Assignment (..))
import Competences.Document.FileRef (FileRef)
import Competences.Document.Submission (Submission (..), SubmissionId)
import Competences.Document.User (UserId)
import Control.Monad (when, unless)
import Data.Set qualified as Set
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core ((&), (%~))

-- | Patch for modifying a Submission
data SubmissionPatch = SubmissionPatch
  { description :: !(Change (Maybe Text))
  , files :: !(Change [FileRef])
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Submissions context
data SubmissionsCommand
  = OnSubmissions !(EntityCommand Submission SubmissionPatch)
  deriving (Eq, Generic, Show)

instance Binary SubmissionPatch
#ifdef WITH_AESON
instance FromJSON SubmissionPatch
instance ToJSON SubmissionPatch
#endif

instance Binary SubmissionsCommand
#ifdef WITH_AESON
instance FromJSON SubmissionsCommand
instance ToJSON SubmissionsCommand
#endif

-- | Apply a patch to a Submission
applySubmissionPatch :: Submission -> SubmissionPatch -> Either Text Submission
applySubmissionPatch s p =
  Right s
    >>= patchField' @"description" p
    >>= patchField' @"files" p

-- | Handle a Submissions command.
-- Custom handler because submissions have student-only authorization.
handleSubmissionsCommand :: UserId -> SubmissionsCommand -> Document -> UpdateResult
handleSubmissionsCommand userId (OnSubmissions cmd) d = do
  -- Teachers cannot create submissions
  case Ix.getOne (d.users Ix.@= userId) of
    Nothing -> Left "Submission: user not found"
    Just u -> when (u.role == Teacher) $ Left "Teachers cannot create submissions"
  case cmd of
    Create s -> do
      when (s.userId /= userId) $
        Left "Submission: can only submit as yourself"
      -- Verify assignment exists and includes this student
      case Ix.getOne (d.assignments Ix.@= s.assignmentId) of
        Nothing -> Left "Submission: assignment not found"
        Just a ->
          unless (Set.member userId a.studentIds) $
            Left "Submission: not assigned to this student"
      when (null s.files) $
        Left "Submission: at least one file required"
      unless (Ix.null $ d.submissions Ix.@= s.id) $
        Left "Submission: entity with that id already exists."
      let d' = d & #submissions %~ Ix.insert s
      Right (d', affectedUsersFor s d)

    CreateAndLock s -> do
      when (s.userId /= userId) $
        Left "Submission: can only submit as yourself"
      case Ix.getOne (d.assignments Ix.@= s.assignmentId) of
        Nothing -> Left "Submission: assignment not found"
        Just a ->
          unless (Set.member userId a.studentIds) $
            Left "Submission: not assigned to this student"
      when (null s.files) $
        Left "Submission: at least one file required"
      unless (Ix.null $ d.submissions Ix.@= s.id) $
        Left "Submission: entity with that id already exists."
      let d' = d & #submissions %~ Ix.insert s
      d'' <- doLock userId (SubmissionLock s.id) d'
      Right (d'', affectedUsersFor s d)

    Delete sid -> do
      s <- fetchSubmission sid d
      when (s.userId /= userId) $
        Left "Submission: can only delete your own submission"
      -- TODO: When evidence→submission link exists, check no evidences reference this submission
      let d' = d & #submissions %~ Ix.delete s
      Right (d', affectedUsersFor s d)

    Modify sid Lock -> do
      s <- fetchSubmission sid d
      when (s.userId /= userId) $
        Left "Submission: can only modify your own submission"
      d' <- doLock userId (SubmissionLock sid) d
      Right (d', affectedUsersFor s d)

    Modify sid (Release patch) -> do
      s <- fetchSubmission sid d
      when (s.userId /= userId) $
        Left "Submission: can only modify your own submission"
      -- TODO: When evidence→submission link exists, reject file changes if referenced by evidence
      d' <- doRelease userId (SubmissionLock sid) d
      s' <- applySubmissionPatch s patch
      let d'' = d' & #submissions %~ Ix.insert s' . Ix.deleteIx sid
      Right (d'', affectedUsersFor s d <> affectedUsersFor s' d)

-- | Fetch a submission by ID, or fail
fetchSubmission :: SubmissionId -> Document -> Either Text Submission
fetchSubmission sid d =
  case Ix.getOne (d.submissions Ix.@= sid) of
    Nothing -> Left "Submission: not found"
    Just s -> Right s

-- | Affected users: all teachers + the submitting student
affectedUsersFor :: Submission -> Document -> AffectedUsers
affectedUsersFor s d =
  AffectedUsers $
    map (.id) $
      filter (\u -> u.role == Teacher || u.id == s.userId) $
        Ix.toList d.users
