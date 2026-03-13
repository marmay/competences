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
import Competences.Document.Submission (Submission (..), SubmissionId, SubmissionKind (..), SubmissionOwnership (..), ownerIds)
import Competences.Document.User (UserId)
import Control.Monad (when, unless)
import Data.Set qualified as Set
import Data.Text qualified as T
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core ((&), (%~))

-- | Patch for modifying a Submission
data SubmissionPatch = SubmissionPatch
  { kind :: !(Change SubmissionKind)
  , remark :: !(Change (Maybe Text))
  , ownership :: !(Change SubmissionOwnership)
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
    >>= patchField' @"kind" p
    >>= patchField' @"remark" p
    >>= patchField' @"ownership" p

-- | Check if the acting user is an owner of the submission.
isOwner :: UserId -> Submission -> Bool
isOwner uid s = uid `elem` ownerIds s.ownership

-- | Validate a submission kind.
validateKind :: SubmissionKind -> Either Text ()
validateKind (DigitalSubmission files) =
  when (null files) $ Left "Submission: at least one file required for digital submission"
validateKind (NonDigitalSubmission _) = Right ()
validateKind (VoidSubmission reason) =
  when (T.null (T.strip reason)) $ Left "Submission: void submission requires a non-empty reason"

-- | Validate that all owners are students assigned to this assignment.
validateOwnership :: SubmissionOwnership -> UserId -> Assignment -> Document -> Either Text ()
validateOwnership own actingUserId assignment d = do
  -- Reject collaborative submissions when not allowed
  case own of
    CollaborativeSubmission _ ->
      unless assignment.groupSubmissionAllowed $
        Left "Submission: collaborative submissions not allowed for this assignment"
    _ -> pure ()
  let owners = ownerIds own
  -- Acting user must be in ownership list
  unless (actingUserId `elem` owners) $
    Left "Submission: submitting user must be in ownership list"
  -- All owners must be students assigned to this assignment
  mapM_ (\uid -> do
    case Ix.getOne (d.users Ix.@= uid) of
      Nothing -> Left "Submission: owner user not found"
      Just u -> when (u.role == Teacher) $ Left "Teachers cannot own submissions"
    unless (Set.member uid assignment.studentIds) $
      Left "Submission: owner not assigned to this assignment"
    ) owners

-- | Validate void submissions: can't create if non-void submissions exist for this assignment+user.
validateVoidConstraint :: SubmissionKind -> Submission -> Document -> Either Text ()
validateVoidConstraint (VoidSubmission _) s d =
  let existing = Ix.toList $ d.submissions Ix.@= s.assignmentId
      userOwns sub = any (`elem` ownerIds sub.ownership) (ownerIds s.ownership)
      nonVoidExists = any (\sub -> userOwns sub && not (isVoid sub.kind)) existing
   in when nonVoidExists $
        Left "Submission: cannot create void submission when non-void submissions exist"
  where
    isVoid (VoidSubmission _) = True
    isVoid _ = False
validateVoidConstraint _ _ _ = Right ()

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
      unless (isOwner userId s) $
        Left "Submission: can only submit as yourself"
      -- Verify assignment exists and validate ownership
      case Ix.getOne (d.assignments Ix.@= s.assignmentId) of
        Nothing -> Left "Submission: assignment not found"
        Just a -> validateOwnership s.ownership userId a d
      validateKind s.kind
      validateVoidConstraint s.kind s d
      unless (Ix.null $ d.submissions Ix.@= s.id) $
        Left "Submission: entity with that id already exists."
      let d' = d & #submissions %~ Ix.insert s
      Right (d', affectedUsersFor s d)

    CreateAndLock s -> do
      unless (isOwner userId s) $
        Left "Submission: can only submit as yourself"
      case Ix.getOne (d.assignments Ix.@= s.assignmentId) of
        Nothing -> Left "Submission: assignment not found"
        Just a -> validateOwnership s.ownership userId a d
      validateKind s.kind
      validateVoidConstraint s.kind s d
      unless (Ix.null $ d.submissions Ix.@= s.id) $
        Left "Submission: entity with that id already exists."
      let d' = d & #submissions %~ Ix.insert s
      d'' <- doLock userId (SubmissionLock s.id) d'
      Right (d'', affectedUsersFor s d)

    Delete sid -> do
      s <- fetchSubmission sid d
      unless (isOwner userId s) $
        Left "Submission: can only delete your own submission"
      let d' = d & #submissions %~ Ix.delete s
      Right (d', affectedUsersFor s d)

    Modify sid Lock -> do
      s <- fetchSubmission sid d
      unless (isOwner userId s) $
        Left "Submission: can only modify your own submission"
      d' <- doLock userId (SubmissionLock sid) d
      Right (d', affectedUsersFor s d)

    Modify sid (Release patch) -> do
      s <- fetchSubmission sid d
      unless (isOwner userId s) $
        Left "Submission: can only modify your own submission"
      d' <- doRelease userId (SubmissionLock sid) d
      s' <- applySubmissionPatch s patch
      validateKind s'.kind
      let d'' = d' & #submissions %~ Ix.insert s' . Ix.deleteIx sid
      Right (d'', affectedUsersFor s d <> affectedUsersFor s' d)

-- | Fetch a submission by ID, or fail
fetchSubmission :: SubmissionId -> Document -> Either Text Submission
fetchSubmission sid d =
  case Ix.getOne (d.submissions Ix.@= sid) of
    Nothing -> Left "Submission: not found"
    Just s -> Right s

-- | Affected users: all teachers + all owners of the submission
affectedUsersFor :: Submission -> Document -> AffectedUsers
affectedUsersFor s d =
  let owners = Set.fromList (ownerIds s.ownership)
   in AffectedUsers $
        map (.id) $
          filter (\u -> u.role == Teacher || Set.member u.id owners) $
            Ix.toList d.users
