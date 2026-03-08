{-# LANGUAGE CPP #-}

module Competences.Document.Submission
  ( SubmissionId
  , Submission (..)
  , SubmissionIxs
  , SubmissionKind (..)
  , SubmissionOwnership (..)
  , ownerIds
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Common.IxSet qualified as Ix
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.FileRef (FileRef)
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

type SubmissionId = Id Submission

-- | The kind of submission a student makes.
data SubmissionKind
  = DigitalSubmission ![FileRef]       -- ^ At least one file required
  | NonDigitalSubmission !(Maybe Text) -- ^ Optional location ("Heft S. 42")
  | VoidSubmission !Text               -- ^ Required reason why not completed
  deriving (Eq, Generic, Ord, Show)

instance Binary SubmissionKind
#ifdef WITH_AESON
instance FromJSON SubmissionKind
instance ToJSON SubmissionKind
#endif

-- | Who owns the submission.
data SubmissionOwnership
  = IndividualSubmission !UserId
  | CollaborativeSubmission !(NonEmpty UserId)  -- ^ All listed students co-own equally
  deriving (Eq, Generic, Ord, Show)

instance Binary SubmissionOwnership
#ifdef WITH_AESON
instance FromJSON SubmissionOwnership
instance ToJSON SubmissionOwnership
#endif

-- | Extract all owner UserIds from an ownership value.
ownerIds :: SubmissionOwnership -> [UserId]
ownerIds (IndividualSubmission uid) = [uid]
ownerIds (CollaborativeSubmission uids) = toList uids

-- | A student submission for an assignment.
data Submission = Submission
  { id :: !SubmissionId
  , assignmentId :: !AssignmentId
  , ownership :: !SubmissionOwnership
  , kind :: !SubmissionKind
  , remark :: !(Maybe Text)
  , submittedAt :: !UTCTime
  }
  deriving (Eq, Generic, Ord, Show)

type SubmissionIxs = '[SubmissionId, AssignmentId, UserId]

instance Ix.Indexable SubmissionIxs Submission where
  indices =
    Ix.ixList
      (Ix.ixFun $ \s -> [s.id])
      (Ix.ixFun $ \s -> [s.assignmentId])
      (Ix.ixFun $ \s -> ownerIds s.ownership)

#ifdef WITH_AESON
instance FromJSON Submission

instance ToJSON Submission
#endif

instance Binary Submission
