{-# LANGUAGE CPP #-}

module Competences.Document.Submission
  ( SubmissionId
  , Submission (..)
  , SubmissionIxs
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
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

type SubmissionId = Id Submission

-- | A student submission for an assignment.
-- Contains uploaded files, an optional description, and a timestamp.
data Submission = Submission
  { id :: !SubmissionId
  , assignmentId :: !AssignmentId
  , userId :: !UserId
  , files :: ![FileRef]
  , description :: !(Maybe Text)
  , submittedAt :: !UTCTime
  }
  deriving (Eq, Generic, Ord, Show)

type SubmissionIxs = '[SubmissionId, AssignmentId, UserId]

instance Ix.Indexable SubmissionIxs Submission where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.assignmentId))
      (Ix.ixFun $ singleton . (.userId))

#ifdef WITH_AESON
instance FromJSON Submission

instance ToJSON Submission
#endif

instance Binary Submission
