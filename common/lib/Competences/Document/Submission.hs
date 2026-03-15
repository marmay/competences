{-# LANGUAGE CPP #-}

module Competences.Document.Submission
  ( SubmissionId
  , Submission (..)
  , SubmissionIxs
  , SubmissionKind (..)
  , SubmissionOwnership (..)
  , VoidReason (..)
  , simpleVoidReasons
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
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.=))
import Data.Aeson.Types (Parser)
import Control.Applicative ((<|>))
import Data.Text qualified as T
#endif
import Data.Binary (Binary)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

type SubmissionId = Id Submission

-- | Structured reason for a void submission.
data VoidReason
  = VoidSick
  | VoidTooEasy
  | VoidTooHard
  | VoidNoLongerRelevant
  | VoidOther !Text
  deriving (Eq, Generic, Ord, Show)

instance Binary VoidReason

-- | All simple (non-free-text) void reasons.
simpleVoidReasons :: [VoidReason]
simpleVoidReasons = [VoidSick, VoidTooEasy, VoidTooHard, VoidNoLongerRelevant]

#ifdef WITH_AESON
instance ToJSON VoidReason where
  toJSON VoidSick = String "Sick"
  toJSON VoidTooEasy = String "TooEasy"
  toJSON VoidTooHard = String "TooHard"
  toJSON VoidNoLongerRelevant = String "NoLongerRelevant"
  toJSON (VoidOther t) = object ["Other" .= t]

instance FromJSON VoidReason where
  parseJSON (String "Sick") = pure VoidSick
  parseJSON (String "TooEasy") = pure VoidTooEasy
  parseJSON (String "TooHard") = pure VoidTooHard
  parseJSON (String "NoLongerRelevant") = pure VoidNoLongerRelevant
  parseJSON (Object o) = VoidOther <$> o .: "Other"
  parseJSON _ = fail "Invalid VoidReason"
#endif

-- | The kind of submission a student makes.
data SubmissionKind
  = DigitalSubmission ![FileRef]       -- ^ At least one file required
  | NonDigitalSubmission !(Maybe Text) -- ^ Optional location ("Heft S. 42")
  | VoidSubmission !VoidReason         -- ^ Required reason why not completed
  deriving (Eq, Generic, Ord, Show)

instance Binary SubmissionKind
#ifdef WITH_AESON
instance ToJSON SubmissionKind

instance FromJSON SubmissionKind where
  parseJSON = withObject "SubmissionKind" $ \v -> do
    tag <- v .: "tag" :: Parser Text
    case tag of
      "DigitalSubmission" -> DigitalSubmission <$> v .: "contents"
      "NonDigitalSubmission" -> NonDigitalSubmission <$> v .: "contents"
      "VoidSubmission" -> do
        contents <- v .: "contents"
        VoidSubmission <$> (parseJSON contents <|> withText "old" (pure . VoidOther) contents)
      _ -> fail $ "Unknown SubmissionKind: " <> T.unpack tag
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
