{-# OPTIONS_GHC -Wno-orphans #-}
module Competences.Document.Evidence
  ( Evidence (..)
  , EvidenceId
  , EvidenceIxs
  , SocialForm (..)
  , Ability (..)
  , evidenceLevel
  )
where

import Competences.Document.Competence (CompetenceId, Level)
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Data.Binary (Binary)

type EvidenceId = Id Evidence

-- | Whether a competence is demonstrated as part of a group or
-- individually.
data SocialForm
  = -- | Competence is demonstrated as part of a group.
    Group
  | -- | Competence is demonstrated individually.
    Individual
  deriving (Eq, Generic, Ord, Show)

-- | Whether the competence was demonstrated self-reliantly,
-- with some support or not yet at all.
data Ability
  = -- | Competence was demonstrated self-reliantly.
    SelfReliant
  | -- | Competence was demonstrated with some support, like
    -- giving a hint or correcting a minor mistake.
    WithSupport
  | -- | Competence was not successfully demonstrated, either
    -- because the student did not try, did not have the correct
    -- idea or they made a significant mistake.
    NotYet
  deriving (Eq, Generic, Ord, Show)

data Evidence = Evidence
  { id :: !EvidenceId
  , userId :: !UserId
  , competence :: !(CompetenceId, Level)
  , date :: !Day
  , description :: !(Maybe Text)
  , socialForm :: !SocialForm
  , ability :: !Ability
  }
  deriving (Eq, Generic, Ord, Show)

evidenceLevel :: Evidence -> Level
evidenceLevel = snd . (.competence)

type EvidenceIxs = '[EvidenceId, UserId, CompetenceId, Level, Day]

instance Ix.Indexable EvidenceIxs Evidence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.userId))
      (Ix.ixFun $ singleton . fst . (.competence))
      (Ix.ixFun $ singleton . snd . (.competence))
      (Ix.ixFun $ singleton . (.date))

instance FromJSON SocialForm

instance ToJSON SocialForm

instance Binary SocialForm

instance FromJSON Ability

instance ToJSON Ability

instance Binary Ability

instance FromJSON Evidence

instance ToJSON Evidence

instance Binary Day

instance Binary Evidence
