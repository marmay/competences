module Competences.Model.Evidence
  ( Evidence (..)
  , EvidenceIxs
  , SocialForm (..)
  , Ability (..)
  )
where

import Competences.Model.Competence (CompetenceId)
import Competences.Model.User (UserId)
import Data.IxSet.Typed qualified as Ix
import Data.Text (Text)
import Data.Time (Day)
import Data.UUID (UUID)
import Data.List (singleton)

newtype EvidenceId = EvidenceId UUID
  deriving (Eq, Ord, Show)

-- | Whether a competence is demonstrated as part of a group or
-- individually.
data SocialForm
  = -- | Competence is demonstrated as part of a group.
    Group
  | -- | Competence is demonstrated individually.
    Individual
  deriving (Eq, Show, Ord)

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
  deriving (Eq, Show, Ord)

-- | Level of a competence.
data Level
  = -- | Basic level of competence; the essentials.
    BasicLevel
  | -- | Intermediate level; slightly going above the essentials.
    IntermediateLevel
  | -- | Advanced level; mastering the given competence in terms
    -- of the current curriculum.
    AdvancedLevel
  deriving (Eq, Show, Ord)

data Evidence = Evidence
  { id :: !EvidenceId
  , userId :: !UserId
  , competenceId :: !CompetenceId
  , level :: !Level
  , date :: !Day
  , description :: !(Maybe Text)
  , socialForm :: !SocialForm
  , ability :: !Ability
  }
  deriving (Eq, Ord, Show)

type EvidenceIxs = '[EvidenceId, UserId, CompetenceId, Level]

instance Ix.Indexable EvidenceIxs Evidence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.userId))
      (Ix.ixFun $ singleton . (.competenceId))
      (Ix.ixFun $ singleton . (.level))
