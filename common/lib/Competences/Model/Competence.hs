module Competences.Model.Competence
  ( CompetenceId
  , Competence (..)
  , CompetenceLevelId
  , CompetenceIxs
  , Level (..)
  )
where

import Competences.Model.CompetenceGrid (CompetenceGridId)
import Competences.Model.Id (Id)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)

type CompetenceId = Id Competence

-- | Level of a competence.
data Level
  = -- | Basic level of competence; the essentials.
    BasicLevel
  | -- | Intermediate level; slightly going above the essentials.
    IntermediateLevel
  | -- | Advanced level; mastering the given competence in terms
    -- of the current curriculum.
    AdvancedLevel
  deriving (Eq, Generic, Ord, Show)

data Competence = Competence
  { id :: !CompetenceId
  , competenceGridId :: !CompetenceGridId
  , description :: !Text
  , levelDescriptions :: !(M.Map Level Text)
  }
  deriving (Eq, Generic, Ord, Show)

type CompetenceLevelId = (CompetenceId, Level)

type CompetenceIxs = '[CompetenceId, CompetenceGridId]

instance Ix.Indexable CompetenceIxs Competence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.competenceGridId))

instance FromJSON Level

instance ToJSON Level

instance FromJSONKey Level

instance ToJSONKey Level

instance FromJSON Competence

instance ToJSON Competence
