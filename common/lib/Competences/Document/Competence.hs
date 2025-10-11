module Competences.Document.Competence
  ( CompetenceId
  , Competence (..)
  , CompetenceLevelId
  , CompetenceIxs
  , Level (..)
  , levels
  , competenceLevelIdsOf
  )
where

import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Id (Id)
import Competences.Document.Order (Order, Orderable)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary (Binary)
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
  , order :: !Order
  , description :: !Text
  , levelDescriptions :: !(M.Map Level Text)
  }
  deriving (Eq, Generic, Ord, Show)

-- | List of all levels in increasing order of competence.
levels :: [Level]
levels = [BasicLevel, IntermediateLevel, AdvancedLevel]

competenceLevelIdsOf :: Competence -> [CompetenceLevelId]
competenceLevelIdsOf competence =
   map (competence.id,) $ M.keys competence.levelDescriptions

type CompetenceLevelId = (CompetenceId, Level)

type CompetenceIxs = '[CompetenceId, Order, CompetenceGridId]

instance Ix.Indexable CompetenceIxs Competence where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.order))
      (Ix.ixFun $ singleton . (.competenceGridId))

instance FromJSON Level

instance ToJSON Level

instance Binary Level

instance FromJSONKey Level

instance ToJSONKey Level

instance FromJSON Competence

instance ToJSON Competence

instance Binary Competence

instance Orderable Competence
