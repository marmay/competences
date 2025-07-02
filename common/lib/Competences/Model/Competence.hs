module Competences.Model.Competence
  ( CompetenceId (..)
  , Competence (..)
  , CompetenceIxs
  )
where

import Competences.Model.CompetenceGrid (CompetenceGridId)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.IxSet.Typed as Ix
import Data.List (singleton)

newtype CompetenceId = CompetenceId UUID
  deriving (Eq, Ord, Show)

data Competence = Competence
  { id :: !CompetenceId
  , competenceGridId :: !CompetenceGridId
  , description :: !Text
  , basicDescription :: !(Maybe Text)
  , intermediateDescription :: !(Maybe Text)
  , advancedDescription :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show)

type CompetenceIxs = '[ CompetenceId, CompetenceGridId ]

instance Ix.Indexable CompetenceIxs Competence where
  indices = Ix.ixList
              (Ix.ixFun $ singleton . (.id))
              (Ix.ixFun $ singleton . (.competenceGridId))
