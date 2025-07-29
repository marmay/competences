module Competences.Document.Resource
  ( ResourceId (..)
  , Resource (..)
  , ResourceIxs
  )
where

import Competences.Document.Competence (CompetenceLevelId)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import Data.UUID (UUID)

newtype ResourceId = ResourceId UUID
  deriving (Eq, Ord, Show)

data Resource = Resource
  { id :: !ResourceId
  , competences :: ![CompetenceLevelId]
  , name :: !Text
  }
  deriving (Eq, Ord, Show)

type ResourceIxs = '[ResourceId, CompetenceLevelId]

instance Ix.Indexable ResourceIxs Resource where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun (.competences))
