module Competences.Document.Resource
  ( ResourceId
  , Resource (..)
  , ResourceIxs
  )
where

import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id)
import Data.Aeson (FromJSON, ToJSON)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import GHC.Generics (Generic)

type ResourceId = Id Resource

data Resource = Resource
  { id :: !ResourceId
  , competences :: ![CompetenceLevelId]
  , name :: !Text
  }
  deriving (Eq, Generic, Ord, Show)

type ResourceIxs = '[ResourceId, CompetenceLevelId]

instance Ix.Indexable ResourceIxs Resource where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun (.competences))

instance FromJSON Resource

instance ToJSON Resource
