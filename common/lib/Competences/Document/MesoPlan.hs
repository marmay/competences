module Competences.Document.MesoPlan
  ( -- * MesoPlan
    MesoPlanId
  , MesoPlan (..)
  , MesoPlanIxs
    -- * MesoPlanEntry
  , MesoPlanEntryId
  , MesoPlanEntry (..)
  , MesoPlanEntryIxs
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Id (Id)
import Competences.Document.Order (Order, Orderable)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- ============================================================================
-- MesoPlan
-- ============================================================================

type MesoPlanId = Id MesoPlan

-- | Container for meso-level planning, linked to a CompetenceGrid.
-- Usually one per grid, but the link is not enforced as 1:1.
data MesoPlan = MesoPlan
  { id :: !MesoPlanId
  , competenceGridId :: !CompetenceGridId
  , title :: !Text
  , dateFrom :: !(Maybe Day)
  -- ^ When teaching this plan period starts
  , dateTo :: !(Maybe Day)
  -- ^ When teaching this plan period ends
  }
  deriving (Eq, Generic, Ord, Show)

type MesoPlanIxs = '[MesoPlanId, CompetenceGridId]

instance Ix.Indexable MesoPlanIxs MesoPlan where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.competenceGridId))

-- Custom FromJSON for backward compatibility (old docs won't have dateFrom/dateTo)
instance FromJSON MesoPlan where
  parseJSON = withObject "MesoPlan" $ \v ->
    MesoPlan
      <$> v .: "id"
      <*> v .: "competenceGridId"
      <*> v .: "title"
      <*> v .:? "dateFrom" .!= Nothing
      <*> v .:? "dateTo" .!= Nothing

instance ToJSON MesoPlan where
  toJSON p =
    object
      [ "id" .= p.id
      , "competenceGridId" .= p.competenceGridId
      , "title" .= p.title
      , "dateFrom" .= p.dateFrom
      , "dateTo" .= p.dateTo
      ]

instance Binary MesoPlan

-- ============================================================================
-- MesoPlanEntry
-- ============================================================================

type MesoPlanEntryId = Id MesoPlanEntry

-- | Individual entry in a meso plan. Ordered sequence describing teaching units.
-- Each entry can have at most one linked LessonPlan.
data MesoPlanEntry = MesoPlanEntry
  { id :: !MesoPlanEntryId
  , mesoPlanId :: !MesoPlanId
  , order :: !Order
  , title :: !Text
  , description :: !Text
  , competenceLevels :: ![CompetenceLevelId]
  }
  deriving (Eq, Generic, Ord, Show)

type MesoPlanEntryIxs = '[MesoPlanEntryId, MesoPlanId, Order]

instance Ix.Indexable MesoPlanEntryIxs MesoPlanEntry where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.mesoPlanId))
      (Ix.ixFun $ singleton . (.order))

instance FromJSON MesoPlanEntry

instance ToJSON MesoPlanEntry

instance Binary MesoPlanEntry

instance Orderable MesoPlanEntry
