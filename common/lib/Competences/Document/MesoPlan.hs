module Competences.Document.MesoPlan
  ( MesoPlanId
  , MesoPlan (..)
  , MesoPlanIxs
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.List (singleton)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- ============================================================================
-- MesoPlan
-- ============================================================================

type MesoPlanId = Id MesoPlan

-- | Container for meso-level planning.
-- Standalone entity - not linked to a specific CompetenceGrid.
-- Individual lessons are linked via Lesson.mesoPlanId.
data MesoPlan = MesoPlan
  { id :: !MesoPlanId
  , title :: !Text
  , dateFrom :: !(Maybe Day)
  -- ^ When teaching this plan period starts
  , dateTo :: !(Maybe Day)
  -- ^ When teaching this plan period ends
  }
  deriving (Eq, Generic, Ord, Show)

type MesoPlanIxs = '[MesoPlanId]

instance Ix.Indexable MesoPlanIxs MesoPlan where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))

instance FromJSON MesoPlan

instance ToJSON MesoPlan

instance Binary MesoPlan
