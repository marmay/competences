module Competences.Command.Competences
  ( CompetencesCommand (..)
  , CompetenceGridPatch (..)
  , CompetencePatch (..)
  )
where

import Competences.Command.Common (Change, EntityCommand)
import Competences.Document.Competence (Competence, Level)
import Competences.Document.CompetenceGrid (CompetenceGrid)
import Competences.Document.Order (OrderPosition, Reorder)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Patch for modifying a CompetenceGrid (only editable fields)
data CompetenceGridPatch = CompetenceGridPatch
  { title :: !(Change Text)
    -- ^ Change title from old to new value
  , description :: !(Change Text)
    -- ^ Change description from old to new value
  }
  deriving (Eq, Generic, Show)

-- | Patch for modifying a Competence (only editable fields)
data CompetencePatch = CompetencePatch
  { description :: !(Change Text)
    -- ^ Change description from old to new value
  , levelDescriptions :: !(Change (Map Level Text))
    -- ^ Change level descriptions from old to new value
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Competences context (CompetenceGrids and Competences)
data CompetencesCommand
  = OnCompetenceGrids !(EntityCommand CompetenceGrid CompetenceGridPatch)
  | OnCompetences !(EntityCommand Competence CompetencePatch)
  | ReorderCompetence !(OrderPosition Competence) !(Reorder Competence)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON CompetenceGridPatch
instance ToJSON CompetenceGridPatch
instance Binary CompetenceGridPatch

instance FromJSON CompetencePatch
instance ToJSON CompetencePatch
instance Binary CompetencePatch

instance FromJSON CompetencesCommand
instance ToJSON CompetencesCommand
instance Binary CompetencesCommand
