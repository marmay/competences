module Competences.Document.CompetenceGrid
  ( CompetenceGridId
  , CompetenceGrid (..)
  , emptyCompetenceGrid
  )
where

import Competences.Document.Id (Id, nilId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

type CompetenceGridId = Id CompetenceGrid

-- | Describes an entire competence grid; this is a list of
-- competences with a title and description. It is a grid,
-- because each of the competences has multiple levels; so the
-- grid has a fixed number of rows (the competences) and a fixed
-- number of columns (the levels).
data CompetenceGrid = CompetenceGrid
  { id :: !CompetenceGridId
  -- ^ Unique identifier for the competence grid.
  , title :: !Text
  -- ^ Title of the competence grid.
  , description :: !Text
  -- ^ Description of the competence grid.
  }
  deriving (Eq, Generic, Ord, Show)

emptyCompetenceGrid :: CompetenceGrid
emptyCompetenceGrid = CompetenceGrid nilId "" ""

instance FromJSON CompetenceGrid

instance ToJSON CompetenceGrid

instance Binary CompetenceGrid
