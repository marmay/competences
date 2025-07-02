module Competences.Model.CompetenceGrid
  ( CompetenceGridId (..)
  , CompetenceGrid (..)
  )
where

import Data.Text (Text)
import Data.UUID (UUID)

newtype CompetenceGridId = CompetenceGridId UUID
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Show)
