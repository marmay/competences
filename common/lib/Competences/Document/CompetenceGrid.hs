module Competences.Document.CompetenceGrid
  ( CompetenceGridId
  , CompetenceGrid (..)
  , CompetenceGridIxs
  , emptyCompetenceGrid
  )
where

import Competences.Common.BinaryOrphans ()
import Competences.Common.IxSet qualified as Ix
import Competences.Document.Id (Id, nilId)
import Competences.Document.Order (Order, orderMax, Orderable)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Binary (Binary)
import Data.List (singleton)
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
  , order :: !Order
  -- ^ Order in which competence grids are displayed.
  , title :: !Text
  -- ^ Title of the competence grid.
  , description :: !Text
  -- ^ Description of the competence grid.
  }
  deriving (Eq, Generic, Ord, Show)

type CompetenceGridIxs = '[CompetenceGridId, Order]

instance Ix.Indexable CompetenceGridIxs CompetenceGrid where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.order))

emptyCompetenceGrid :: CompetenceGrid
emptyCompetenceGrid = CompetenceGrid nilId orderMax "" ""

-- Custom FromJSON ignores legacy dateFrom/dateTo/expectedLessons fields
instance FromJSON CompetenceGrid where
  parseJSON = withObject "CompetenceGrid" $ \v ->
    CompetenceGrid
      <$> v .: "id"
      <*> v .: "order"
      <*> v .: "title"
      <*> v .: "description"

instance ToJSON CompetenceGrid where
  toJSON g =
    object
      [ "id" .= g.id
      , "order" .= g.order
      , "title" .= g.title
      , "description" .= g.description
      ]

instance Binary CompetenceGrid

instance Orderable CompetenceGrid
