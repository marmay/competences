-- | CompetenceGrid queries on the Document.
-- Provides reusable lookups for competence grids.
module Competences.Query.CompetenceGrid
  ( -- * Single-entity lookup
    getGrid
    -- * All grids
  , allGridsSorted
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (CompetenceGrid, CompetenceGridId, Document (..), Order)
import Data.Proxy (Proxy (..))

-- | Lookup a competence grid by primary key.
getGrid :: Document -> CompetenceGridId -> Maybe CompetenceGrid
getGrid doc gridId = Ix.getOne $ doc.competenceGrids Ix.@= gridId

-- | All competence grids, sorted by Order.
allGridsSorted :: Document -> [CompetenceGrid]
allGridsSorted doc = Ix.toAscList (Proxy @Order) doc.competenceGrids
