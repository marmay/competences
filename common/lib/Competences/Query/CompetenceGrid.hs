-- | CompetenceGrid queries on the Document.
-- Provides reusable lookups for competence grids.
module Competences.Query.CompetenceGrid
  ( allGridsSorted
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (CompetenceGrid, Document (..), Order)
import Data.Proxy (Proxy (..))

-- | All competence grids, sorted by Order.
allGridsSorted :: Document -> [CompetenceGrid]
allGridsSorted doc = Ix.toAscList (Proxy @Order) doc.competenceGrids
