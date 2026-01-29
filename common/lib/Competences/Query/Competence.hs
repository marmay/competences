-- | Competence queries on the Document.
-- Provides reusable lookups for competences by grid.
module Competences.Query.Competence
  ( gridCompetences
  , gridCompetencesSorted
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Competence, CompetenceGridId, CompetenceIxs, Document (..), Order)
import Data.Proxy (Proxy (..))

-- | All competences for a grid (as IxSet for further filtering).
gridCompetences :: Document -> CompetenceGridId -> Ix.IxSet CompetenceIxs Competence
gridCompetences doc gridId = doc.competences Ix.@= gridId

-- | All competences for a grid, sorted by Order.
gridCompetencesSorted :: Document -> CompetenceGridId -> [Competence]
gridCompetencesSorted doc gridId =
  Ix.toAscList (Proxy @Order) $ doc.competences Ix.@= gridId
