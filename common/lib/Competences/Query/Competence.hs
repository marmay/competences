-- | Competence queries on the Document.
-- Provides reusable lookups for competences by grid, and cross-entity navigation.
module Competences.Query.Competence
  ( -- * Single-entity lookup
    getCompetence
    -- * Grid-scoped queries
  , gridCompetences
  , gridCompetencesSorted
    -- * Cross-entity navigation
  , competenceWithGrid
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Competence (..), CompetenceGrid, CompetenceGridId, CompetenceId, CompetenceIxs, Document (..), Order)
import Data.Proxy (Proxy (..))

-- | Lookup a competence by primary key.
getCompetence :: Document -> CompetenceId -> Maybe Competence
getCompetence doc competenceId = Ix.getOne $ doc.competences Ix.@= competenceId

-- | All competences for a grid (as IxSet for further filtering).
gridCompetences :: Document -> CompetenceGridId -> Ix.IxSet CompetenceIxs Competence
gridCompetences doc gridId = doc.competences Ix.@= gridId

-- | All competences for a grid, sorted by Order.
gridCompetencesSorted :: Document -> CompetenceGridId -> [Competence]
gridCompetencesSorted doc gridId =
  Ix.toAscList (Proxy @Order) $ doc.competences Ix.@= gridId

-- | Lookup a competence and its parent grid (cross-entity navigation).
-- Returns Nothing if either the competence or its grid is not found.
competenceWithGrid :: Document -> CompetenceId -> Maybe (Competence, CompetenceGrid)
competenceWithGrid doc competenceId = do
  c <- Ix.getOne $ doc.competences Ix.@= competenceId
  g <- Ix.getOne $ doc.competenceGrids Ix.@= c.competenceGridId
  pure (c, g)
