-- | Example usage of the generic Pipeline selector
--
-- This module demonstrates how to construct pipelines and shows
-- that the existential type machinery works correctly.
module Competences.Frontend.Component.Selector.Example where

import Competences.Document (Competence (..), CompetenceGrid (..), Document, Level)
import Competences.Document.Id (Id)
import Competences.Frontend.Component.Selector.MultiStageSelector
  ( HList (..)
  , Pipeline (..)
  , RuntimeState
  , initialize
  )
import Competences.Frontend.Component.Selector.ObservationSelector
  ( competenceGridP
  , competenceP
  , levelP
  )

-- | Simple 2-stage pipeline: CompetenceGrid -> Competence -> (Id Competence, CompetenceGrid)
--
-- This demonstrates:
-- 1. How to construct a pipeline with multiple stages
-- 2. How context flows through stages (grows as we nest)
-- 3. How closures and HList parameters work together
--
-- Type breakdown:
-- - Pipeline '[] CompetenceGrid (...) means: start with empty context, select CompetenceGrid
-- - Pipeline '[CompetenceGrid] Competence (...) means: with CompetenceGrid in context, select Competence
simplePipeline :: Pipeline '[] CompetenceGrid (Id Competence, CompetenceGrid)
simplePipeline =
  Stage competenceGridP $ \HNil cg ->
    Done (competenceP cg) $ \(HCons cg' HNil) c ->
      (c.id, cg')

-- | Three-stage pipeline: CompetenceGrid -> Competence -> Level -> (Id Competence, Level)
--
-- Demonstrates a longer pipeline with more context accumulation
competenceLevelPipeline :: Pipeline '[] CompetenceGrid (Id Competence, Level)
competenceLevelPipeline =
  Stage competenceGridP $ \HNil cg ->
    Stage (competenceP cg) $ \(HCons _cg' HNil) c ->
      Done (levelP c) $ \(HCons c' (HCons _cg'' HNil)) l ->
        (c'.id, l)

-- | Initialize a simple pipeline
--
-- This demonstrates that the initialize function works with our pipeline
-- and creates the correct RuntimeState with existential types properly bundled.
exampleInitialize :: Document -> RuntimeState (Id Competence, CompetenceGrid)
exampleInitialize = initialize simplePipeline

-- | Initialize the three-stage pipeline
exampleInitialize3 :: Document -> RuntimeState (Id Competence, Level)
exampleInitialize3 = initialize competenceLevelPipeline

-- Note: We can't easily demonstrate advanceIfReady here because we'd need
-- to simulate user input and selections. The key point is that this compiles,
-- which proves that the existential type machinery works correctly!
--
-- The fact that we can:
-- 1. Define pipelines with different stages and result types
-- 2. Initialize them to RuntimeState
-- 3. Have GHC accept all the type signatures
--
-- ...proves that the existential type bundling in CurrentStage is working.
