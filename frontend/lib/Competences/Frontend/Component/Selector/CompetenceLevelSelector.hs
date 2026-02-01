module Competences.Frontend.Component.Selector.CompetenceLevelSelector
  ( competenceLevelSelectorComponent
  , competenceLevelEditorField
  , formatCompetenceLevelBadge
  , ResultView (..)
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , CompetenceGridIxs
  , Document (..)
  , Level (..)
  , levelDescription
  )
import Competences.Document.Competence (CompetenceIxs)
import Competences.Document.Id (Id)
import Competences.Query.Competence (competenceWithGridIx)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorField)
import Competences.Frontend.Component.Selector.Common
  ( EntityPatchTransformedLens (..)
  , SelectorTransformedLens
  )
import Competences.Frontend.Component.Selector.MultiStageSelector
  ( HList (..)
  , IncrementalParserSpec (..)
  , MultiStageSelectorComponent
  , MultiStageSelectorConfig (..)
  , MultiStageSelectorStyle (..)
  , Pipeline
  , ResultView (..)
  , StageKind (..)
  , done
  , initialize
  , multiStageSelectorComponent
  , stage'
  )
import Competences.Frontend.Component.Selector.ObservationSelector
  ( competenceGridP
  , competenceP
  , levelP
  )
import Competences.Frontend.SyncContext (SyncContext)
import Data.Default (Default)
import Data.List (intercalate)
import Miso qualified as M
import Optics.Core qualified as O

-- | The competence-level pipeline: 3 stages leading to (Id Competence, Level)
--
-- This is simpler than ObservationSelector - no ID generation needed!
competenceLevelPipeline :: Pipeline 'IsStage '[] CompetenceGrid (Id Competence, Level)
competenceLevelPipeline =
  stage' competenceGridP $ \cg ->
    stage' (competenceP cg) $ \c ->
      stage' (levelP c) $ \_ ->
        done $ \(HCons l' (HCons c' (HCons _cg HNil))) ->
          pure (c'.id, l')

-- | Configuration for competence-level selector
--   Takes an initResults function to load initial values
--   minResultsCount specifies the minimum number of results required (delete button hidden when at minimum)
competenceLevelConfig
  :: (Document -> [(Id Competence, Level)])
  -> MultiStageSelectorStyle
  -> Int  -- ^ Minimum number of results required
  -> MultiStageSelectorConfig (Id Competence, Level)
competenceLevelConfig initResults style minResultsCount =
  MultiStageSelectorConfig
    { initialState = initialize competenceLevelPipeline
    , errorMessage = C.translate' C.LblPleaseSelectItemShort
    , initResults = initResults
    , validateResults = validateCompetenceLevels
    , viewResult = viewCompetenceLevelResult
    , style = style
    , minResults = minResultsCount
    }

-- | Validate that competence-levels still exist in the document
--   Filter out any that have been deleted
validateCompetenceLevels :: Document -> [(Id Competence, Level)] -> [(Id Competence, Level)]
validateCompetenceLevels doc competenceLevels =
  filter (isValidCompetenceLevel doc) competenceLevels
  where
    isValidCompetenceLevel d (competenceId, level) =
      case competenceWithGridIx d.competences d.competenceGrids competenceId of
        Nothing -> False -- Competence deleted
        Just _ ->
          -- Check if the level is valid (all three levels are always valid)
          level `elem` [BasicLevel, IntermediateLevel, AdvancedLevel]

-- | Format a competence-level pair as a compact badge with tooltip.
--
-- Core logic extracted from 'viewCompetenceLevelResult' so it can be
-- used with projected IxSets (without requiring a full Document).
formatCompetenceLevelBadge
  :: Ix.IxSet CompetenceIxs Competence
  -> Ix.IxSet CompetenceGridIxs CompetenceGrid
  -> (Id Competence, Level)
  -> ResultView
formatCompetenceLevelBadge comps grids (competenceId, level) =
  case competenceWithGridIx comps grids competenceId of
    Nothing ->
      ResultView
        { badgeText = "???"
        , tooltipContent = Just "Die Kompetenz existiert nicht länger."
        }
    Just (competence, competenceGrid) ->
      let competenceGridLabel = competenceGridP.reconstructInput competenceGrid
          competenceLabel = (competenceP competenceGrid).reconstructInput competence
          levelLabel = (levelP competence).reconstructInput level
          label = M.ms $ intercalate "." [competenceGridLabel, competenceLabel, levelLabel]
          tooltipText =
            M.ms competenceLabel
              <> ": "
              <> M.ms competence.description
              <> "\n"
              <> M.ms levelLabel
              <> ": "
              <> M.ms (levelDescription level competence)
       in ResultView
            { badgeText = label
            , tooltipContent = Just tooltipText
            }

-- | Thin wrapper for use in 'MultiStageSelectorConfig' which requires @Document@.
viewCompetenceLevelResult :: Document -> (Id Competence, Level) -> ResultView
viewCompetenceLevelResult doc =
  formatCompetenceLevelBadge doc.competences doc.competenceGrids

competenceLevelSelectorComponent
  :: SyncContext
  -> (Document -> [(Id Competence, Level)]) -- Function to load initial values
  -> MultiStageSelectorStyle
  -> Int  -- ^ Minimum number of results required
  -> SelectorTransformedLens p [] (Id Competence, Level) f' a'
  -> MultiStageSelectorComponent p (Id Competence, Level)
competenceLevelSelectorComponent r initResults style minResultsCount =
  multiStageSelectorComponent r (competenceLevelConfig initResults style minResultsCount)

competenceLevelEditorField
  :: (Ord p, Default patch)
  => SyncContext
  -> M.MisoString
  -> Int  -- ^ Minimum number of results required
  -> EntityPatchTransformedLens p patch [] (Id Competence, Level) [] (Id Competence, Level)
  -> EditorField p patch f'
competenceLevelEditorField r key minResultsCount eptl =
  selectorEditorField
    key
    eptl
    (\entity style -> competenceLevelSelectorComponent r (\_ -> entity O.^. eptl.viewLens) style minResultsCount)
    ( MultiStageSelectorDisabled
    , MultiStageSelectorEnabled
    )
