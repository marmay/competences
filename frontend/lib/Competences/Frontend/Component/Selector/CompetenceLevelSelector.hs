module Competences.Frontend.Component.Selector.CompetenceLevelSelector
  ( competenceLevelSelectorComponent
  , competenceLevelEditorField
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid
  , Document (..)
  , Level (..)
  , levelDescription
  )
import Competences.Document.Id (Id)
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
import Competences.Frontend.SyncDocument (SyncContext)
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
competenceLevelConfig
  :: (Document -> [(Id Competence, Level)])
  -> MultiStageSelectorStyle
  -> MultiStageSelectorConfig (Id Competence, Level)
competenceLevelConfig initResults style =
  MultiStageSelectorConfig
    { initialState = initialize competenceLevelPipeline
    , errorMessage = C.translate' C.LblPleaseSelectItemShort
    , initResults = initResults
    , validateResults = validateCompetenceLevels
    , viewResult = viewCompetenceLevelResult
    , style = style
    }

-- | Validate that competence-levels still exist in the document
--   Filter out any that have been deleted
validateCompetenceLevels :: Document -> [(Id Competence, Level)] -> [(Id Competence, Level)]
validateCompetenceLevels doc competenceLevels =
  filter (isValidCompetenceLevel doc) competenceLevels
  where
    isValidCompetenceLevel d (competenceId, level) =
      case lookupCompetenceData d competenceId of
        Nothing -> False -- Competence deleted
        Just _ ->
          -- Check if the level is valid (all three levels are always valid)
          level `elem` [BasicLevel, IntermediateLevel, AdvancedLevel]

-- | Extract competence-level display data (pure function)
viewCompetenceLevelResult :: Document -> (Id Competence, Level) -> ResultView
viewCompetenceLevelResult doc (competenceId, level) =
  case lookupCompetenceData doc competenceId of
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

-- | Helper to lookup competence-related data
lookupCompetenceData :: Document -> Id Competence -> Maybe (Competence, CompetenceGrid)
lookupCompetenceData doc competenceId = do
  competence <- Ix.getOne $ doc.competences Ix.@= competenceId
  competenceGrid <- Ix.getOne $ doc.competenceGrids Ix.@= competence.competenceGridId
  pure (competence, competenceGrid)

competenceLevelSelectorComponent
  :: SyncContext
  -> (Document -> [(Id Competence, Level)]) -- Function to load initial values
  -> MultiStageSelectorStyle
  -> SelectorTransformedLens p [] (Id Competence, Level) f' a'
  -> MultiStageSelectorComponent p (Id Competence, Level)
competenceLevelSelectorComponent r initResults style =
  multiStageSelectorComponent r (competenceLevelConfig initResults style)

competenceLevelEditorField
  :: (Ord p, Default patch)
  => SyncContext
  -> M.MisoString
  -> EntityPatchTransformedLens p patch [] (Id Competence, Level) [] (Id Competence, Level)
  -> EditorField p patch f'
competenceLevelEditorField r key eptl =
  selectorEditorField
    key
    eptl
    (\entity style -> competenceLevelSelectorComponent r (\_ -> entity O.^. eptl.viewLens) style)
    ( MultiStageSelectorDisabled
    , MultiStageSelectorEnabled
    )
