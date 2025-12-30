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
  )
import Competences.Document.Id (Id)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorField)
import Competences.Frontend.Component.Selector.Common
  ( EntityPatchTransformedLens
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
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Data.Default (Default)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Miso qualified as M

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
competenceLevelConfig :: MultiStageSelectorStyle -> MultiStageSelectorConfig (Id Competence, Level)
competenceLevelConfig style =
  MultiStageSelectorConfig
    { initialState = initialize competenceLevelPipeline
    , errorMessage = C.translate' C.LblPleaseSelectItemShort
    , updateResults = \_ currentResults -> currentResults -- Just preserve current results
    , viewResult = viewCompetenceLevelResult
    , style = style
    }

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
              <> M.ms (fromMaybe "" (competence.levelDescriptions Map.!? level))
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
  :: SyncDocumentRef
  -> MultiStageSelectorStyle
  -> SelectorTransformedLens p [] (Id Competence, Level) f' a'
  -> MultiStageSelectorComponent p (Id Competence, Level)
competenceLevelSelectorComponent r style =
  multiStageSelectorComponent r (competenceLevelConfig style)

competenceLevelEditorField
  :: (Ord p, Default patch)
  => SyncDocumentRef
  -> M.MisoString
  -> EntityPatchTransformedLens p patch [] (Id Competence, Level) f t
  -> EditorField p patch f'
competenceLevelEditorField r key eptl =
  selectorEditorField
    key
    eptl
    (\_ -> competenceLevelSelectorComponent r)
    ( MultiStageSelectorDisabled
    , MultiStageSelectorEnabled
    )
