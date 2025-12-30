{-# LANGUAGE UndecidableInstances #-}

module Competences.Frontend.Component.Selector.ObservationSelector
  ( observationSelectorComponent
  , observationEditorField

    -- * Exported parsers for examples
  , competenceGridP
  , competenceP
  , levelP
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Evidence (..)
  , EvidenceId
  , Level (..)
  , Order
  , levels
  )
import Competences.Document.Evidence
  ( Ability (..)
  , Observation (..)
  , SocialForm (..)
  , abilities
  , socialForms
  )
import Competences.Document.Order (formatOrderNumber)
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
  , makeSuggestions'
  , matchingInput
  , multiStageSelectorComponent
  , stage'
  )
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , isInitialUpdate
  , nextId
  )
import Data.Default (Default)
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Miso qualified as M

-- | The observation pipeline: 5 stages leading to an Observation
--
-- Takes a SyncDocumentRef to generate IDs for new observations
observationPipeline :: SyncDocumentRef -> Pipeline 'IsStage '[] CompetenceGrid Observation
observationPipeline r =
  stage' competenceGridP $ \cg ->
    stage' (competenceP cg) $ \c ->
      stage' (levelP c) $ \_ ->
        stage' socialFormP $ \_ ->
          stage' abilityP $ \_ ->
            done $ \(HCons a' (HCons sf' (HCons l' (HCons c' (HCons _cg HNil))))) -> do
              obsId <- nextId r
              pure $
                Observation
                  { id = obsId
                  , competenceLevelId = (c'.id, l')
                  , socialForm = sf'
                  , ability = a'
                  }

competenceGridP :: IncrementalParserSpec CompetenceGrid
competenceGridP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions i s =
      makeSuggestions'
        s
        reconstructInput
        (M.ms . (.title))
        (Ix.toAscList (Proxy @Order) i.competenceGrids)
    reconstructInput c = formatOrderNumber c.order

competenceP :: CompetenceGrid -> IncrementalParserSpec Competence
competenceP competenceGrid = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions i s =
      makeSuggestions'
        s
        reconstructInput
        (M.ms . (.description))
        (Ix.toAscList (Proxy @Order) (i.competences Ix.@= competenceGrid.id))
    reconstructInput c = formatOrderNumber c.order

levelP :: Competence -> IncrementalParserSpec Level
levelP competence = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions _ s =
      matchingInput s $
        mapMaybe
          ( \level -> do
              let label = reconstructInput level
              description <- competence.levelDescriptions Map.!? level
              pure (label, M.ms label <> ": " <> M.ms description, level)
          )
          levels
    reconstructInput BasicLevel = "1"
    reconstructInput IntermediateLevel = "2"
    reconstructInput AdvancedLevel = "3"

socialFormP :: IncrementalParserSpec SocialForm
socialFormP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions _ s =
      matchingInput s $
        map
          ( \socialForm ->
              let label = reconstructInput socialForm
                  description = C.translate' (C.LblSocialForm socialForm)
               in (label, M.ms label <> ": " <> description, socialForm)
          )
          socialForms
    reconstructInput Group = "1"
    reconstructInput Individual = "2"

abilityP :: IncrementalParserSpec Ability
abilityP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions _ s =
      matchingInput s $
        map
          ( \ability ->
              let label = reconstructInput ability
                  description = C.translate' (C.LblAbility ability)
               in (label, M.ms label <> ": " <> description, ability)
          )
          abilities
    reconstructInput SelfReliant = "1"
    reconstructInput SelfReliantWithSillyMistakes = "2"
    reconstructInput WithSupport = "3"
    reconstructInput NotYet = "4"

-- Configuration for observation selector
observationConfig :: SyncDocumentRef -> EvidenceId -> MultiStageSelectorStyle -> MultiStageSelectorConfig Observation
observationConfig r evidenceId style =
  MultiStageSelectorConfig
    { initialState = initialize (observationPipeline r)
    , errorMessage = C.translate' C.LblPleaseCompleteObservation
    , updateResults = \(DocumentChange doc info) currentResults ->
        if isInitialUpdate info
          then case Ix.getOne $ doc.evidences Ix.@= evidenceId of
            Just e -> Ix.toList e.observations
            Nothing -> []
          else currentResults
    , viewResult = viewObservationResult
    , style = style
    }

-- | Extract observation display data (pure function)
viewObservationResult :: Document -> Observation -> ResultView
viewObservationResult doc observation =
  case lookupObservationData doc observation of
    Nothing ->
      ResultView
        { badgeText = "???"
        , tooltipContent = Just "Die Beobachtung bezieht sich auf Daten, die nicht länger existieren."
        }
    Just (competence, competenceGrid) ->
      let competenceGridLabel = competenceGridP.reconstructInput competenceGrid
          competenceLabel = (competenceP competenceGrid).reconstructInput competence
          levelLabel = (levelP competence).reconstructInput (snd observation.competenceLevelId)
          socialFormLabel = socialFormP.reconstructInput observation.socialForm
          abilityLabel = abilityP.reconstructInput observation.ability
          label = M.ms $ intercalate "." [competenceGridLabel, competenceLabel, levelLabel, socialFormLabel, abilityLabel]
          tooltipText =
            M.ms competenceLabel
              <> ": "
              <> M.ms competence.description
              <> "\n"
              <> M.ms levelLabel
              <> ": "
              <> M.ms (fromMaybe "" (competence.levelDescriptions Map.!? snd observation.competenceLevelId))
              <> "\n"
              <> M.ms socialFormLabel
              <> ": "
              <> C.translate' (C.LblSocialForm observation.socialForm)
              <> "\n"
              <> M.ms abilityLabel
              <> ": "
              <> C.translate' (C.LblAbility observation.ability)
       in ResultView
            { badgeText = label
            , tooltipContent = Just tooltipText
            }

-- | Helper to lookup observation-related data
lookupObservationData :: Document -> Observation -> Maybe (Competence, CompetenceGrid)
lookupObservationData doc observation = do
  competence <- Ix.getOne $ doc.competences Ix.@= fst observation.competenceLevelId
  competenceGrid <- Ix.getOne $ doc.competenceGrids Ix.@= competence.competenceGridId
  pure (competence, competenceGrid)

observationSelectorComponent
  :: SyncDocumentRef
  -> EvidenceId
  -> MultiStageSelectorStyle
  -> SelectorTransformedLens p [] Observation f' a'
  -> MultiStageSelectorComponent p Observation
observationSelectorComponent r evidenceId style =
  multiStageSelectorComponent r (observationConfig r evidenceId style)

observationEditorField
  :: (Ord p, Default patch)
  => SyncDocumentRef
  -> M.MisoString
  -> (p -> EvidenceId)
  -> EntityPatchTransformedLens p patch [] Observation f t
  -> EditorField p patch f'
observationEditorField r key evidenceId eptl =
  selectorEditorField
    key
    eptl
    (observationSelectorComponent r . evidenceId)
    (MultiStageSelectorDisabled, MultiStageSelectorEnabled)
