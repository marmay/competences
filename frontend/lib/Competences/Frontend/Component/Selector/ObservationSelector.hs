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
import Competences.Frontend.Component.Selector.MultiStageSelector
  ( HList (..)
  , IncrementalParserSpec (..)
  , Input (..)
  , Pipeline (..)
  , RuntimeState
  , getCurrentInput
  , getCurrentSuggestions
  , handleKeyboardInput
  , initialize
  , makeSuggestions'
  , mapKeyCode
  , matchingInput
  )
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , EvidenceId
  , Level (..)
  , Order
  , emptyDocument
  , levels
  )
import Competences.Document.Evidence
  ( Ability (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  , abilities
  , socialForms
  )
import Competences.Document.Evidence qualified as Evidence
import Competences.Document.Order (formatOrderNumber)
import Competences.Frontend.Common (onClick')
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorField)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens, SelectorTransformedLens, mkSelectorBinding)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , isInitialUpdate
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.Default (Default)
import Data.List (delete, intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%~), (&), (.~))

-- | The observation pipeline: 5 stages leading to an Observation
observationPipeline :: Pipeline '[] CompetenceGrid Observation
observationPipeline =
  Stage competenceGridP $ \HNil cg ->
    Stage (competenceP cg) $ \(HCons _cg' HNil) c ->
      Stage (levelP c) $ \(HCons _c' (HCons _cg'' HNil)) l ->
        Stage socialFormP $ \(HCons _l' (HCons _c'' (HCons _cg''' HNil))) sf ->
          Done abilityP $ \(HCons _sf' (HCons _l'' (HCons c''' (HCons _cg'''' HNil)))) a ->
            Observation
              { id = error "ID will be set after selection"
              , competenceLevelId = (c'''.id, l)
              , socialForm = sf
              , ability = a
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

data Model = Model
  { document :: Document
  , observations :: ![Observation]
  , tooltipFor :: !(Maybe Observation)
  , runtimeState :: !(RuntimeState Observation)
  , error :: !(Maybe M.MisoString)
  }
  deriving (Eq, Generic)

data Action
  = UpdateDocument !DocumentChange
  | HandleKeyPress !M.KeyInfo
  | AddObservation !Observation
  | DeleteObservation !Observation
  | TooltipFor !(Maybe Observation)
  deriving (Eq, Show)

data ObservationSelectorStyle
  = ObservationSelectorStyleEnabled
  | ObservationSelectorStyleDisabled
  deriving (Eq, Show)

observationSelectorComponent
  :: SyncDocumentRef
  -> EvidenceId
  -> ObservationSelectorStyle
  -> SelectorTransformedLens p [] Observation f' a'
  -> M.Component p Model Action
observationSelectorComponent r evidenceId style lens =
  (M.component model update view)
    { M.events = M.defaultEvents <> M.keyboardEvents <> M.mouseEvents
    , M.subs = [subscribeDocument r UpdateDocument]
    , M.bindings = [mkSelectorBinding lens #observations]
    }
  where
    model =
      Model
        { document = emptyDocument
        , observations = []
        , tooltipFor = Nothing
        , runtimeState = initialize observationPipeline emptyDocument
        , error = Nothing
        }

    update (UpdateDocument (DocumentChange d info)) = do
      let observationUpdater =
            if isInitialUpdate info
              then case Ix.getOne $ d.evidences Ix.@= evidenceId of
                Just e -> const $ Ix.toList e.observations
                Nothing -> const []
              else id
      M.modify $ \m ->
        m
          & (#document .~ d)
          & (#observations %~ observationUpdater)
      update' InputRefresh
    update (HandleKeyPress c) =
      case mapKeyCode c.keyCode of
        Just k -> update' (InputKey k)
        Nothing -> pure ()
    update (AddObservation o) = M.modify (#observations %~ (<> [o]))
    update (DeleteObservation o) = M.modify (#observations %~ delete o)
    update (TooltipFor o) = M.modify (#tooltipFor .~ o)

    update' :: Input -> M.Effect p Model Action
    update' input = do
      Model {document = doc, runtimeState} <- M.get
      case handleKeyboardInput (C.translate' C.LblPleaseCompleteObservation) doc input runtimeState of
        Left observation -> do
          -- Finished: observation selected
          M.withSink $ \sink -> do
            observationId <- nextId r
            sink $ AddObservation $ Evidence.Observation {Evidence.id = observationId, Evidence.competenceLevelId = observation.competenceLevelId, Evidence.socialForm = observation.socialForm, Evidence.ability = observation.ability}
          M.modify $ \m ->
            m
              & (#runtimeState .~ initialize observationPipeline doc)
              & (#error .~ Nothing)
        Right (newState, maybeError) ->
          M.modify $ \m -> m & (#runtimeState .~ newState) & (#error .~ maybeError)

    view m =
      V.viewFlow
        (V.vFlow & #expandOrthogonal .~ V.Expand V.Center)
        ( [viewSelectedObservations m]
            <> ( if style == ObservationSelectorStyleEnabled
                   then [viewCurrentInput m, viewCurrentSuggestions m.runtimeState]
                   else []
               )
        )

    viewSelectedObservations m =
      M.div_ [] [V.viewFlow (V.hFlow & (#gap .~ V.SmallSpace)) (map (viewObservation m) m.observations)]

    viewObservation m observation =
      let (short, tooltip) =
            fromMaybe ("???", V.text_ "Die Beobachtung bezieht sich auf Daten, die nicht länger existieren.") $
              do
                competence <- Ix.getOne $ m.document.competences Ix.@= fst observation.competenceLevelId
                competenceGrid <- Ix.getOne $ m.document.competenceGrids Ix.@= competence.competenceGridId
                let competenceGridLabel = competenceGridP.reconstructInput competenceGrid
                let competenceLabel = (competenceP competenceGrid).reconstructInput competence
                let levelLabel = (levelP competence).reconstructInput (snd observation.competenceLevelId)
                let socialFormLabel = socialFormP.reconstructInput observation.socialForm
                let abilityLabel = abilityP.reconstructInput observation.ability
                let label = M.ms $ intercalate "." [competenceGridLabel, competenceLabel, levelLabel, socialFormLabel, abilityLabel]
                let short' =
                      M.span_
                        [T.tailwind []]
                        [ V.viewFlow
                            (V.hFlow & (#gap .~ V.SmallSpace))
                            ( [V.text_ label]
                                <> [ V.viewButton (V.deleteButton (DeleteObservation observation))
                                   | style == ObservationSelectorStyleEnabled
                                   ]
                            )
                        ]
                let tooltip' =
                      V.viewFlow
                        (V.vFlow & (#expandOrthogonal .~ V.Expand V.Start))
                        [ V.text_ $ M.ms competenceLabel <> ": " <> M.ms competence.description
                        , V.text_ $
                            M.ms levelLabel
                              <> ": "
                              <> M.ms (fromMaybe "" (competence.levelDescriptions Map.!? snd observation.competenceLevelId))
                        , V.text_ $ M.ms socialFormLabel <> ": " <> C.translate' (C.LblSocialForm observation.socialForm)
                        , V.text_ $ M.ms abilityLabel <> ": " <> C.translate' (C.LblAbility observation.ability)
                        ]
                pure (short', tooltip')
       in if m.tooltipFor == Just observation
            then
              M.span_
                [onClick' (TooltipFor Nothing)]
                [V.withTooltip short (M.span_ [T.tailwind [T.P2]] [tooltip])]
            else M.span_ [onClick' (TooltipFor (Just observation))] [short]

    viewCurrentInput m =
      M.div_
        [ M.intProp "tabindex" 0
        , M.onKeyDownWithInfo HandleKeyPress
        , T.tailwind [T.WFull]
        ]
        [ V.text_
            ( M.toMisoString $
                ">"
                  <> intercalate
                    "."
                    (viewObservationContext m.runtimeState <> [getCurrentInput m.runtimeState])
                  <> "▁"
            )
        ]

    viewCurrentSuggestions state =
      M.div_ [T.tailwind [T.TooltipBox, T.TextSm, T.H96, T.W96, T.OverflowYScroll]] $
        map
          (\v -> V.viewFlow (V.vFlow & (#gap .~ V.SmallSpace)) [M.text_ [M.ms v]])
          (getCurrentSuggestions state)

    -- Reconstruct context breadcrumb for observation pipeline
    -- TODO: This currently returns empty list. We need to track breadcrumb separately
    -- or find a way to extract typed values from the existential context.
    viewObservationContext :: RuntimeState Observation -> [String]
    viewObservationContext _state = []

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
    (ObservationSelectorStyleDisabled, ObservationSelectorStyleEnabled)
