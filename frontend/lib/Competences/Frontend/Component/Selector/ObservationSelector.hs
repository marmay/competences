module Competences.Frontend.Component.Selector.ObservationSelector
  ( observationSelectorComponent
  , observationEditorField
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceIxs
  , Document (..)
  , EvidenceId
  , Level (..)
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
import Competences.Document.Order (formatOrderNumber)
import Competences.Frontend.Common (onClick')
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorField)
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , isInitialUpdate
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.List (delete, intercalate)
import Data.List.Extra (isInfixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%~), (&), (.~), (?~))

data StageInfo a = StageInfo
  { currentInput :: String
  , currentSuggestions :: [(String, M.MisoString, a)]
  }
  deriving (Eq, Generic, Show)

data IncrementalParserSpec a = IncrementalParserSpec
  { makeSuggestions :: Ix.IxSet CompetenceIxs Competence -> String -> [(String, M.MisoString, a)]
  , reconstructInput :: a -> String
  }

data State
  = SelectingCompetence (StageInfo Competence)
  | SelectingCompetenceLevel Competence (StageInfo Level)
  | SelectingSocialForm Competence Level (StageInfo SocialForm)
  | SelectingAbility Competence Level SocialForm (StageInfo Ability)
  deriving (Eq, Show)

data Input
  = InputKey MappedKeyCode
  | InputRefresh

data MappedKeyCode
  = MEnter
  | MBackspace
  | MPeriod
  | MChar Char
  deriving (Eq, Show)

mapKeyCode :: M.KeyCode -> Maybe MappedKeyCode
mapKeyCode (M.KeyCode c)
  | c == 13 = Just MEnter
  | c == 8 = Just MBackspace
  | c == 190 = Just MPeriod
  | c == 48 = Just $ MChar '0'
  | c == 49 = Just $ MChar '1'
  | c == 50 = Just $ MChar '2'
  | c == 51 = Just $ MChar '3'
  | c == 52 = Just $ MChar '4'
  | c == 53 = Just $ MChar '5'
  | c == 54 = Just $ MChar '6'
  | c == 55 = Just $ MChar '7'
  | c == 56 = Just $ MChar '8'
  | c == 57 = Just $ MChar '9'
  | otherwise = Nothing

data HandleInputResult
  = HandleInputUpdate State
  | HandleInputFinished Competence Level SocialForm Ability
  | HandleInputError M.MisoString
  deriving (Eq, Show)

handleInput :: Ix.IxSet CompetenceIxs Competence -> Input -> State -> HandleInputResult
handleInput d input (SelectingCompetence i) =
  case handleInput' d input competenceP i of
    StateUpdatePop -> HandleInputUpdate $ SelectingCompetence $ initStageInfo competenceP d
    StateUpdatePush c -> HandleInputUpdate $ SelectingCompetenceLevel c $ initStageInfo (levelP c) d
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetence i'
    StateUpdateError e -> HandleInputError e
handleInput d input (SelectingCompetenceLevel c i) =
  case handleInput' d input (levelP c) i of
    StateUpdatePop -> HandleInputUpdate $ SelectingCompetence $ initStageInfo competenceP d
    StateUpdatePush l -> HandleInputUpdate $ SelectingSocialForm c l $ initStageInfo socialFormP d
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetenceLevel c i'
    StateUpdateError e -> HandleInputError e
handleInput d input (SelectingSocialForm c l i) =
  case handleInput' d input socialFormP i of
    StateUpdatePop -> HandleInputUpdate $ SelectingCompetenceLevel c $ initStageInfo (levelP c) d
    StateUpdatePush s -> HandleInputUpdate $ SelectingAbility c l s $ initStageInfo abilityP d
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingSocialForm c l i'
    StateUpdateError e -> HandleInputError e
handleInput d input (SelectingAbility c l s i) =
  case handleInput' d input abilityP i of
    StateUpdatePop -> HandleInputUpdate $ SelectingSocialForm c l $ initStageInfo socialFormP d
    StateUpdatePush a -> HandleInputFinished c l s a
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingAbility c l s i'
    StateUpdateError e -> HandleInputError e

data StateUpdateResult a
  = StateUpdatePop
  | StateUpdatePush a
  | StateUpdateUpdate (StageInfo a)
  | StateUpdateError M.MisoString
  deriving (Eq, Show)

matchingInput :: String -> [(String, M.MisoString, a)] -> [(String, M.MisoString, a)]
matchingInput s cs = filter (\(k, _, _) -> s `isInfixOf` k) cs

competenceP :: IncrementalParserSpec Competence
competenceP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions competences s =
      matchingInput
        s
        ( map
            ( \c ->
                let r = reconstructInput c
                 in (r, M.ms r <> ": " <> M.ms c.description, c)
            )
            (Ix.toList competences)
        )
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

initStageInfo :: IncrementalParserSpec a -> Ix.IxSet CompetenceIxs Competence -> StageInfo a
initStageInfo s competences = StageInfo "" (s.makeSuggestions competences "")

handleInput'
  :: Ix.IxSet CompetenceIxs Competence
  -> Input
  -> IncrementalParserSpec a
  -> StageInfo a
  -> StateUpdateResult a
handleInput' competences InputRefresh spec (StageInfo s _) =
  StateUpdateUpdate (StageInfo s (spec.makeSuggestions competences s))
handleInput' competences (InputKey k) spec (StageInfo s suggestions) =
  case k of
    MEnter -> StateUpdateError (C.translate' C.LblPleaseCompleteObservation)
    MBackspace ->
      if s == ""
        then StateUpdatePop
        else
          let s' = init s
           in StateUpdateUpdate (StageInfo s' (spec.makeSuggestions competences s'))
    MPeriod ->
      case suggestions of
        (_, _, r) : _ -> StateUpdatePush r
        _ -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
    MChar char ->
      let s' = s <> [char]
          suggestions' = spec.makeSuggestions competences s'
       in case suggestions' of
            [] -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
            _ -> StateUpdateUpdate (StageInfo s' suggestions')

data Model = Model
  { competences :: !(Ix.IxSet CompetenceIxs Competence)
  , observations :: ![Observation]
  , tooltipFor :: !(Maybe Observation)
  , state :: !State
  , error :: !(Maybe M.MisoString)
  }
  deriving (Eq, Generic, Show)

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
        { competences = Ix.empty
        , observations = []
        , tooltipFor = Nothing
        , state = SelectingCompetence $ initStageInfo competenceP Ix.empty
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
          & (#competences .~ d.competences)
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
      Model {competences, state} <- M.get
      case handleInput competences input state of
        HandleInputUpdate s -> M.modify $ \m -> m & (#state .~ s) & (#error .~ Nothing)
        HandleInputFinished competence level socialForm ability -> do
          M.withSink $ \sink -> do
            observationId <- nextId r
            sink $
              AddObservation $
                Observation {id = observationId, competenceLevelId = (competence.id, level), socialForm, ability}
          M.modify $ \m ->
            m & (#state .~ SelectingCompetence (initStageInfo competenceP competences)) & (#error .~ Nothing)
        HandleInputError e ->
          M.modify (#error ?~ e)

    view m =
      V.viewFlow
        (V.vFlow & #expandOrthogonal .~ V.Expand V.Center)
        ( [viewSelectedObservations m, viewCurrentInput m]
            <> [viewCurrentSuggestions m]
        )

    viewSelectedObservations m =
      M.div_ [] [V.viewFlow (V.hFlow & (#gap .~ V.SmallSpace)) (map (viewObservation m) m.observations)]

    viewObservation m observation =
      let (short, tooltip) =
            fromMaybe ("???", V.text_ "Die Beobachtung bezieht sich auf Daten, die nicht länger existieren.") $
              do
                competence <- Ix.getOne $ m.competences Ix.@= fst observation.competenceLevelId
                let competenceLabel = competenceP.reconstructInput competence
                let levelLabel = (levelP competence).reconstructInput (snd observation.competenceLevelId)
                let socialFormLabel = socialFormP.reconstructInput observation.socialForm
                let abilityLabel = abilityP.reconstructInput observation.ability
                let label = M.ms $ intercalate "." [competenceLabel, levelLabel, socialFormLabel, abilityLabel]
                let short' =
                      M.span_
                        [T.tailwind []]
                        [ V.viewFlow
                            (V.hFlow & (#gap .~ V.SmallSpace))
                            [ V.text_ label
                            , V.viewButton (V.deleteButton (DeleteObservation observation))
                            ]
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
                    (viewCompetence m.state <> viewLevel m.state <> viewSocialForm m.state <> viewCurrentInput' m.state)
                  <> "▁"
            )
        ]
    viewCompetence (SelectingCompetenceLevel c _) = [viewCompetence' c]
    viewCompetence (SelectingSocialForm c _ _) = [viewCompetence' c]
    viewCompetence (SelectingAbility c _ _ _) = [viewCompetence' c]
    viewCompetence _ = []
    viewCompetence' = competenceP.reconstructInput
    viewLevel (SelectingSocialForm c l _) = [viewLevel' c l]
    viewLevel (SelectingAbility c l _ _) = [viewLevel' c l]
    viewLevel _ = []
    viewLevel' c = (levelP c).reconstructInput
    viewSocialForm (SelectingAbility _ _ s _) = [viewSocialForm' s]
    viewSocialForm _ = []
    viewSocialForm' = socialFormP.reconstructInput
    viewCurrentInput' (SelectingCompetence i) = [i.currentInput]
    viewCurrentInput' (SelectingCompetenceLevel _ i) = [i.currentInput]
    viewCurrentInput' (SelectingSocialForm _ _ i) = [i.currentInput]
    viewCurrentInput' (SelectingAbility _ _ _ i) = [i.currentInput]

    viewCurrentSuggestions m =
      M.div_ [T.tailwind [T.TooltipBox, T.TextSm, T.H96, T.OverflowYScroll]] $ map (\v -> V.viewFlow (V.vFlow & (#gap .~ V.SmallSpace)) [M.text_ [M.ms v]]) (viewCurrentSuggestions' m.state)
    viewCurrentSuggestions' (SelectingCompetence i) = map (\(_, d, _) -> d) i.currentSuggestions
    viewCurrentSuggestions' (SelectingCompetenceLevel _ i) = map (\(_, d, _) -> d) i.currentSuggestions
    viewCurrentSuggestions' (SelectingSocialForm _ _ i) = map (\(_, d, _) -> d) i.currentSuggestions
    viewCurrentSuggestions' (SelectingAbility _ _ _ i) = map (\(_, d, _) -> d) i.currentSuggestions

observationEditorField
  :: (Ord p)
  => SyncDocumentRef
  -> M.MisoString
  -> (p -> EvidenceId)
  -> SelectorTransformedLens p [] Observation f t
  -> EditorField p f'
observationEditorField r key evidenceId lens =
  selectorEditorField
    key
    lens
    (observationSelectorComponent r . evidenceId)
    (ObservationSelectorStyleDisabled, ObservationSelectorStyleEnabled)
