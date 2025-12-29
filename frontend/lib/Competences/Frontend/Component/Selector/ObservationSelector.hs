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
  , StageInfo (..)
  , hListPop
  , hListPush
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
import Data.Kind (Type)
import Data.List (delete, intercalate)
import Data.List.Extra (isInfixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%~), (&), (.~), (?~))

data State
  = SelectingCompetenceGrid !(StageInfo '[] CompetenceGrid)
  | SelectingCompetence !(StageInfo '[CompetenceGrid] Competence)
  | SelectingCompetenceLevel !(StageInfo '[Competence, CompetenceGrid] Level)
  | SelectingSocialForm !(StageInfo '[Level, Competence, CompetenceGrid] SocialForm)
  | SelectingAbility !(StageInfo '[SocialForm, Level, Competence, CompetenceGrid] Ability)
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
  | HandleInputFinished (HList '[Ability, SocialForm, Level, Competence, CompetenceGrid])
  | HandleInputError M.MisoString

handleInput :: Document -> Input -> State -> HandleInputResult
handleInput doc input (SelectingCompetenceGrid i) =
  case handleInput' doc input competenceGridP i of
    StateUpdatePop -> HandleInputUpdate $ SelectingCompetenceGrid $ initStageInfo i.currentContext competenceGridP doc
    StateUpdatePush c ->
      HandleInputUpdate $
        SelectingCompetence $
          initStageInfo (hListPush c i.currentContext) (competenceP c) doc
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetenceGrid i'
    StateUpdateError e -> HandleInputError e
handleInput doc input (SelectingCompetence i@StageInfo {currentContext = competenceGrid `HCons` _}) =
  case handleInput' doc input (competenceP competenceGrid) i of
    StateUpdatePop ->
      HandleInputUpdate $
        SelectingCompetenceGrid $
          initStageInfo (hListPop i.currentContext) competenceGridP doc
    StateUpdatePush c ->
      HandleInputUpdate $
        SelectingCompetenceLevel $
          initStageInfo (hListPush c i.currentContext) (levelP c) doc
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetence i'
    StateUpdateError e -> HandleInputError e
handleInput doc input (SelectingCompetenceLevel i@StageInfo {currentContext = competence `HCons` competenceGrid `HCons` _}) =
  case handleInput' doc input (levelP competence) i of
    StateUpdatePop ->
      HandleInputUpdate $
        SelectingCompetence $
          initStageInfo (hListPop i.currentContext) (competenceP competenceGrid) doc
    StateUpdatePush l ->
      HandleInputUpdate $ SelectingSocialForm $ initStageInfo (hListPush l i.currentContext) socialFormP doc
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetenceLevel i'
    StateUpdateError e -> HandleInputError e
handleInput doc input (SelectingSocialForm i@StageInfo {currentContext = _ `HCons` competence `HCons` _}) =
  case handleInput' doc input socialFormP i of
    StateUpdatePop ->
      HandleInputUpdate $
        SelectingCompetenceLevel $
          initStageInfo (hListPop i.currentContext) (levelP competence) doc
    StateUpdatePush s -> HandleInputUpdate $ SelectingAbility $ initStageInfo (hListPush s i.currentContext) abilityP doc
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingSocialForm i'
    StateUpdateError e -> HandleInputError e
handleInput doc input (SelectingAbility i) =
  case handleInput' doc input abilityP i of
    StateUpdatePop ->
      HandleInputUpdate $ SelectingSocialForm $ initStageInfo (hListPop i.currentContext) socialFormP doc
    StateUpdatePush a -> HandleInputFinished $ hListPush a i.currentContext
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingAbility i'
    StateUpdateError e -> HandleInputError e

data StateUpdateResult (ctx :: [Type]) a
  = StateUpdatePop
  | StateUpdatePush a
  | StateUpdateUpdate (StageInfo ctx a)
  | StateUpdateError M.MisoString

deriving instance (Eq (HList ctx), Eq a) => Eq (StateUpdateResult ctx a)
deriving instance (Show (HList ctx), Show a) => Show (StateUpdateResult ctx a)

matchingInput :: String -> [(String, M.MisoString, a)] -> [(String, M.MisoString, a)]
matchingInput s cs = filter (\(k, _, _) -> s `isInfixOf` k) cs

makeSuggestions'
  :: String -> (a -> String) -> (a -> M.MisoString) -> [a] -> [(String, M.MisoString, a)]
makeSuggestions' s toKey toDescription as =
  matchingInput s (map (\a -> let key = toKey a in (key, M.ms key <> ": " <> toDescription a, a)) as)

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

initStageInfo :: HList ctx -> IncrementalParserSpec a -> Document -> StageInfo ctx a
initStageInfo ctx s doc = StageInfo ctx "" (s.makeSuggestions doc "")

handleInput'
  :: Document
  -> Input
  -> IncrementalParserSpec a
  -> StageInfo ctx a
  -> StateUpdateResult ctx a
handleInput' doc InputRefresh spec (StageInfo c s _) =
  StateUpdateUpdate (StageInfo c s (spec.makeSuggestions doc s))
handleInput' doc (InputKey k) spec (StageInfo c s suggestions) =
  case k of
    MEnter -> StateUpdateError (C.translate' C.LblPleaseCompleteObservation)
    MBackspace ->
      if s == ""
        then StateUpdatePop
        else
          let s' = init s
           in StateUpdateUpdate (StageInfo c s' (spec.makeSuggestions doc s'))
    MPeriod ->
      case suggestions of
        (_, _, r) : _ -> StateUpdatePush r
        _ -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
    MChar char ->
      let s' = s <> [char]
          suggestions' = spec.makeSuggestions doc s'
       in case suggestions' of
            [] -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
            _ -> StateUpdateUpdate (StageInfo c s' suggestions')

data Model = Model
  { document :: Document
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
        , state =
            SelectingCompetenceGrid $ initStageInfo HNil competenceGridP emptyDocument
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
      Model {document = doc, state} <- M.get
      case handleInput doc input state of
        HandleInputUpdate s -> M.modify $ \m -> m & (#state .~ s) & (#error .~ Nothing)
        HandleInputFinished
          (ability `HCons` socialForm `HCons` level `HCons` competence `HCons` _competenceGrid' `HCons` HNil) -> do
            M.withSink $ \sink -> do
              observationId <- nextId r
              sink $
                AddObservation $
                  Observation {id = observationId, competenceLevelId = (competence.id, level), socialForm, ability}
            M.modify $ \m ->
              m
                & (#state .~ SelectingCompetenceGrid (initStageInfo HNil competenceGridP doc))
                & (#error .~ Nothing)
        HandleInputError e ->
          M.modify (#error ?~ e)

    view m =
      V.viewFlow
        (V.vFlow & #expandOrthogonal .~ V.Expand V.Center)
        ( [viewSelectedObservations m]
            <> ( if style == ObservationSelectorStyleEnabled
                   then [viewCurrentInput m.state, viewCurrentSuggestions m.state]
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
                    (viewContext m <> viewCurrentInput' m)
                  <> "▁"
            )
        ]

    viewCurrentSuggestions state =
      M.div_ [T.tailwind [T.TooltipBox, T.TextSm, T.H96, T.W96, T.OverflowYScroll]] $
        map
          (\v -> V.viewFlow (V.vFlow & (#gap .~ V.SmallSpace)) [M.text_ [M.ms v]])
          (viewCurrentSuggestions' state)

    onStageInfo :: (forall p a. StageInfo p a -> b) -> State -> b
    onStageInfo f (SelectingCompetenceGrid s) = f s
    onStageInfo f (SelectingCompetence s) = f s
    onStageInfo f (SelectingCompetenceLevel s) = f s
    onStageInfo f (SelectingSocialForm s) = f s
    onStageInfo f (SelectingAbility s) = f s

    viewCurrentInput' = onStageInfo (\s -> [s.currentInput])
    viewCurrentSuggestions' = onStageInfo (\s -> map (\(_, d, _) -> d) s.currentSuggestions)

    viewContext (SelectingCompetenceGrid _) = []
    viewContext (SelectingCompetence StageInfo {currentContext = competenceGrid `HCons` HNil}) =
      [competenceGridP.reconstructInput competenceGrid]
    viewContext (SelectingCompetenceLevel StageInfo {currentContext = competence `HCons` competenceGrid `HCons` HNil}) =
      [ competenceGridP.reconstructInput competenceGrid
      , (competenceP competenceGrid).reconstructInput competence
      ]
    viewContext ( SelectingSocialForm
                    StageInfo {currentContext = level `HCons` competence `HCons` competenceGrid `HCons` HNil}
                  ) =
      [ competenceGridP.reconstructInput competenceGrid
      , (competenceP competenceGrid).reconstructInput competence
      , (levelP competence).reconstructInput level
      ]
    viewContext ( SelectingAbility
                    StageInfo
                      { currentContext = socialForm `HCons` level `HCons` competence `HCons` competenceGrid `HCons` HNil
                      }
                  ) =
      [ competenceGridP.reconstructInput competenceGrid
      , (competenceP competenceGrid).reconstructInput competence
      , (levelP competence).reconstructInput level
      , socialFormP.reconstructInput socialForm
      ]

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
