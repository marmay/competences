{-# LANGUAGE UndecidableInstances #-}

module Competences.Frontend.Component.Selector.ObservationSelector
  ( observationSelectorComponent
  , observationEditorField
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , CompetenceGridIxs
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
import Data.Kind (Type)
import Data.List (delete, intercalate)
import Data.List.Extra (isInfixOf)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%~), (&), (.~), (?~))

data StageInfo p a = StageInfo
  { currentContext :: p
  , currentInput :: String
  , currentSuggestions :: [(String, M.MisoString, a)]
  }
  deriving (Eq, Generic, Show)

data MakeSuggestionsInput = MakeSuggestionsInput
  { competenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , competences :: !(Ix.IxSet CompetenceIxs Competence)
  }
  deriving (Eq, Show)

mkMakeSuggestionsInput :: Document -> MakeSuggestionsInput
mkMakeSuggestionsInput d =
  MakeSuggestionsInput
    { competenceGrids = d.competenceGrids
    , competences = d.competences
    }

data IncrementalParserSpec a = IncrementalParserSpec
  { makeSuggestions :: MakeSuggestionsInput -> String -> [(String, M.MisoString, a)]
  , reconstructInput :: a -> String
  }

data State
  = SelectingCompetenceGrid !(StageInfo (HList '[]) CompetenceGrid)
  | SelectingCompetence !(StageInfo (HList '[CompetenceGrid]) Competence)
  | SelectingCompetenceLevel !(StageInfo (HList '[Competence, CompetenceGrid]) Level)
  | SelectingSocialForm !(StageInfo (HList '[Level, Competence, CompetenceGrid]) SocialForm)
  | SelectingAbility !(StageInfo (HList '[SocialForm, Level, Competence, CompetenceGrid]) Ability)
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

type HList :: [Type] -> Type
data HList xs where
  HCons :: x -> HList xs -> HList (x ': xs)
  HNil :: HList '[]

infixr 6 `HCons`

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs)) where
  x `HCons` xs == x' `HCons` xs' = x == x' && xs == xs'

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
  show (x `HCons` xs) = show x <> " `HCons` " <> show xs

hListPop :: HList (x ': xs) -> HList xs
hListPop (HCons _ xs) = xs

hListPush :: x -> HList xs -> HList (x ': xs)
hListPush = HCons

class HListOn f a xs where
  hListOn' :: a -> f -> HList xs -> a
  hListOn' def _ _ = def

instance HListOn (x -> a) a (x ': xs) where
  hListOn' _ f (x `HCons` _) = f x

instance (HListOn (y -> a) a xs) => HListOn (x -> y -> a) a (x ': xs) where
  hListOn' def f (x `HCons` xs) = hListOn' def (f x) xs

instance HListOn f a '[] where
  hListOn' def _ HNil = def

instance (HListOn f a xs) => HListOn f a (x ': xs) where
  hListOn' def f (_ `HCons` xs) = hListOn' def f xs

hListOn :: (HListOn f a xs) => a -> f -> HList xs -> a
hListOn = hListOn'

data HandleInputResult
  = HandleInputUpdate State
  | HandleInputFinished (HList '[Ability, SocialForm, Level, Competence, CompetenceGrid])
  | HandleInputError M.MisoString

handleInput :: MakeSuggestionsInput -> Input -> State -> HandleInputResult
handleInput d input (SelectingCompetenceGrid i) =
  case handleInput' d input competenceGridP i of
    StateUpdatePop -> HandleInputUpdate $ SelectingCompetenceGrid $ initStageInfo i.currentContext competenceGridP d
    StateUpdatePush c ->
      HandleInputUpdate $ SelectingCompetence $ initStageInfo (hListPush c i.currentContext) competenceP d
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetenceGrid i'
    StateUpdateError e -> HandleInputError e
handleInput d input (SelectingCompetence i) =
  case handleInput' d input competenceP i of
    StateUpdatePop -> HandleInputUpdate $ SelectingCompetence $ initStageInfo i.currentContext competenceP d
    StateUpdatePush c ->
      HandleInputUpdate $
        SelectingCompetenceLevel $
          initStageInfo (hListPush c i.currentContext) (levelP c) d
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetence i'
    StateUpdateError e -> HandleInputError e
handleInput d input (SelectingCompetenceLevel i@StageInfo {currentContext = competence `HCons` _}) =
  case handleInput' d input (levelP competence) i of
    StateUpdatePop ->
      HandleInputUpdate $ SelectingCompetence $ initStageInfo (hListPop i.currentContext) competenceP d
    StateUpdatePush l ->
      HandleInputUpdate $ SelectingSocialForm $ initStageInfo (hListPush l i.currentContext) socialFormP d
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingCompetenceLevel i'
    StateUpdateError e -> HandleInputError e
handleInput d input (SelectingSocialForm i@StageInfo {currentContext = _ `HCons` competence `HCons` _}) =
  case handleInput' d input socialFormP i of
    StateUpdatePop ->
      HandleInputUpdate $
        SelectingCompetenceLevel $
          initStageInfo (hListPop i.currentContext) (levelP competence) d
    StateUpdatePush s -> HandleInputUpdate $ SelectingAbility $ initStageInfo (hListPush s i.currentContext) abilityP d
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingSocialForm i'
    StateUpdateError e -> HandleInputError e
handleInput d input (SelectingAbility i) =
  case handleInput' d input abilityP i of
    StateUpdatePop ->
      HandleInputUpdate $ SelectingSocialForm $ initStageInfo (hListPop i.currentContext) socialFormP d
    StateUpdatePush a -> HandleInputFinished $ hListPush a i.currentContext
    StateUpdateUpdate i' -> HandleInputUpdate $ SelectingAbility i'
    StateUpdateError e -> HandleInputError e

data StateUpdateResult p a
  = StateUpdatePop
  | StateUpdatePush a
  | StateUpdateUpdate (StageInfo p a)
  | StateUpdateError M.MisoString
  deriving (Eq, Show)

matchingInput :: String -> [(String, M.MisoString, a)] -> [(String, M.MisoString, a)]
matchingInput s cs = filter (\(k, _, _) -> s `isInfixOf` k) cs

makeSuggestions'
  :: String -> (a -> String) -> (a -> M.MisoString) -> [a] -> [(String, M.MisoString, a)]
makeSuggestions' s toKey toDescription as =
  matchingInput s (map (\a -> let key = toKey a in (key, M.ms key <> toDescription a, a)) as)
  
competenceGridP :: IncrementalParserSpec CompetenceGrid
competenceGridP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions i s =
      makeSuggestions'
        s
        reconstructInput
        (M.ms . (.title))
        (Ix.toList i.competenceGrids)
    reconstructInput c = formatOrderNumber c.order

competenceP :: IncrementalParserSpec Competence
competenceP = IncrementalParserSpec {makeSuggestions, reconstructInput}
  where
    makeSuggestions i s =
      makeSuggestions'
        s
        reconstructInput
        (M.ms . (.description))
        (Ix.toList i.competences)
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

initStageInfo :: p -> IncrementalParserSpec a -> MakeSuggestionsInput -> StageInfo p a
initStageInfo p s i = StageInfo p "" (s.makeSuggestions i "")

handleInput'
  :: MakeSuggestionsInput
  -> Input
  -> IncrementalParserSpec a
  -> StageInfo p a
  -> StateUpdateResult p a
handleInput' makeSuggestionsInput InputRefresh spec (StageInfo c s _) =
  StateUpdateUpdate (StageInfo c s (spec.makeSuggestions makeSuggestionsInput s))
handleInput' makeSuggestionsInput (InputKey k) spec (StageInfo c s suggestions) =
  case k of
    MEnter -> StateUpdateError (C.translate' C.LblPleaseCompleteObservation)
    MBackspace ->
      if s == ""
        then StateUpdatePop
        else
          let s' = init s
           in StateUpdateUpdate (StageInfo c s' (spec.makeSuggestions makeSuggestionsInput s'))
    MPeriod ->
      case suggestions of
        (_, _, r) : _ -> StateUpdatePush r
        _ -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
    MChar char ->
      let s' = s <> [char]
          suggestions' = spec.makeSuggestions makeSuggestionsInput s'
       in case suggestions' of
            [] -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
            _ -> StateUpdateUpdate (StageInfo c s' suggestions')

data Model = Model
  { suggestionsInput :: MakeSuggestionsInput
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
        { suggestionsInput = mkMakeSuggestionsInput emptyDocument
        , observations = []
        , tooltipFor = Nothing
        , state =
            SelectingCompetenceGrid $ initStageInfo HNil competenceGridP (mkMakeSuggestionsInput emptyDocument)
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
          & (#suggestionsInput .~ mkMakeSuggestionsInput d)
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
      Model {suggestionsInput, state} <- M.get
      case handleInput suggestionsInput input state of
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
                & (#state .~ SelectingCompetenceGrid (initStageInfo HNil competenceGridP suggestionsInput))
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
                competence <- Ix.getOne $ m.suggestionsInput.competences Ix.@= fst observation.competenceLevelId
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
                    (viewCompetence m <> viewLevel m <> viewSocialForm m <> viewCurrentInput' m)
                  <> "▁"
            )
        ]

    viewCurrentSuggestions state =
      M.div_ [T.tailwind [T.TooltipBox, T.TextSm, T.H96, T.W96, T.OverflowYScroll]] $
        map
          (\v -> V.viewFlow (V.vFlow & (#gap .~ V.SmallSpace)) [M.text_ [M.ms v]])
          (viewCurrentSuggestions' state)

    onContext :: a -> (HList xs -> a) -> State -> a
    onContext def f (SelectingCompetenceGrid s) = hListOn def f s.currentContext
    onContext def f (SelectingCompetence s) = hListOn def f s.currentContext
    onContext def f (SelectingCompetenceLevel s) = hListOn def f s.currentContext
    onContext def f (SelectingSocialForm s) = hListOn def f s.currentContext
    onContext def f (SelectingAbility s) = hListOn def f s.currentContext

    onStageInfo :: (forall p a. StageInfo p a -> b) -> State -> b
    onStageInfo f (SelectingCompetenceGrid s) = f s
    onStageInfo f (SelectingCompetence s) = f s
    onStageInfo f (SelectingCompetenceLevel s) = f s
    onStageInfo f (SelectingSocialForm s) = f s
    onStageInfo f (SelectingAbility s) = f s

    viewCurrentInput' = onStageInfo (\s -> [s.currentInput])
    viewCurrentSuggestions' = onStageInfo (\s -> map (\(_, d, _) -> d) s.currentSuggestions)

    viewCompetence = onContext [] viewCompetence'
      where
        viewCompetence' :: HList '[Competence] -> [String]
        viewCompetence' (c `HCons` HNil) = [competenceP.reconstructInput c]
    viewLevel = onContext [] viewLevel'
      where
        viewLevel' :: HList '[Level, Competence] -> [String]
        viewLevel' (l `HCons` (c `HCons` HNil)) = [(levelP c).reconstructInput l]
    viewSocialForm = onContext [] viewSocialForm'
      where
        viewSocialForm' :: HList '[SocialForm] -> [String]
        viewSocialForm' (s `HCons` HNil) = [socialFormP.reconstructInput s]

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
