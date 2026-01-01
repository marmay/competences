{-# LANGUAGE UndecidableInstances #-}

-- | Generic multi-stage selector using GADTs and existential types
module Competences.Frontend.Component.Selector.MultiStageSelector
  ( -- * Type Alias
    MultiStageSelectorComponent

    -- * Pipeline Construction
  , stage
  , stage'
  , done

    -- * Pipeline Types (for type signatures)
  , Pipeline
  , StageKind (..)
  , HList (..)

    -- * Configuration
  , MultiStageSelectorConfig (..)
  , MultiStageSelectorStyle (..)
  , ResultView (..)
  , initialize

    -- * Component
  , multiStageSelectorComponent

    -- * Helper Functions
  , IncrementalParserSpec (..)
  , makeSuggestions'
  , matchingInput
  , getCurrentBreadcrumb
  )
where

import Competences.Document (Document (..), emptyDocument)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, isInitialUpdate, subscribeDocument)
import Data.Kind (Type)
import Data.List (delete, intercalate)
import Data.List.Extra (isInfixOf)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString, ms)
import Optics.Core ((%~), (&), (.~))
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding)

-- | Heterogeneous list for storing pipeline context
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

-- | Kind to distinguish Stage pipelines from Done pipelines at the type level
data StageKind = IsStage | IsDone

-- | Specification for incrementally parsing/selecting a value
data IncrementalParserSpec a = IncrementalParserSpec
  { makeSuggestions :: Document -> String -> [(String, M.MisoString, a)]
  , reconstructInput :: a -> String
  }

-- | State for a single stage of selection
data StageInfo (ctx :: [Type]) a = StageInfo
  { currentInput :: String
  , currentSuggestions :: [(String, M.MisoString, a)]
  }
  deriving (Generic)

deriving instance (Eq (HList ctx), Eq a) => Eq (StageInfo ctx a)

deriving instance (Show (HList ctx), Show a) => Show (StageInfo ctx a)

-- | A pipeline that produces 'result' given context 'ctx' (as type-level list)
--
-- The type parameter 'k' is the stage kind (IsStage or IsDone).
-- The type parameter 'a' is what we're selecting at the CURRENT stage.
-- This makes the existential explicit, which allows GHC to verify that
-- CurrentStage's StageInfo and Pipeline agree on the type being selected.
--
-- Type-level guarantee: RuntimeState can only contain 'IsStage pipelines.
--
-- Note: Use 'stage', 'stage'', or 'done' to construct pipelines instead of
-- using the constructors directly.
data Pipeline (k :: StageKind) (ctx :: [Type]) (a :: Type) result where
  -- Final stage: no selection, just build result from context using IO effects
  -- The 'a ~ ()' constraint indicates no value is selected at this stage
  Done
    :: (HList ctx -> IO result)
    -> Pipeline 'IsDone ctx () result
  -- Intermediate stage: select 'a', continue with extended context
  -- Note: The continuation returns a pipeline of unknown kind (Stage or Done)
  Stage
    :: IncrementalParserSpec a
    -> (HList ctx -> a -> Pipeline k (a ': ctx) b result)
    -> Pipeline 'IsStage ctx a result

-- ============================================================================
-- Pipeline Construction Functions
-- ============================================================================

-- | Construct a pipeline stage with explicit context parameter
--
-- The continuation receives both the HList context and the selected value.
-- This is useful when you need to inspect the context programmatically.
stage
  :: IncrementalParserSpec a
  -> (HList ctx -> a -> Pipeline k (a ': ctx) b result)
  -> Pipeline 'IsStage ctx a result
stage = Stage

-- | Construct a pipeline stage without explicit context parameter
--
-- The continuation only receives the selected value and can use closure
-- to access previous selections. This is more convenient and avoids boilerplate.
--
-- Example:
-- @
--   stage' competenceGridP $ \cg ->
--     stage' (competenceP cg) $ \c ->
--       stage' (levelP c) $ \l ->
--         done $ \(HCons l' (HCons c' (HCons cg' HNil))) ->
--           pure (cg', c', l')
-- @
stage'
  :: IncrementalParserSpec a
  -> (a -> Pipeline k (a ': ctx) b result)
  -> Pipeline 'IsStage ctx a result
stage' parser continue = Stage parser (\_ a -> continue a)

-- | Construct the final Done stage
--
-- The continuation receives the full HList context and performs IO effects
-- to produce the final result.
done
  :: (HList ctx -> IO result)
  -> Pipeline 'IsDone ctx () result
done = Done

-- | Runtime state while executing a pipeline
--
-- Both 'ctx' and 'a' are existential - hidden from the outside.
-- The CurrentStage bundles them together to ensure type safety.
--
-- Note: Contains a generation counter for Eq - since RuntimeState contains
-- functions (in Pipeline) and existential types, we can't compare structurally.
-- The generation counter increments on every update, so RuntimeStates are
-- never equal after modification.
--
-- The breadcrumb field stores string labels of selections made so far,
-- for display purposes (e.g., "1.2.3" showing the selection path).
data RuntimeState result where
  RuntimeState
    :: !Int -- Generation counter (for Eq)
    -> HList ctx -- Selections so far (existential ctx)
    -> CurrentStage ctx result -- Current stage (bundles StageInfo + Pipeline)
    -> [RuntimeState result] -- Previous states (for backspace)
    -> [String] -- Breadcrumb: labels of selections made so far
    -> RuntimeState result

-- Eq instance based on generation counter only
instance Eq (RuntimeState result) where
  RuntimeState gen1 _ _ _ _ == RuntimeState gen2 _ _ _ _ = gen1 == gen2

-- | Bundles StageInfo with Pipeline to tie their 'a' types together
--
-- This is the key to making existential types work: both StageInfo ctx a and
-- Pipeline ctx a result need to agree on what 'a' is at the current stage.
-- Now that both use type-level lists and expose 'a', GHC can verify they match!
--
-- Type-level guarantee: Only IsStage pipelines can be stored here.
data CurrentStage ctx result where
  CurrentStage
    :: StageInfo ctx a -- UI state for selecting 'a' (context is type-level list)
    -> Pipeline 'IsStage ctx a result -- Only Stage pipelines allowed!
    -> CurrentStage ctx result

-- | Initialize a pipeline with empty context
--
-- Type-level guarantee: Only accepts IsStage pipelines, so no error case needed!
initialize :: Pipeline 'IsStage '[] a result -> Document -> RuntimeState result
initialize pipeline@(Stage parser _) doc =
  RuntimeState
    0 -- Initial generation
    HNil
    (CurrentStage (initStageInfo HNil parser doc) pipeline)
    [] -- No history yet
    [] -- Empty breadcrumb at start

-- | Initialize a StageInfo from a parser
initStageInfo :: HList ctx -> IncrementalParserSpec a -> Document -> StageInfo ctx a
initStageInfo _ctx parser doc =
  StageInfo
    { currentInput = ""
    , currentSuggestions = parser.makeSuggestions doc ""
    }

-- | Go back to previous stage
backspace :: RuntimeState result -> RuntimeState result
backspace currentState@(RuntimeState gen _ctx _currentStage history _breadcrumb) =
  case history of
    [] ->
      -- At first stage, can't go back - but increment generation to trigger update
      incrementGeneration currentState
    (RuntimeState _oldGen ctx currentStage' hist breadcrumb : _rest) ->
      -- Restore previous state with a NEW generation number
      -- (breadcrumb is restored from history)
      RuntimeState (gen + 1) ctx currentStage' hist breadcrumb
  where
    incrementGeneration :: RuntimeState result -> RuntimeState result
    incrementGeneration (RuntimeState g c s h b) = RuntimeState (g + 1) c s h b

-- | Extract the current StageInfo from RuntimeState
--
-- This function uses existential types, so the result type is polymorphic.
-- It's mainly useful for display purposes.
getCurrentStageInfo :: RuntimeState result -> (forall ctx a. StageInfo ctx a -> r) -> r
getCurrentStageInfo (RuntimeState _gen _ctx (CurrentStage stageInfo _pipeline) _history _breadcrumb) f = f stageInfo

-- ============================================================================
-- Input Handling
-- ============================================================================

-- | Input events for the selector
data Input
  = InputKey MappedKeyCode
  | InputRefresh
  deriving (Eq, Show)

-- | Mapped key codes for selector navigation
data MappedKeyCode
  = MEnter
  | MBackspace
  | MPeriod
  | MChar Char
  deriving (Eq, Show)

-- | Map Miso key codes to our representation
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

-- | Result of updating a single stage
data StateUpdateResult (ctx :: [Type]) a
  = StateUpdatePop
  | StateUpdatePush a
  | StateUpdateUpdate (StageInfo ctx a)
  | StateUpdateError M.MisoString

deriving instance (Eq (HList ctx), Eq a) => Eq (StateUpdateResult ctx a)

deriving instance (Show (HList ctx), Show a) => Show (StateUpdateResult ctx a)

-- | Filter suggestions by substring match
matchingInput :: String -> [(String, M.MisoString, a)] -> [(String, M.MisoString, a)]
matchingInput s cs = filter (\(k, _, _) -> s `isInfixOf` k) cs

-- | Helper to make suggestions from a list of values
makeSuggestions'
  :: String -> (a -> String) -> (a -> M.MisoString) -> [a] -> [(String, M.MisoString, a)]
makeSuggestions' s toKey toDescription as =
  matchingInput s (map (\a -> let key = toKey a in (key, M.ms key <> ": " <> toDescription a, a)) as)

-- | Handle input for a single stage
handleInput'
  :: M.MisoString -- Error message for incomplete selection
  -> Document
  -> Input
  -> IncrementalParserSpec a
  -> StageInfo ctx a
  -> StateUpdateResult ctx a
handleInput' _errorMsg doc InputRefresh spec (StageInfo s _) =
  StateUpdateUpdate (StageInfo s (spec.makeSuggestions doc s))
handleInput' errorMsg doc (InputKey k) spec (StageInfo s suggestions) =
  case k of
    MEnter -> StateUpdateError errorMsg
    MBackspace ->
      if s == ""
        then StateUpdatePop
        else
          let s' = init s
           in StateUpdateUpdate (StageInfo s' (spec.makeSuggestions doc s'))
    MPeriod ->
      case suggestions of
        (_, _, r) : _ -> StateUpdatePush r
        _ -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
    MChar char ->
      let s' = s <> [char]
          suggestions' = spec.makeSuggestions doc s'
       in case suggestions' of
            [] -> StateUpdateError (C.translate' C.LblNoMatchingAlternatives)
            _ -> StateUpdateUpdate (StageInfo s' suggestions')

-- ============================================================================
-- Generic Model and Actions
-- ============================================================================

-- | Generic selector model
data Model result = Model
  { document :: Document
  , selectedResults :: [result]
  , tooltipFor :: Maybe result
  , runtimeState :: RuntimeState result
  , error :: Maybe M.MisoString
  }
  deriving (Generic)

deriving instance (Eq result) => Eq (Model result)

-- | Generic selector actions
data Action result
  = UpdateDocument !DocumentChange
  | HandleKeyPress !M.KeyInfo
  | AddResult !result
  | DeleteResult !result
  | TooltipFor !(Maybe result)
  | UpdateState !(RuntimeState result) !(Maybe M.MisoString) -- Internal: update runtime state

deriving instance (Eq result) => Eq (Action result)

-- Manual Show instance since RuntimeState can't derive Show (contains functions)
instance (Show result) => Show (Action result) where
  show (UpdateDocument dc) = "UpdateDocument " ++ show dc
  show (HandleKeyPress ki) = "HandleKeyPress " ++ show ki
  show (AddResult r) = "AddResult " ++ show r
  show (DeleteResult r) = "DeleteResult " ++ show r
  show (TooltipFor r) = "TooltipFor " ++ show r
  show (UpdateState _ err) = "UpdateState <RuntimeState> " ++ show err

-- | Handle keyboard input and advance/backtrack the state machine
handleKeyboardInput
  :: M.MisoString -- Error message for incomplete selection
  -> Document
  -> Input
  -> RuntimeState result
  -> IO (Either result (RuntimeState result, Maybe M.MisoString))
handleKeyboardInput errorMsg doc input state@(RuntimeState gen ctx (CurrentStage stageInfo pipeline) history breadcrumb) =
  case handleInput' errorMsg doc input (getParser pipeline) stageInfo of
    StateUpdatePop ->
      -- Go back to previous stage
      pure $ Right (backspace state, Nothing)
    StateUpdatePush selected ->
      -- Advance to next stage or finish
      -- Type-level guarantee: pipeline is always IsStage (Stage constructor)
      case pipeline of
        Stage parser continue ->
          let label = parser.reconstructInput selected -- Get string label!
              newBreadcrumb = breadcrumb ++ [label]    -- Append to breadcrumb
              newCtx = HCons selected ctx
              nextPipeline = continue ctx selected
           in case nextPipeline of
                Stage nextParser _nextContinue ->
                  pure $ Right
                    ( RuntimeState
                        (gen + 1) -- Increment generation
                        newCtx
                        (CurrentStage (initStageInfo newCtx nextParser doc) nextPipeline)
                        (state : history) -- Push current state (with old breadcrumb)
                        newBreadcrumb     -- New breadcrumb!
                    , Nothing
                    )
                Done buildResult ->
                  -- Auto-execute the Done stage immediately!
                  Left <$> buildResult newCtx
    StateUpdateUpdate stageInfo' ->
      pure $ Right (RuntimeState (gen + 1) ctx (CurrentStage stageInfo' pipeline) history breadcrumb, Nothing)
    StateUpdateError err ->
      pure $ Right (state, Just err)
  where
    -- Type-level guarantee: pipeline is always IsStage, so no Done case needed!
    getParser :: Pipeline 'IsStage ctx a result -> IncrementalParserSpec a
    getParser (Stage parser _) = parser

-- ============================================================================
-- Generic View Helpers
-- ============================================================================

-- | Get the current input string
getCurrentInput :: RuntimeState result -> String
getCurrentInput state = getCurrentStageInfo state (\s -> s.currentInput)

-- | Get the current suggestions
getCurrentSuggestions :: RuntimeState result -> [M.MisoString]
getCurrentSuggestions state = getCurrentStageInfo state (\s -> map (\(_, d, _) -> d) s.currentSuggestions)

-- | Get the current breadcrumb (selection path as list of strings)
getCurrentBreadcrumb :: RuntimeState result -> [String]
getCurrentBreadcrumb (RuntimeState _ _ _ _ breadcrumb) = breadcrumb

-- ============================================================================
-- Generic Selector Component
-- ============================================================================

-- | Pure data representation of a result for display
--
-- The viewResult function in SelectorConfig produces this pure data,
-- and the generic component handles rendering it with interactive elements.
data ResultView = ResultView
  { badgeText :: M.MisoString
  , tooltipContent :: Maybe M.MisoString
  }
  deriving (Eq, Show)

-- | Style configuration for selector
data MultiStageSelectorStyle
  = MultiStageSelectorEnabled
  | MultiStageSelectorDisabled
  deriving (Eq, Show)

-- | Configuration for a generic selector component
--
-- The selector is self-contained and uses Action internally.
-- Parent components interact via:
--   - Document input (subscription/binding)
--   - Results output (binding to selectedResults)
data MultiStageSelectorConfig result = MultiStageSelectorConfig
  { initialState :: Document -> RuntimeState result -- Initialize the state machine
  , errorMessage :: MisoString
  , initResults :: Document -> [result] -- Load initial results from document
  , validateResults :: Document -> [result] -> [result] -- Validate/filter results when document changes
  , viewResult :: Document -> result -> ResultView -- Pure data extraction
  , style :: MultiStageSelectorStyle
  }

-- Generic update function
genericUpdate
  :: (Eq result)
  => MultiStageSelectorConfig result
  -> Action result
  -> M.Effect p (Model result) (Action result)
genericUpdate config action =
  case action of
    UpdateDocument (DocumentChange doc info) -> do
      M.modify $ \m ->
        m
          & (#document .~ doc)
          & (#selectedResults %~ \currentResults ->
              if isInitialUpdate info
                then config.initResults doc
                else config.validateResults doc currentResults
            )
      -- Refresh suggestions with the new document data
      genericUpdate' config InputRefresh
    HandleKeyPress keyInfo ->
      case mapKeyCode keyInfo.keyCode of
        Just k -> genericUpdate' config (InputKey k)
        Nothing -> pure ()
    AddResult r ->
      M.modify (#selectedResults %~ (<> [r]))
    DeleteResult r ->
      M.modify (#selectedResults %~ delete r)
    TooltipFor r ->
      M.modify (#tooltipFor .~ r)
    UpdateState newState maybeError ->
      M.modify $ \m -> m & (#runtimeState .~ newState) & (#error .~ maybeError)

genericUpdate'
  :: (Eq result)
  => MultiStageSelectorConfig result
  -> Input
  -> M.Effect p (Model result) (Action result)
genericUpdate' config input = do
  Model {document = doc, runtimeState} <- M.get
  M.withSink $ \sink -> do
    resultOrState <- handleKeyboardInput config.errorMessage doc input runtimeState
    case resultOrState of
      Left result -> do
        -- Finished: result selected - add it and reset state
        sink $ AddResult result
        sink $ UpdateState (config.initialState doc) Nothing
      Right (newState, maybeError) ->
        -- Update the runtime state
        sink $ UpdateState newState maybeError

-- Generic view function
genericView
  :: (Eq result)
  => MultiStageSelectorConfig result
  -> Model result
  -> M.View (Model result) (Action result)
genericView config model =
  M.div_ [] $
    [ viewSelectedResults config model
    ]
      <> ( if config.style == MultiStageSelectorEnabled
             then [viewCurrentInput model, viewCurrentSuggestions model, viewError model]
             else []
         )

viewSelectedResults
  :: (Eq result)
  => MultiStageSelectorConfig result
  -> Model result
  -> M.View (Model result) (Action result)
viewSelectedResults config model =
  M.div_ [] $
    map (viewResultBadge config model) model.selectedResults

-- | Render a single result badge with delete button and tooltip
viewResultBadge
  :: (Eq result)
  => MultiStageSelectorConfig result
  -> Model result
  -> result
  -> M.View (Model result) (Action result)
viewResultBadge config model result =
  let ResultView {badgeText, tooltipContent} = config.viewResult model.document result
      badgeContent =
        M.span_
          []
          [ M.text_ [badgeText]
          , if config.style == MultiStageSelectorEnabled
              then M.button_ [M.onClick (DeleteResult result)] [M.text_ ["×"]]
              else M.text_ []
          ]
      withTooltip content =
        case tooltipContent of
          Nothing -> content
          Just tip ->
            if model.tooltipFor == Just result
              then
                M.span_
                  [M.onClick (TooltipFor Nothing)]
                  [ content
                  , M.span_ [] [M.text_ [tip]]
                  ]
              else M.span_ [M.onClick (TooltipFor (Just result))] [content]
   in withTooltip badgeContent

viewCurrentInput
  :: Model result
  -> M.View (Model result) (Action result)
viewCurrentInput model =
  M.div_
    [ M.intProp "tabindex" 0
    , M.onKeyDownWithInfo HandleKeyPress
    ]
    [ M.text_
        [ ms $
            ">"
              <> intercalate
                "."
                (getCurrentBreadcrumb model.runtimeState <> [getCurrentInput model.runtimeState])
              <> "▁"
        ]
    ]

viewCurrentSuggestions
  :: Model result
  -> M.View (Model result) (Action result)
viewCurrentSuggestions model =
  M.div_ [] $
    map
      (\v -> M.div_ [] [M.text_ [v]])
      (getCurrentSuggestions model.runtimeState)

viewError
  :: Model result
  -> M.View (Model result) (Action result)
viewError model =
  case model.error of
    Nothing -> M.div_ [] []
    Just err -> M.div_ [] [M.text_ ["Error: " <> err]]

-- ============================================================================
-- Generic Multi-Stage Selector Component
-- ============================================================================

-- | Type alias for a multi-stage selector component
--
-- This provides a cleaner interface by hiding the internal Model and Action types.
type MultiStageSelectorComponent p result = M.Component p (Model result) (Action result)

-- | Create a multi-stage selector component
--
-- This function creates a self-contained Miso component for a multi-stage selector.
-- The selector manages its own state and actions internally.
--
-- Parent components interact via:
--   - Document input: binding/subscription to update document
--   - Results output: binding to selectedResults field
--
-- Example usage:
--   multiStageSelectorComponent config updateResults subs bindings
--     where
--       updateResults = ... -- how to update results when document changes
--       subs = [documentSubscription]
--       bindings = [resultBinding]
multiStageSelectorComponent
  :: (Eq result, Show result)
  => SyncDocumentRef
  -> MultiStageSelectorConfig result
  -> SelectorTransformedLens p [] result f' a'
  -> MultiStageSelectorComponent p result
multiStageSelectorComponent r config lens =
  (M.component model update view)
    { M.events = M.defaultEvents <> M.keyboardEvents <> M.mouseEvents
    , M.subs = [subscribeDocument r UpdateDocument]
    , M.bindings = [mkSelectorBinding lens #selectedResults]
    }
  where
    model =
      Model
        { document = emptyDocument
        , selectedResults = []
        , tooltipFor = Nothing
        , runtimeState = config.initialState emptyDocument
        , error = Nothing
        }

    update = genericUpdate config

    view = genericView config
