{-# LANGUAGE UndecidableInstances #-}

-- | Generic multi-stage selector using GADTs and existential types
module Competences.Frontend.Component.Selector.MultiStageSelector
  ( -- * Pipeline Definition
    Pipeline (..)
  , RuntimeState (..)
  , CurrentStage (..)

    -- * HList - Heterogeneous lists for context
  , HList (..)
  , hListPush
  , hListPop

    -- * Operations
  , initialize
  , advanceIfReady
  , backspace
  , getSelection
  , initStageInfo
  , updateInput
  , clearInput
  , getCurrentStageInfo

    -- * Input Handling
  , Input (..)
  , MappedKeyCode (..)
  , StateUpdateResult (..)
  , mapKeyCode
  , handleInput'
  , matchingInput
  , makeSuggestions'

    -- * Generic Model and Actions
  , SelectorModel (..)
  , SelectorAction (..)
  , initialModel
  , handleKeyboardInput

    -- * Generic View Helpers
  , getCurrentInput
  , getCurrentSuggestions
  , reconstructContext

    -- * Re-exports
  , IncrementalParserSpec (..)
  , StageInfo (..)
  )
where

import Data.Kind (Type)
import Data.List.Extra (isInfixOf)

import Competences.Document (Document (..))
import GHC.Generics (Generic)
import Miso qualified as M

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

-- | Push a value onto the front of an HList
hListPush :: x -> HList xs -> HList (x ': xs)
hListPush = HCons

-- | Pop a value from the front of an HList
hListPop :: HList (x ': xs) -> HList xs
hListPop (HCons _ xs) = xs

-- | Specification for incrementally parsing/selecting a value
data IncrementalParserSpec a = IncrementalParserSpec
  { makeSuggestions :: Document -> String -> [(String, M.MisoString, a)]
  , reconstructInput :: a -> String
  }

-- | State for a single stage of selection
data StageInfo (ctx :: [Type]) a = StageInfo
  { currentContext :: HList ctx
  , currentInput :: String
  , currentSuggestions :: [(String, M.MisoString, a)]
  }
  deriving (Generic)

deriving instance (Eq (HList ctx), Eq a) => Eq (StageInfo ctx a)
deriving instance (Show (HList ctx), Show a) => Show (StageInfo ctx a)

-- | A pipeline that produces 'result' given context 'ctx' (as type-level list)
--
-- The type parameter 'a' is what we're selecting at the CURRENT stage.
-- This makes the existential explicit, which allows GHC to verify that
-- CurrentStage's StageInfo and Pipeline agree on the type being selected.
data Pipeline (ctx :: [Type]) (a :: Type) result where
  -- Final stage: select value 'a', produce result
  Done
    :: IncrementalParserSpec a
    -> (HList ctx -> a -> result)
    -> Pipeline ctx a result
  -- Intermediate stage: select 'a', continue with extended context
  -- Note: The next pipeline selects type 'b', which may be different from 'a'
  Stage
    :: IncrementalParserSpec a
    -> (HList ctx -> a -> Pipeline (a ': ctx) b result)
    -> Pipeline ctx a result

-- | Runtime state while executing a pipeline
--
-- Both 'ctx' and 'a' are existential - hidden from the outside.
-- The CurrentStage bundles them together to ensure type safety.
--
-- Note: Contains a generation counter for Eq - since RuntimeState contains
-- functions (in Pipeline) and existential types, we can't compare structurally.
-- The generation counter increments on every update, so RuntimeStates are
-- never equal after modification.
data RuntimeState result where
  RuntimeState
    :: !Int -- Generation counter (for Eq)
    -> HList ctx -- Selections so far (existential ctx)
    -> CurrentStage ctx result -- Current stage (bundles StageInfo + Pipeline)
    -> [RuntimeState result] -- Previous states (for backspace)
    -> RuntimeState result

-- Eq instance based on generation counter only
instance Eq (RuntimeState result) where
  RuntimeState gen1 _ _ _ == RuntimeState gen2 _ _ _ = gen1 == gen2

-- | Bundles StageInfo with Pipeline to tie their 'a' types together
--
-- This is the key to making existential types work: both StageInfo ctx a and
-- Pipeline ctx a result need to agree on what 'a' is at the current stage.
-- Now that both use type-level lists and expose 'a', GHC can verify they match!
data CurrentStage ctx result where
  CurrentStage
    :: StageInfo ctx a -- UI state for selecting 'a' (context is type-level list)
    -> Pipeline ctx a result -- Pipeline (also selecting 'a' at current stage)
    -> CurrentStage ctx result

-- | Initialize a pipeline with empty context
initialize :: Pipeline '[] a result -> Document -> RuntimeState result
initialize pipeline doc =
  RuntimeState
    0 -- Initial generation
    HNil
    (CurrentStage (initStageInfo HNil (getParser pipeline) doc) pipeline)
    []
  where
    getParser :: Pipeline ctx a result -> IncrementalParserSpec a
    getParser (Stage parser _) = parser
    getParser (Done parser _) = parser

-- | Initialize a StageInfo from a parser
initStageInfo :: HList ctx -> IncrementalParserSpec a -> Document -> StageInfo ctx a
initStageInfo ctx parser doc =
  StageInfo
    { currentContext = ctx
    , currentInput = ""
    , currentSuggestions = parser.makeSuggestions doc ""
    }

-- | Extract the selected value from StageInfo if one has been chosen
--
-- For now, we return the first suggestion if any exist (simplified version).
-- In practice, this would check if the user has confirmed a selection.
getSelection :: StageInfo ctx a -> Maybe a
getSelection stageInfo =
  case stageInfo.currentSuggestions of
    (_, _, value) : _ | stageInfo.currentInput /= "" -> Just value
    _ -> Nothing

-- | Advance the pipeline if a selection is ready
--
-- This is the key function that ties existential types together:
-- 1. Pattern match on RuntimeState to bring existential 'a' into scope
-- 2. Extract selection from StageInfo (which produces a value of type 'a')
-- 3. Use that value with Pipeline (which expects a value of type 'a')
-- 4. All in the same scope, so types are guaranteed to match
advanceIfReady
  :: RuntimeState result
  -> Document
  -> Either result (RuntimeState result)
advanceIfReady state@(RuntimeState gen ctx (CurrentStage stageInfo pipeline) history) doc =
  case getSelection stageInfo of
    Nothing -> Right state -- Not ready to advance
    Just selected ->
      -- Here 'selected :: a' where StageInfo has type (StageInfo ctx a)
      -- and Pipeline has type (Pipeline ctx a result)
      -- Both agree on 'a' and 'ctx' thanks to CurrentStage bundling!
      case pipeline of
        Stage _parser continue ->
          let newCtx = HCons selected ctx
              -- continue takes the OLD context (ctx) and selected value,
              -- and returns a pipeline with NEW context (a : ctx)
              nextPipeline = continue ctx selected
           in case nextPipeline of
                Stage nextParser _nextContinue ->
                  Right $
                    RuntimeState
                      (gen + 1) -- Increment generation
                      newCtx
                      (CurrentStage (initStageInfo newCtx nextParser doc) nextPipeline)
                      (state : history) -- Push current state
                Done finalParser _buildResult ->
                  -- Could continue to final stage, or finish here
                  Right $
                    RuntimeState
                      (gen + 1) -- Increment generation
                      newCtx
                      (CurrentStage (initStageInfo newCtx finalParser doc) nextPipeline)
                      (state : history)
        Done _parser buildResult ->
          Left (buildResult ctx selected) -- Done!

-- | Go back to previous stage
backspace :: RuntimeState result -> RuntimeState result
backspace currentState@(RuntimeState gen _ctx _currentStage history) =
  case history of
    [] ->
      -- At first stage, can't go back - but increment generation to trigger update
      incrementGeneration currentState
    (RuntimeState _oldGen ctx stage hist : _rest) ->
      -- Restore previous state with a NEW generation number
      RuntimeState (gen + 1) ctx stage hist
  where
    incrementGeneration :: RuntimeState result -> RuntimeState result
    incrementGeneration (RuntimeState g c s h) = RuntimeState (g + 1) c s h

-- | Update the input string in the current stage
updateInput :: String -> RuntimeState result -> Document -> RuntimeState result
updateInput newInput (RuntimeState gen ctx (CurrentStage stageInfo pipeline) history) doc =
  RuntimeState (gen + 1) ctx (CurrentStage (updateStageInput newInput parser stageInfo doc) pipeline) history
  where
    parser = case pipeline of
      Stage p _ -> p
      Done p _ -> p

    updateStageInput input p (StageInfo c _ _) d =
      StageInfo c input (p.makeSuggestions d input)

-- | Clear the input in the current stage
clearInput :: RuntimeState result -> Document -> RuntimeState result
clearInput = updateInput ""

-- | Extract the current StageInfo from RuntimeState
--
-- This function uses existential types, so the result type is polymorphic.
-- It's mainly useful for display purposes.
getCurrentStageInfo :: RuntimeState result -> (forall ctx a. StageInfo ctx a -> r) -> r
getCurrentStageInfo (RuntimeState _gen _ctx (CurrentStage stageInfo _pipeline) _history) f = f stageInfo

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
handleInput' _errorMsg doc InputRefresh spec (StageInfo c s _) =
  StateUpdateUpdate (StageInfo c s (spec.makeSuggestions doc s))
handleInput' errorMsg doc (InputKey k) spec (StageInfo c s suggestions) =
  case k of
    MEnter -> StateUpdateError errorMsg
    MBackspace ->
      if s == ""
        then StateUpdatePop
        else
          let s' = init s
           in StateUpdateUpdate (StageInfo c s' (spec.makeSuggestions doc s'))
    MPeriod ->
      case suggestions of
        (_, _, r) : _ -> StateUpdatePush r
        _ -> StateUpdateError "No matching alternatives"
    MChar char ->
      let s' = s <> [char]
          suggestions' = spec.makeSuggestions doc s'
       in case suggestions' of
            [] -> StateUpdateError "No matching alternatives"
            _ -> StateUpdateUpdate (StageInfo c s' suggestions')

-- ============================================================================
-- Generic Model and Actions
-- ============================================================================

-- | Generic selector model
data SelectorModel result = SelectorModel
  { document :: Document
  , selectedResults :: [result]
  , tooltipFor :: Maybe result
  , runtimeState :: RuntimeState result
  , error :: Maybe M.MisoString
  }
  deriving (Generic)

deriving instance Eq result => Eq (SelectorModel result)

-- | Generic selector actions
data SelectorAction result
  = SelectorUpdateDocument !Document
  | SelectorHandleKeyPress !M.KeyInfo
  | SelectorAddResult !result
  | SelectorDeleteResult !result
  | SelectorTooltipFor !(Maybe result)

-- | Create initial model
initialModel :: Pipeline '[] a result -> Document -> [result] -> SelectorModel result
initialModel pipeline doc results =
  SelectorModel
    { document = doc
    , selectedResults = results
    , tooltipFor = Nothing
    , runtimeState = initialize pipeline doc
    , error = Nothing
    }

-- | Handle keyboard input and advance/backtrack the state machine
handleKeyboardInput
  :: M.MisoString -- Error message for incomplete selection
  -> Document
  -> Input
  -> RuntimeState result
  -> Either result (RuntimeState result, Maybe M.MisoString)
handleKeyboardInput errorMsg doc input state@(RuntimeState gen ctx (CurrentStage stageInfo pipeline) history) =
  case handleInput' errorMsg doc input (getParser pipeline) stageInfo of
    StateUpdatePop ->
      -- Go back to previous stage
      Right (backspace state, Nothing)
    StateUpdatePush selected ->
      -- Advance to next stage or finish
      case pipeline of
        Stage _parser continue ->
          let newCtx = HCons selected ctx
              nextPipeline = continue ctx selected
           in case nextPipeline of
                Stage nextParser _nextContinue ->
                  Right
                    ( RuntimeState
                        (gen + 1) -- Increment generation
                        newCtx
                        (CurrentStage (initStageInfo newCtx nextParser doc) nextPipeline)
                        (state : history)
                    , Nothing
                    )
                Done finalParser _buildResult ->
                  Right
                    ( RuntimeState
                        (gen + 1) -- Increment generation
                        newCtx
                        (CurrentStage (initStageInfo newCtx finalParser doc) nextPipeline)
                        (state : history)
                    , Nothing
                    )
        Done _parser buildResult ->
          Left (buildResult ctx selected)
    StateUpdateUpdate stageInfo' ->
      Right (RuntimeState (gen + 1) ctx (CurrentStage stageInfo' pipeline) history, Nothing)
    StateUpdateError err ->
      Right (state, Just err)
  where
    getParser :: Pipeline ctx a result -> IncrementalParserSpec a
    getParser (Stage parser _) = parser
    getParser (Done parser _) = parser

-- ============================================================================
-- Generic View Helpers
-- ============================================================================

-- | Get the current input string
getCurrentInput :: RuntimeState result -> String
getCurrentInput state = getCurrentStageInfo state (\s -> s.currentInput)

-- | Get the current suggestions
getCurrentSuggestions :: RuntimeState result -> [M.MisoString]
getCurrentSuggestions state = getCurrentStageInfo state (\s -> map (\(_, d, _) -> d) s.currentSuggestions)

-- | Reconstruct context as list of strings using reconstructInput functions
--
-- Takes a function that extracts the parser for each stage from the pipeline.
-- This is pipeline-specific, so it must be provided by the caller.
reconstructContext
  :: RuntimeState result
  -> (forall ctx a. Pipeline ctx a result -> HList ctx -> [String])
  -> [String]
reconstructContext (RuntimeState _gen ctx (CurrentStage _stageInfo pipeline) _history) f = f pipeline ctx
