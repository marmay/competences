-- | Two-stage initialization for Miso components.
--
-- Components that depend on a 'Document' from a subscription cannot render
-- meaningful content before receiving the first update.  'deferredComponent'
-- cleanly separates "waiting for first document" from "ready to render",
-- eliminating placeholder initial models and flash-of-empty-content bugs.
--
-- @
-- myComponent r =
--   (deferredComponent
--     (\\case DocUpdated dc -> Just dc; _ -> Nothing)
--     initFromDocument
--     update
--     view
--   ) { M.subs = [subscribeDocument r DocUpdated] }
-- @
module Competences.Frontend.Component.Deferred
  ( -- * Types
    Initializing (..)

    -- * Component constructor
  , deferredComponent

    -- * Utilities
  , zoomReady
  , _Ready
  )
where

import Competences.Document (Document)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.SyncContext.SyncDocument (DocumentChange (..))
import Competences.Frontend.View.Typography qualified as Typography
import Control.Monad.RWS (ask, execRWS, get, put, tell)
import Data.Coerce (coerce)
import Miso qualified as M
import Optics.Core qualified as O

-- | Two-stage model wrapper. A component starts in 'Loading' and transitions
-- to @'Ready' model@ once the first 'Document' arrives via subscription.
data Initializing model = Loading | Ready !model
  deriving (Eq, Show)

-- | Construct a deferred component that waits for the first 'DocumentChange'
-- before initializing its inner model.
--
-- * @extractDoc@: pull a 'DocumentChange' out of an action (return 'Nothing'
--   for actions that are not document updates)
-- * @initModel@: build the initial inner model from the first 'Document'
-- * @innerUpdate@: the regular update function, operating on the inner model
-- * @innerView@: the regular view function; note the parent model is
--   @'Initializing' model@ (see '_Ready' for child component bindings)
deferredComponent
  :: (Eq model)
  => (action -> Maybe DocumentChange)
  -> (Document -> model)
  -> (action -> M.Effect (Initializing model) model action)
  -> (model -> M.View (Initializing model) action)
  -> M.Component parent (Initializing model) action
deferredComponent extractDoc initModel innerUpdate innerView =
  M.component Loading (deferredUpdate extractDoc initModel innerUpdate) (liftView innerView)

-- | The outer update function that handles the Loading → Ready transition.
--
-- When in 'Loading', only a 'DocumentChange' action triggers initialization.
-- Once in 'Ready', all actions (including subsequent document changes) are
-- forwarded to the inner update via 'zoomReady'.
deferredUpdate
  :: (action -> Maybe DocumentChange)
  -> (Document -> model)
  -> (action -> M.Effect (Initializing model) model action)
  -> action
  -> M.Effect parent (Initializing model) action
deferredUpdate extractDoc initModel innerUpdate action = do
  s <- get
  case s of
    Loading ->
      case extractDoc action of
        Just dc -> put (Ready (initModel dc.document))
        Nothing -> pure ()
    Ready _ ->
      zoomReady (innerUpdate action)

-- | Run an inner 'Effect' (operating on @model@) inside the outer 'Effect'
-- (operating on @'Initializing' model@). Only executes when the state is
-- 'Ready'; does nothing in 'Loading'.
zoomReady
  :: M.Effect (Initializing model) model action
  -> M.Effect parent (Initializing model) action
zoomReady inner = do
  s <- get
  case s of
    Loading -> pure ()
    Ready m -> do
      r <- ask
      -- ComponentInfo's parent type parameter is phantom, so coerce is safe.
      let (m', w) = execRWS inner (coerce r) m
      put (Ready m')
      tell w

-- | Wrap the inner view, showing a loading placeholder when not yet initialized.
liftView
  :: (model -> M.View (Initializing model) action)
  -> Initializing model
  -> M.View (Initializing model) action
liftView _ Loading = Typography.placeholder (C.translate' C.LblInitializing)
liftView innerView (Ready m) = innerView m

-- | Partial lens into the 'Ready' state.
--
-- This is safe to use in child component bindings because 'liftView' never
-- renders child components in the 'Loading' state — bindings are only
-- evaluated when the component is mounted, which only happens in 'Ready'.
--
-- __Do not use this lens outside of component bindings.__
_Ready :: O.Lens' (Initializing model) model
_Ready = O.lens getter setter
  where
    getter (Ready m) = m
    getter Loading = error "Deferred._Ready: accessed in Loading state"
    setter (Ready _) m = Ready m
    setter Loading _ = Loading
