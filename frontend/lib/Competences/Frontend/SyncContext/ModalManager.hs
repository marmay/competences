{-# LANGUAGE GADTs #-}

-- |
-- Module      : Competences.Frontend.SyncContext.ModalManager
-- Description : Central modal manager with subscription-based updates
--
-- Provides a central modal manager that follows the SyncContext subscription pattern.
-- Any component can open/close modals, and the ModalHost subscribes to render them.
module Competences.Frontend.SyncContext.ModalManager
  ( -- * Types
    ModalManagerRef (..)
  , AnyModal (..)
  , Model (..)
  , ModalChange (..)

    -- * API
  , newModalManager
  , openModal
  , closeModal
  , subscribeModals
  )
where

import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Subscription.Util (createSub)
import UnliftIO (MVar, modifyMVar, modifyMVar_, newMVar)

-- | Existential wrapper for any modal component.
-- The component can have any model/action types.
data AnyModal where
  AnyModal :: (Eq m, Typeable m) => M.Component Model m a -> AnyModal

instance Eq AnyModal where
  AnyModal c1 == AnyModal c2 =
    case cast c2.model of
      Just m2 -> c1.model == m2
      Nothing -> False

-- Note: We need a Model type for the ModalHost context.
-- This is defined here to avoid circular imports.
-- The actual ModalHost component will use this.
data Model = Model
  { activeModal :: !(Maybe AnyModal)
  }
  deriving (Eq, Generic)

-- | Change notification sent to subscribers.
data ModalChange = ModalChange
  { modal :: !(Maybe AnyModal)
  , isInitial :: !Bool
  }

-- | Internal state for the modal manager.
data ModalState = ModalState
  { currentModal :: !(Maybe AnyModal)
  , handlers :: !(Map.Map Int ModalHandler)
  , nextHandlerId :: !Int
  }

-- | Handler GADT (like ChangedHandler in SyncDocument).
data ModalHandler where
  ModalHandler :: forall a. (ModalChange -> a) -> M.Sink a -> ModalHandler

-- | Reference to the modal manager.
newtype ModalManagerRef = ModalManagerRef (MVar ModalState)

-- | Create a new modal manager.
newModalManager :: IO ModalManagerRef
newModalManager = ModalManagerRef <$> newMVar emptyState
  where
    emptyState = ModalState { currentModal = Nothing, handlers = Map.empty, nextHandlerId = 0 }

-- | Open a modal with any component.
-- The caller constructs the component with whatever context is needed.
openModal :: (Eq m, Typeable m) => ModalManagerRef -> M.Component Model m a -> IO ()
openModal (ModalManagerRef ref) comp = do
  modifyMVar_ ref $ \s -> do
    let modal = Just (AnyModal comp)
    let s' = s { currentModal = modal }
    -- Notify all handlers
    forM_ s.handlers $ issueModalChange (ModalChange modal False)
    pure s'

-- | Close the current modal.
closeModal :: ModalManagerRef -> IO ()
closeModal (ModalManagerRef ref) = do
  modifyMVar_ ref $ \s -> do
    let s' = s { currentModal = Nothing }
    -- Notify all handlers
    forM_ s.handlers $ issueModalChange (ModalChange Nothing False)
    pure s'

-- | Subscribe to modal changes.
-- Uses createSub for automatic cleanup when the component unmounts.
subscribeModals :: ModalManagerRef -> (ModalChange -> a) -> M.Sink a -> IO ()
subscribeModals ref f sink = createSub acquire release sink
  where
    acquire = do
      (handlerId, initialModal) <- registerHandler ref f sink
      -- Send initial notification (outside MVar lock)
      sink $ f (ModalChange initialModal True)
      pure handlerId
    release = unregisterHandler ref

-- | Register a modal handler. Returns (handler ID, current modal).
registerHandler :: ModalManagerRef -> (ModalChange -> a) -> M.Sink a -> IO (Int, Maybe AnyModal)
registerHandler (ModalManagerRef ref) f sink = do
  modifyMVar ref $ \s ->
    pure
      ( s
          { handlers = Map.insert s.nextHandlerId (ModalHandler f sink) s.handlers
          , nextHandlerId = s.nextHandlerId + 1
          }
      , (s.nextHandlerId, s.currentModal)
      )

-- | Unregister a modal handler by ID.
unregisterHandler :: ModalManagerRef -> Int -> IO ()
unregisterHandler (ModalManagerRef ref) handlerId =
  modifyMVar_ ref $ \s ->
    pure s { handlers = Map.delete handlerId s.handlers }

-- | Send a modal change to a handler.
issueModalChange :: ModalChange -> ModalHandler -> IO ()
issueModalChange change (ModalHandler f sink) = sink $ f change
