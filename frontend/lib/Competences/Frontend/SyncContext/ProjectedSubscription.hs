-- | Combined subscription for document + focused user with projection support.
--
-- This module provides a subscription mechanism that:
--
-- 1. Atomically delivers document + focused user changes
-- 2. Allows components to define efficient projections (extract only needed data)
-- 3. Deduplicates updates when projection hasn't changed
--
-- Usage:
--
-- @
-- data MyProjection = MyProjection
--   { evidences :: !(IxSet EvidenceIxs Evidence)
--   , focusedUser :: !(Maybe User)
--   }
--   deriving (Eq, Generic, Show)
--
-- myProjection :: Document -> Maybe User -> MyProjection
-- myProjection doc mUser = MyProjection
--   { evidences = case mUser of
--       Nothing -> Ix.empty
--       Just u -> doc.evidences Ix.@= u.id
--   , focusedUser = mUser
--   }
--
-- myComponent r =
--   (M.component model update view)
--     { M.subs = [subscribeWithProjection r myProjection ProjectionChanged]
--     }
-- @
module Competences.Frontend.SyncContext.ProjectedSubscription
  ( -- * Types
    ProjectedChange (..)
  , ChangeInfo (..)
    -- * Subscription
  , subscribeWithProjection
  )
where

import Competences.Document (Document, User)
import Competences.Frontend.SyncContext.SyncDocument
  ( SyncContext (..)
  , SyncDocument (..)
  , DocumentChange (..)
  , getFocusedUserRef
  , readSyncDocument
  , registerDocumentHandler
  , unregisterDocumentHandler
  )
import Competences.Frontend.SyncContext.UIState
  ( FocusedUserChange (..)
  , FocusedUserRef
  , readFocusedUser
  , registerFocusedUserHandler
  , unregisterFocusedUserHandler
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Subscription.Util (createSub)

-- | A change notification containing the projected data and information about what changed.
data ProjectedChange a = ProjectedChange
  { projection :: !a
  -- ^ The projected data extracted from document + focused user
  , changeInfo :: !ChangeInfo
  -- ^ Information about what triggered this change
  }
  deriving (Eq, Show, Generic)

-- | Information about what triggered a projection change.
data ChangeInfo
  = InitialSnapshot
  -- ^ First notification when subscribing
  | DocumentOnly
  -- ^ Only the document changed
  | FocusedUserOnly
  -- ^ Only the focused user changed
  | Both
  -- ^ Both document and focused user changed
  deriving (Eq, Show, Generic)

-- | Internal state for tracking the combined subscription.
-- Stores the last projection and handler IDs for cleanup.
data CombinedState a = CombinedState
  { lastProjection :: !(Maybe a)
  , documentHandlerId :: !Int
  , focusedUserHandlerId :: !Int
  }

-- | Subscribe to document and focused user changes with a projection function.
--
-- The projection function extracts only the data the component needs from
-- the document and focused user. Notifications are only sent when the
-- projection actually changes, which reduces unnecessary re-renders.
--
-- This subscription:
--
-- 1. Registers handlers for both document and focused user subscriptions
-- 2. When either fires, computes @project doc focusedUser@
-- 3. Compares with last-sent projection
-- 4. Only sends notification if projection changed
subscribeWithProjection
  :: forall a action. (Eq a)
  => SyncContext
  -- ^ The sync context containing document and focused user state
  -> (Document -> Maybe User -> a)
  -- ^ Projection function: extracts needed data from document + focused user
  -> (ProjectedChange a -> action)
  -- ^ Action constructor to wrap the change
  -> M.Sink action
  -- ^ Sink to send actions to
  -> IO ()
subscribeWithProjection ctx project toAction sink = createSub acquire release sink
  where
    -- Get the FocusedUserRef from SyncContext
    focusedUserRef :: FocusedUserRef
    focusedUserRef = getFocusedUserRef ctx

    acquire :: IO (IORef (CombinedState a))
    acquire = do
      -- Create state ref to track last projection (initially empty)
      stateRef <- newIORef $ CombinedState Nothing 0 0

      -- Register handlers using direct registration (no nested createSub)
      -- These return initial values outside the MVar lock
      (docHandlerId, initialDoc) <- registerDocumentHandler ctx (handleDocumentChange stateRef) id
      (userHandlerId, initialUser) <- registerFocusedUserHandler focusedUserRef (handleFocusedUserChange stateRef) id

      -- Compute initial projection and update state
      let initialProj = project initialDoc initialUser
      writeIORef stateRef $ CombinedState (Just initialProj) docHandlerId userHandlerId

      -- Send initial snapshot (outside MVar locks)
      sink $ toAction $ ProjectedChange initialProj InitialSnapshot

      pure stateRef

    release :: IORef (CombinedState a) -> IO ()
    release stateRef = do
      -- Unregister handlers to prevent memory leaks
      st <- readIORef stateRef
      unregisterDocumentHandler ctx st.documentHandlerId
      unregisterFocusedUserHandler focusedUserRef st.focusedUserHandlerId

    handleDocumentChange :: IORef (CombinedState a) -> DocumentChange -> IO ()
    handleDocumentChange stateRef docChange = do
      -- Use the document from the change notification (avoids re-reading MVar)
      -- Read focused user from its MVar (different MVar, so safe)
      mUser <- readFocusedUser focusedUserRef
      let newProj = project docChange.document mUser

      -- Check if projection changed
      st <- readIORef stateRef
      case st.lastProjection of
        Just lastProj | lastProj == newProj -> pure () -- No change, skip
        _ -> do
          -- Update state and send notification
          writeIORef stateRef $ st { lastProjection = Just newProj }
          sink $ toAction $ ProjectedChange newProj DocumentOnly

    handleFocusedUserChange :: IORef (CombinedState a) -> FocusedUserChange -> IO ()
    handleFocusedUserChange stateRef userChange = do
      -- Skip initial notification (we already sent our own initial snapshot)
      if userChange.isInitial
        then pure ()
        else do
          -- Read current document from its MVar (different MVar, so safe)
          syncDoc <- readSyncDocument ctx
          -- Use the user from the change notification directly
          let newProj = project syncDoc.localDocument userChange.user

          -- Check if projection changed
          st <- readIORef stateRef
          case st.lastProjection of
            Just lastProj | lastProj == newProj -> pure ()
            _ -> do
              -- Update state and send notification
              writeIORef stateRef $ st { lastProjection = Just newProj }
              sink $ toAction $ ProjectedChange newProj FocusedUserOnly
