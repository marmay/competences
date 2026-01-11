module Competences.Frontend.SyncContext.UIState
  ( -- * Focused User State
    FocusedUserState (..)
  , FocusedUserChange (..)
  , FocusedUserRef
    -- * Construction
  , mkFocusedUserRef
    -- * Operations
  , subscribeFocusedUser
  , registerFocusedUserHandler
  , unregisterFocusedUserHandler
  , setFocusedUser
  , readFocusedUser
  )
where

import Competences.Document (User (..))
import Competences.Document.User (isStudent)
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Subscription.Util (createSub)
import UnliftIO (MVar, MonadIO, liftIO, modifyMVar, modifyMVar_, newMVar, readMVar)

-- | State for the focused user feature, bundled in a single MVar
-- to avoid race conditions between handler registration and notifications
data FocusedUserState = FocusedUserState
  { focusedUser :: !(Maybe User)
  , onFocusedUserChanged :: !(Map.Map Int FocusedUserHandler)
  , nextFocusedUserHandlerId :: !Int
  }
  deriving (Generic)

data FocusedUserHandler where
  FocusedUserHandler :: forall a. (FocusedUserChange -> a) -> (M.Sink a) -> FocusedUserHandler

data FocusedUserChange = FocusedUserChange
  { user :: !(Maybe User)
  , isInitial :: !Bool
  }
  deriving (Eq, Show, Generic)

-- | Reference to focused user state
data FocusedUserRef = FocusedUserRef
  { state :: !(MVar FocusedUserState)
  , connectedUser :: !User
  }

-- | Get initial focused user based on role
initialFocusedUser :: User -> Maybe User
initialFocusedUser u
  | isStudent u = Just u -- Students always focus themselves
  | otherwise = Nothing -- Teachers start with no focus

-- | Create initial focused user state
mkFocusedUserState :: User -> FocusedUserState
mkFocusedUserState u =
  FocusedUserState
    { focusedUser = initialFocusedUser u
    , onFocusedUserChanged = Map.empty
    , nextFocusedUserHandlerId = 0
    }

-- | Create a FocusedUserRef
mkFocusedUserRef :: (MonadIO m) => User -> m FocusedUserRef
mkFocusedUserRef user = do
  st <- newMVar $ mkFocusedUserState user
  pure $ FocusedUserRef st user

-- | Subscribe to focused user changes
subscribeFocusedUser :: forall a. FocusedUserRef -> (FocusedUserChange -> a) -> M.Sink a -> IO ()
subscribeFocusedUser ref f sink = createSub acquire release sink
  where
    acquire = do
      (handlerId, initialUser) <- registerFocusedUserHandler ref f sink
      -- Send initial notification (outside MVar lock)
      sink $ f $ FocusedUserChange initialUser True
      pure handlerId
    release = unregisterFocusedUserHandler ref

-- | Register a focused user handler directly without using createSub.
-- This is for use within other subscriptions that need to compose handlers.
-- Returns (handler ID, current focused user) - caller should send initial notification
-- outside this call to avoid deadlock.
registerFocusedUserHandler :: forall a. FocusedUserRef -> (FocusedUserChange -> a) -> M.Sink a -> IO (Int, Maybe User)
registerFocusedUserHandler ref f sink = do
  modifyMVar ref.state $ \s ->
    let handlerId = s.nextFocusedUserHandlerId
        handler = FocusedUserHandler f sink
        newState = s
          { onFocusedUserChanged = Map.insert handlerId handler s.onFocusedUserChanged
          , nextFocusedUserHandlerId = handlerId + 1
          }
     in pure (newState, (handlerId, s.focusedUser))

-- | Unregister a focused user handler by ID.
unregisterFocusedUserHandler :: FocusedUserRef -> Int -> IO ()
unregisterFocusedUserHandler ref handlerId =
  modifyMVar_ ref.state $ \s ->
    pure s{onFocusedUserChanged = Map.delete handlerId s.onFocusedUserChanged}

-- | Set the focused user (only works for teachers; no-op for students)
setFocusedUser :: FocusedUserRef -> Maybe User -> IO ()
setFocusedUser ref newUser = do
  -- Check if connected user is a student (cannot change focus)
  if isStudent ref.connectedUser
    then pure () -- No-op for students
    else modifyMVar_ ref.state $ \s -> do
      let change = FocusedUserChange newUser False
      -- Notify all handlers
      forM_ (Map.elems s.onFocusedUserChanged) $ \(FocusedUserHandler f sink) ->
        sink (f change)
      pure s{focusedUser = newUser}

-- | Read current focused user
readFocusedUser :: (MonadIO m) => FocusedUserRef -> m (Maybe User)
readFocusedUser ref = liftIO $ (.focusedUser) <$> readMVar ref.state
