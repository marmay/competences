{-# LANGUAGE GADTs #-}

-- |
-- Module      : Competences.Frontend.SyncContext.WindowManager
-- Description : Unified window manager for modals and pinned dialogs
--
-- Manages two types of windows:
--
-- * __Modals__: Blocking dialogs with backdrop. At most one active at a time.
--   For confirmations, imports, quick edits.
--
-- * __Pinned dialogs__: Non-blocking, minimizable, persistent across navigation.
--   For long-lived work surfaces like lesson evaluation. At most one visible at a time;
--   opening/restoring a pin minimizes the currently visible one. Deduplicated by 'PinId'.
module Competences.Frontend.SyncContext.WindowManager
  ( -- * Types
    WindowManagerRef (..)
  , AnyModal (..)
  , AnyPinnedDialog (..)
  , PinId (..)
  , PinVisibility (..)
  , Model (..)
  , WindowChange (..)

    -- * Construction
  , newWindowManager

    -- * Modal API (blocking dialogs)
  , openModal
  , closeModal

    -- * Pin API (persistent dialogs)
  , pinDialog
  , unpinDialog
  , minimizeDialog
  , restoreDialog
  , toggleDialog

    -- * Subscription
  , subscribeWindows
  )
where

import Competences.Frontend.View.Icon (Icon)
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (MisoString)
import Miso.Subscription.Util (createSub)
import UnliftIO (MVar, modifyMVar, modifyMVar_, newMVar)

-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

-- | Unique identifier for a pinned dialog, provided by the caller.
-- Used for deduplication: pinning with an existing 'PinId' restores that dialog
-- instead of creating a new one.
newtype PinId = PinId Text
  deriving (Eq, Ord, Show)

-- | Visibility state of a pinned dialog.
data PinVisibility = PinVisible | PinMinimized
  deriving (Eq, Show)

-- | Existential wrapper for a modal component.
data AnyModal where
  AnyModal :: (Eq m, Typeable m) => M.Component Model m a -> AnyModal

instance Eq AnyModal where
  AnyModal c1 == AnyModal c2 =
    case cast c2.model of
      Just m2 -> c1.model == m2
      Nothing -> False

-- | Existential wrapper for a pinned dialog component.
data AnyPinnedDialog where
  AnyPinnedDialog ::
    (Eq m, Typeable m) =>
    { apdComponent :: M.Component Model m a
    , apdIcon :: Icon
    , apdTitle :: MisoString
    } ->
    AnyPinnedDialog

instance Eq AnyPinnedDialog where
  AnyPinnedDialog c1 i1 t1 == AnyPinnedDialog c2 i2 t2 =
    i1 == i2 && t1 == t2 && case cast c2.model of
      Just m2 -> c1.model == m2
      Nothing -> False

-- | Host model used as the parent type for all managed components.
-- Defined here to avoid circular imports with WindowHost.
data Model = Model
  { activeModal :: !(Maybe AnyModal)
  , pinnedDialogs :: !(Map.Map PinId (AnyPinnedDialog, PinVisibility))
  , pinOrder :: ![PinId]
  }
  deriving (Eq, Generic)

-- | Change notification sent to subscribers.
data WindowChange = WindowChange
  { activeModal :: !(Maybe AnyModal)
  , pinnedDialogs :: !(Map.Map PinId (AnyPinnedDialog, PinVisibility))
  , pinOrder :: ![PinId]
  , isInitial :: !Bool
  }

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

data WindowState = WindowState
  { currentModal :: !(Maybe AnyModal)
  , pins :: !(Map.Map PinId AnyPinnedDialog)
  , pinVisibility :: !(Map.Map PinId PinVisibility)
  , pinOrder :: ![PinId]
  , handlers :: !(Map.Map Int WindowHandler)
  , nextHandlerId :: !Int
  }

data WindowHandler where
  WindowHandler :: forall a. (WindowChange -> a) -> M.Sink a -> WindowHandler

-- | Reference to the window manager.
newtype WindowManagerRef = WindowManagerRef (MVar WindowState)

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Create a new window manager.
newWindowManager :: IO WindowManagerRef
newWindowManager = WindowManagerRef <$> newMVar emptyState
  where
    emptyState =
      WindowState
        { currentModal = Nothing
        , pins = Map.empty
        , pinVisibility = Map.empty
        , pinOrder = []
        , handlers = Map.empty
        , nextHandlerId = 0
        }

-- ---------------------------------------------------------------------------
-- Modal API
-- ---------------------------------------------------------------------------

-- | Open a blocking modal. Renders above everything including pinned dialogs.
-- Replaces any currently open modal.
openModal :: (Eq m, Typeable m) => WindowManagerRef -> M.Component Model m a -> IO ()
openModal (WindowManagerRef ref) comp = do
  modifyMVar_ ref $ \s -> do
    let s' = s {currentModal = Just (AnyModal comp)}
    notifyHandlers s'
    pure s'

-- | Close the active modal.
closeModal :: WindowManagerRef -> IO ()
closeModal (WindowManagerRef ref) = do
  modifyMVar_ ref $ \s -> do
    let s' = s {currentModal = Nothing}
    notifyHandlers s'
    pure s'

-- ---------------------------------------------------------------------------
-- Pin API
-- ---------------------------------------------------------------------------

-- | Pin a dialog. If the 'PinId' already exists, the existing dialog is made
-- visible (restored). If it is new, it is added and made visible. In both cases,
-- any previously visible pin is minimized.
pinDialog :: WindowManagerRef -> PinId -> AnyPinnedDialog -> IO ()
pinDialog (WindowManagerRef ref) pid dialog = do
  modifyMVar_ ref $ \s -> do
    let s' = if Map.member pid s.pins
          then -- Existing pin: just restore it
            makeVisible pid s
          else -- New pin: add and make visible
            let withNewPin = s
                  { pins = Map.insert pid dialog s.pins
                  , pinVisibility = Map.insert pid PinMinimized s.pinVisibility
                  , pinOrder = s.pinOrder ++ [pid]
                  }
             in makeVisible pid withNewPin
    notifyHandlers s'
    pure s'

-- | Remove a pinned dialog entirely.
unpinDialog :: WindowManagerRef -> PinId -> IO ()
unpinDialog (WindowManagerRef ref) pid = do
  modifyMVar_ ref $ \s -> do
    let s' = s
          { pins = Map.delete pid s.pins
          , pinVisibility = Map.delete pid s.pinVisibility
          , pinOrder = filter (/= pid) s.pinOrder
          }
    notifyHandlers s'
    pure s'

-- | Minimize a pinned dialog (hide it).
minimizeDialog :: WindowManagerRef -> PinId -> IO ()
minimizeDialog (WindowManagerRef ref) pid = do
  modifyMVar_ ref $ \s -> do
    let s' = s {pinVisibility = Map.adjust (const PinMinimized) pid s.pinVisibility}
    notifyHandlers s'
    pure s'

-- | Restore a pinned dialog (make it visible). Minimizes any currently visible pin.
restoreDialog :: WindowManagerRef -> PinId -> IO ()
restoreDialog (WindowManagerRef ref) pid = do
  modifyMVar_ ref $ \s -> do
    let s' = makeVisible pid s
    notifyHandlers s'
    pure s'

-- | Toggle a pinned dialog: if visible, minimize; if minimized, restore.
toggleDialog :: WindowManagerRef -> PinId -> IO ()
toggleDialog (WindowManagerRef ref) pid = do
  modifyMVar_ ref $ \s -> do
    let s' = case Map.lookup pid s.pinVisibility of
          Just PinVisible -> s {pinVisibility = Map.insert pid PinMinimized s.pinVisibility}
          Just PinMinimized -> makeVisible pid s
          Nothing -> s -- PinId not found, no-op
    notifyHandlers s'
    pure s'

-- ---------------------------------------------------------------------------
-- Subscription
-- ---------------------------------------------------------------------------

-- | Subscribe to window changes.
-- Uses 'createSub' for automatic cleanup when the component unmounts.
subscribeWindows :: WindowManagerRef -> (WindowChange -> a) -> M.Sink a -> IO ()
subscribeWindows ref f sink = createSub acquire release sink
  where
    acquire = do
      (handlerId, initialChange) <- registerHandler ref f sink
      -- Send initial notification (outside MVar lock)
      sink $ f initialChange
      pure handlerId
    release = unregisterHandler ref

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Make a pin visible, minimizing any currently visible pin.
makeVisible :: PinId -> WindowState -> WindowState
makeVisible pid s =
  s {pinVisibility = Map.insert pid PinVisible $ minimizeAll s.pinVisibility}
  where
    minimizeAll = Map.map (const PinMinimized)

-- | Build the combined pin map (dialog + visibility) for notifications.
buildPinnedDialogs :: WindowState -> Map.Map PinId (AnyPinnedDialog, PinVisibility)
buildPinnedDialogs s =
  Map.intersectionWith (,) s.pins s.pinVisibility

-- | Build a 'WindowChange' from the current state.
mkWindowChange :: Bool -> WindowState -> WindowChange
mkWindowChange initial s =
  WindowChange
    { activeModal = s.currentModal
    , pinnedDialogs = buildPinnedDialogs s
    , pinOrder = s.pinOrder
    , isInitial = initial
    }

-- | Notify all registered handlers of the current state.
notifyHandlers :: WindowState -> IO ()
notifyHandlers s = do
  let change = mkWindowChange False s
  forM_ s.handlers $ issueChange change

-- | Send a change to a single handler.
issueChange :: WindowChange -> WindowHandler -> IO ()
issueChange change (WindowHandler f sink) = sink $ f change

-- | Register a handler. Returns (handler ID, initial change).
registerHandler :: WindowManagerRef -> (WindowChange -> a) -> M.Sink a -> IO (Int, WindowChange)
registerHandler (WindowManagerRef ref) f sink = do
  modifyMVar ref $ \s ->
    pure
      ( s
          { handlers = Map.insert s.nextHandlerId (WindowHandler f sink) s.handlers
          , nextHandlerId = s.nextHandlerId + 1
          }
      , (s.nextHandlerId, mkWindowChange True s)
      )

-- | Unregister a handler by ID.
unregisterHandler :: WindowManagerRef -> Int -> IO ()
unregisterHandler (WindowManagerRef ref) handlerId =
  modifyMVar_ ref $ \s ->
    pure s {handlers = Map.delete handlerId s.handlers}
