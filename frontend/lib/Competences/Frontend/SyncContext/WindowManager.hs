{-# LANGUAGE GADTs #-}

-- |
-- Module      : Competences.Frontend.SyncContext.WindowManager
-- Description : Unified window manager for modals and pinned dialogs
--
-- Manages two types of windows:
--
-- * __Modals__: Blocking dialogs with backdrop. Supports stacking (most recent on top).
--   For confirmations, imports, quick edits.
--
-- * __Pinned dialogs__: Non-blocking, minimizable, persistent across navigation.
--   For long-lived work surfaces like lesson evaluation. At most one visible at a time;
--   opening/restoring a pin minimizes the currently visible one. Deduplicated by 'PinId'.
module Competences.Frontend.SyncContext.WindowManager
  ( -- * Shared chrome types
    WindowChrome (..)
  , ModalConfig (..)
  , ModalId (..)
  , ModalWidth (..)
  , ModalHeight (..)

    -- * Types
  , WindowEventSink (..)
  , WindowEvent (..)
  , AnyModal (..)
  , AnyPinnedDialog (..)
  , PinId -- no (..)
  , mkPinId
  , pinIdKey
  , PinCategory (..)
  , PinMeta (..)
  , SortAtom (..)
  , SortKey (..)
  , PinVisibility (..)
  , Model (..)

    -- * Construction
  , mkWindowEventSink
  , WindowEventSinkInstaller
  , installWindowEventSink

    -- * Lock watching
  , LockWatchConfig (..)
  , startLockWatching

    -- * Modal API (blocking dialogs)
  , openModal
  , closeModal

    -- * Pin API (persistent dialogs)
  , unpinDialog
  , minimizeDialog
  , restoreDialog
  , toggleDialog

    -- * Window mode (opaque)
  , WindowMode -- no (..)
  , isModal
  , isPinned
  , isPinnedOrModal
  , isInline
  , closeWindow
  , closeWhenModal
  , closeWhenPinned
  , closeWhenPinnedOrModal

    -- * Pin state persistence
  , pinSaveStateLens

    -- * Component mounting
  , inlineComponent
  , inlineComponentAttrs
  , inlineComponentWith
  , openFramedModal
  , openFramedModalWith
  , pinDialog
  , pinDialogWith
  )
where

import Competences.Document (Document (..), Lock, LockHolder (..), UserId)
import Competences.Document.Session (SessionId)
import Competences.Frontend.View.Icon qualified as Icon
import Control.Monad (forM_, when)
import Data.Dynamic (Dynamic, Typeable, fromDynamic, toDyn)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Typeable (cast)
import GHC.Generics (Generic)
import Optics.Core qualified as O
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)
import UnliftIO (newMVar, readMVar, swapMVar)

-- ---------------------------------------------------------------------------
-- Shared chrome types
-- ---------------------------------------------------------------------------

-- | Shared window chrome for both modals and pinned dialogs.
-- Defines the icon and title displayed in the title bar.
data WindowChrome = WindowChrome
  { title :: !MisoString
  , icon :: !Icon.Icon
  , iconBadge :: !(Maybe Icon.Icon)
  -- ^ Optional small badge icon at the bottom-right of the main icon.
  -- Used for entity-specific actions (e.g., task icon + edit badge).
  }
  deriving (Eq, Show)

-- | Width of the modal dialog.
data ModalWidth
  = -- | Wide modal (~85vw, capped). For editors, importers.
    ModalWide
  | -- | Narrow modal (~max-w-lg). For dialogs, confirmations.
    ModalNarrow
  deriving (Eq, Show)

-- | Height of the modal dialog.
data ModalHeight
  = -- | Full height (~90vh), content scrolls inside.
    ModalFull
  | -- | Content-driven height, max ~90vh with scroll on overflow.
    ModalAuto
  deriving (Eq, Show)

-- | Unique identifier for a modal, used as the Miso component key.
-- Different 'ModalId' values force Miso to unmount the old component and
-- mount a new one, which is essential when replacing one modal with another.
newtype ModalId = ModalId Text
  deriving (Eq, Show)

-- | Configuration for a framed modal.
data ModalConfig = ModalConfig
  { chrome :: !WindowChrome
  , modalId :: !ModalId
  , width :: !ModalWidth
  , height :: !ModalHeight
  , pinnable :: !(Maybe ())
  -- ^ @Just ()@ = show pin button (pinning logic handled by WindowHost)
  }

-- ---------------------------------------------------------------------------
-- Pin metadata
-- ---------------------------------------------------------------------------

-- | Category of a pinned dialog, used for ordering in the sidebar.
data PinCategory
  = PinCatTask
  | PinCatResource
  | PinCatAssignment
  | PinCatLesson
  | PinCatLessonEvaluation
  | PinCatLessonNotes
  | PinCatCompetenceGrid
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | A single sort component. Two atoms of the same runtime type compare
-- via their Ord instance; two atoms of different types compare as EQ.
data SortAtom where
  SortAtom :: (Ord a, Typeable a) => !a -> SortAtom

instance Eq SortAtom where
  SortAtom a == SortAtom b = case cast b of
    Just b' -> a == b'
    Nothing -> True

instance Ord SortAtom where
  compare (SortAtom a) (SortAtom b) = case cast b of
    Just b' -> compare a b'
    Nothing -> EQ

instance Show SortAtom where
  show _ = "SortAtom"

-- | Lexicographic sort key built from heterogeneous atoms.
newtype SortKey = SortKey [SortAtom]
  deriving (Eq, Ord, Show)

-- | Metadata for a pinned dialog. Callers construct this; internally
-- the 'key' is used to derive the deduplication 'PinId'.
data PinMeta = PinMeta
  { key :: !Text
  , category :: !PinCategory
  , sortKey :: !SortKey
  , context :: !(Maybe MisoString)
  }
  deriving (Eq, Show)

-- | Extract a 'PinId' from 'PinMeta'.
toPinId :: PinMeta -> PinId
toPinId meta = PinId meta.key

-- ---------------------------------------------------------------------------
-- Window mode (opaque)
-- ---------------------------------------------------------------------------

-- | Rendering context for a component. Constructor not exported -- only
-- the mounting helpers ('inlineComponentWith', 'openFramedModalWith',
-- 'pinDialogWith') can create values, so the context is always correct.
data WindowMode = WindowMode
  { _contextTag :: !ContextTag
  , _closeAction :: !(IO ())
  }

-- | Internal tag -- NOT exported.
data ContextTag = CInline | CModal | CPinned
  deriving (Eq)

-- Smart constructors (internal)
mkModalMode :: IO () -> WindowMode
mkModalMode = WindowMode CModal

mkPinnedMode :: IO () -> WindowMode
mkPinnedMode = WindowMode CPinned

-- | Inline rendering context (no close action).
inlineMode :: WindowMode
inlineMode = WindowMode CInline (pure ())

-- | Is this a modal context?
isModal :: WindowMode -> Bool
isModal wm = wm._contextTag == CModal

-- | Is this a pinned context?
isPinned :: WindowMode -> Bool
isPinned wm = wm._contextTag == CPinned

-- | Is this either a modal or pinned context?
isPinnedOrModal :: WindowMode -> Bool
isPinnedOrModal wm = wm._contextTag /= CInline

-- | Is this an inline context?
isInline :: WindowMode -> Bool
isInline wm = wm._contextTag == CInline

-- | Execute the close action for this window (no-op for inline).
closeWindow :: WindowMode -> IO ()
closeWindow = (._closeAction)

-- | Close only if the mode is modal.
closeWhenModal :: WindowMode -> IO ()
closeWhenModal wm = when (isModal wm) wm._closeAction

-- | Close only if the mode is pinned.
closeWhenPinned :: WindowMode -> IO ()
closeWhenPinned wm = when (isPinned wm) wm._closeAction

-- | Close if the mode is either modal or pinned.
closeWhenPinnedOrModal :: WindowMode -> IO ()
closeWhenPinnedOrModal wm = when (isPinnedOrModal wm) wm._closeAction

-- | Lens into the saved state for a specific pin in the host model.
-- Used as the parent side of a binding to persist pin component patches.
-- The 'Dynamic' wraps the pin's typed state (e.g., 'TaskPatch').
pinSaveStateLens :: (Typeable a) => PinId -> O.Lens' Model (Maybe a)
pinSaveStateLens pid = O.lens getter setter
  where
    getter m = Map.lookup pid m.pinSaveStates >>= fromDynamic
    setter m Nothing = m
      { pinSaveStates = Map.delete pid m.pinSaveStates
      , pinSaveGen = m.pinSaveGen + 1
      }
    setter m (Just a) = m
      { pinSaveStates = Map.insert pid (toDyn a) m.pinSaveStates
      , pinSaveGen = m.pinSaveGen + 1
      }

-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

-- | Unique identifier for a pinned dialog, provided by the caller.
-- Used for deduplication: pinning with an existing 'PinId' restores that dialog
-- instead of creating a new one.
newtype PinId = PinId Text
  deriving (Eq, Ord, Show)

-- | Construct a 'PinId' from a key string.
mkPinId :: Text -> PinId
mkPinId = PinId

-- | Extract the key string from a 'PinId'.
pinIdKey :: PinId -> Text
pinIdKey (PinId k) = k

-- | Visibility state of a pinned dialog.
data PinVisibility = PinVisible | PinMinimized
  deriving (Eq, Show)

-- | Existential wrapper for a modal component with its configuration.
data AnyModal where
  AnyModal
    :: (Eq m, Typeable m)
    => !(M.Component Model m a)
    -> !ModalConfig
    -> AnyModal

instance Eq AnyModal where
  AnyModal c1 _ == AnyModal c2 _ =
    case cast c2.model of
      Just m2 -> c1.model == m2
      Nothing -> False

-- | Existential wrapper for a pinned dialog component with its chrome and metadata.
-- Stores a factory function that creates the component from optional saved state,
-- allowing fresh components with restored patches after dormancy.
data AnyPinnedDialog where
  AnyPinnedDialog
    :: (Eq m, Typeable m, Typeable s)
    => !(Maybe s -> M.Component Model m a)
    -- ^ Typed factory: create component from optional saved state.
    -- Called by the view on each render; Miso's keying ensures existing
    -- instances are reused (factory output only matters on first mount).
    -> !WindowChrome
    -> !PinMeta
    -> AnyPinnedDialog

instance Eq AnyPinnedDialog where
  AnyPinnedDialog _ ch1 m1 == AnyPinnedDialog _ ch2 m2 =
    ch1 == ch2 && m1 == m2

-- | Host model used as the parent type for all managed components.
-- Defined here to avoid circular imports with WindowHost.
data Model = Model
  { modalStack :: ![AnyModal]
  , pinnedDialogs :: !(Map.Map PinId (AnyPinnedDialog, PinVisibility))
  , pinOrder :: ![PinId]
  , pinSaveStates :: !(Map.Map PinId Dynamic)
  -- ^ Persisted component state for dormant pins. Updated via bindings
  -- from pin components; read by the factory when creating fresh components.
  , pinSaveGen :: !Int
  -- ^ Generation counter, incremented by pinSaveStateLens on each write.
  -- Included in Eq so Miso persists binding-written model updates.
  -- (Dynamic has no Eq instance, so we can't compare pinSaveStates directly.)
  }
  deriving (Generic)

instance Eq Model where
  a == b =
    a.modalStack == b.modalStack
      && a.pinnedDialogs == b.pinnedDialogs
      && a.pinOrder == b.pinOrder
      && a.pinSaveGen == b.pinSaveGen

-- ---------------------------------------------------------------------------
-- Window events and event sink
-- ---------------------------------------------------------------------------

-- | All window operations as events dispatched to the WindowHost.
data WindowEvent
  = WEOpenModal !AnyModal
  | WECloseTopModal
  | WEPinDialog !AnyPinnedDialog
  | WEUnpinDialog !PinId
  | WETogglePin !PinId
  | WEMinimizePin !PinId
  | WERestorePin !PinId

-- | Event sink for dispatching window events to the WindowHost.
-- The sink is an IO action that accepts events. Before the WindowHost
-- mounts, events are silently dropped (the placeholder no-ops).
newtype WindowEventSink = WindowEventSink (WindowEvent -> IO ())

-- | Installs the real event handler on WindowHost mount and flushes any
-- events that were emitted before mount.
newtype WindowEventSinkInstaller
  = WindowEventSinkInstaller ((WindowEvent -> IO ()) -> IO ())

-- | Install the real handler. Events emitted before this call are flushed
-- to the handler in the order they were emitted.
installWindowEventSink :: WindowEventSinkInstaller -> (WindowEvent -> IO ()) -> IO ()
installWindowEventSink (WindowEventSinkInstaller f) = f

-- | Create a 'WindowEventSink' and the installer used by the WindowHost
-- to register the real handler. Events emitted before install are buffered
-- and flushed in order when 'installWindowEventSink' is called.
mkWindowEventSink :: IO (WindowEventSink, WindowEventSinkInstaller)
mkWindowEventSink = do
  bufferRef <- newIORef []
  let bufferingHandler ev = modifyIORef' bufferRef (ev :)
  sinkRef <- newMVar bufferingHandler
  let sink = WindowEventSink (\ev -> do f <- readMVar sinkRef; f ev)
      installer = WindowEventSinkInstaller $ \realHandler -> do
        _ <- swapMVar sinkRef realHandler
        buffered <- atomicModifyIORef' bufferRef (\xs -> ([], reverse xs))
        mapM_ realHandler buffered
  pure (sink, installer)

-- ---------------------------------------------------------------------------
-- Modal API
-- ---------------------------------------------------------------------------

-- | Open a blocking modal. Renders above everything including pinned dialogs.
-- Pushes onto the modal stack; the new modal appears on top.
openModal :: WindowEventSink -> AnyModal -> IO ()
openModal (WindowEventSink f) modal = f (WEOpenModal modal)

-- | Open a framed modal for a component that ignores 'WindowMode'.
-- Convenience wrapper: @openFramedModal sink cfg c = openFramedModalWith sink cfg (const c)@
openFramedModal
  :: (Eq m, Typeable m)
  => WindowEventSink
  -> ModalConfig
  -> M.Component Model m a
  -> IO ()
openFramedModal sink cfg comp =
  openFramedModalWith sink cfg (const comp)

-- | Close the topmost modal. If multiple modals are stacked, reveals the next one.
closeModal :: WindowEventSink -> IO ()
closeModal (WindowEventSink f) = f WECloseTopModal

-- ---------------------------------------------------------------------------
-- Pin API
-- ---------------------------------------------------------------------------

-- | Remove a pinned dialog entirely.
-- The WindowHost handles clearing save state and invoking the onPinClosed callback.
unpinDialog :: WindowEventSink -> PinId -> IO ()
unpinDialog (WindowEventSink f) pid = f (WEUnpinDialog pid)

-- | Minimize a pinned dialog (hide it).
minimizeDialog :: WindowEventSink -> PinId -> IO ()
minimizeDialog (WindowEventSink f) pid = f (WEMinimizePin pid)

-- | Restore a pinned dialog (make it visible). Minimizes any currently visible pin.
restoreDialog :: WindowEventSink -> PinId -> IO ()
restoreDialog (WindowEventSink f) pid = f (WERestorePin pid)

-- | Toggle a pinned dialog: if visible, minimize; if minimized, restore.
toggleDialog :: WindowEventSink -> PinId -> IO ()
toggleDialog (WindowEventSink f) pid = f (WETogglePin pid)

-- ---------------------------------------------------------------------------
-- Mounting helpers
-- ---------------------------------------------------------------------------

-- | Mount a component inline with HTML attributes on the wrapper div.
inlineComponentAttrs :: (Eq m) => M.MisoString -> [M.Attribute a'] -> M.Component p m a -> M.View p a'
inlineComponentAttrs name attrs c =
  M.div_ attrs [name M.+> c]

-- | Mount a component inline.
inlineComponent :: (Eq m) => M.MisoString -> M.Component p m a -> M.View p a'
inlineComponent name c = inlineComponentAttrs name [] c

-- | Mount a 'WindowMode'-aware component inline.
inlineComponentWith
  :: (Eq m)
  => M.MisoString
  -> (WindowMode -> M.Component p m a)
  -> M.View p a'
inlineComponentWith key mkComp =
  inlineComponent key (mkComp inlineMode)

-- | Open a framed modal, injecting modal 'WindowMode' into the component.
openFramedModalWith
  :: (Eq m, Typeable m)
  => WindowEventSink
  -> ModalConfig
  -> (WindowMode -> M.Component Model m a)
  -> IO ()
openFramedModalWith sink cfg mkComp =
  let mode = mkModalMode (closeModal sink)
   in openModal sink (AnyModal (mkComp mode) cfg)

-- | Pin a dialog, injecting pinned 'WindowMode' into the component factory.
-- The factory receives optional saved state ('Dynamic') when the component
-- is (re-)created after dormancy.
pinDialogWith
  :: (Eq m, Typeable m, Typeable s)
  => WindowEventSink
  -> PinMeta
  -> WindowChrome
  -> (WindowMode -> Maybe s -> M.Component Model m a)
  -> IO ()
pinDialogWith sink meta chrome mkComp =
  let pid = toPinId meta
      mode = mkPinnedMode (unpinDialog sink pid)
      dialog = AnyPinnedDialog (mkComp mode) chrome meta
   in let WindowEventSink f = sink in f (WEPinDialog dialog)

-- | Pin a dialog for a component that ignores 'WindowMode' and saved state.
pinDialog
  :: (Eq m, Typeable m)
  => WindowEventSink
  -> PinMeta
  -> WindowChrome
  -> M.Component Model m a
  -> IO ()
pinDialog sink meta chrome comp =
  pinDialogWith sink meta chrome (\_ (_ :: Maybe ()) -> comp)

-- ---------------------------------------------------------------------------
-- Lock watching
-- ---------------------------------------------------------------------------

-- | Configuration for lock-watching, provided by the caller to avoid
-- circular module dependencies.
data LockWatchConfig = LockWatchConfig
  { userId :: !UserId
  -- ^ The connected user's ID
  , sessionId :: !SessionId
  -- ^ The session ID
  , subscribeDocChanges :: !((Document -> IO ()) -> IO (IO ()))
  -- ^ Subscribe to document changes; returns unsubscribe action.
  -- The callback receives the current document on each change.
  , ensurePin :: !(WindowEventSink -> Document -> Lock -> IO ())
  -- ^ Create a pin for a locked entity
  , lockPinId :: !(Lock -> PinId)
  -- ^ Map a lock to its pin ID (deterministic, for deduplication)
  , watcherRemovedRef :: !(IORef (Set PinId))
  -- ^ Pins the watcher is about to remove (lock gone). These should NOT
  -- trigger a Release command in the onPinClosed callback.
  }

-- | Start watching document locks and maintaining editor pins accordingly.
--
-- When a lock is held by the current user+session, an editor pin
-- is created. When the lock disappears (released, stolen), the pin is removed.
-- On pin close (via close button), the lock is released.
--
-- Returns an unsubscribe action.
startLockWatching :: LockWatchConfig -> WindowEventSink -> IO (IO ())
startLockWatching cfg sink = do
  prevRef <- newIORef Set.empty

  -- Subscribe to document changes
  cfg.subscribeDocChanges (onDocumentChange cfg sink prevRef)

-- | Handle a document change: diff locks against previous state.
onDocumentChange :: LockWatchConfig -> WindowEventSink -> IORef (Set Lock) -> Document -> IO ()
onDocumentChange cfg sink prevRef doc = do
  let current = myLocks cfg doc
  prev <- readIORef prevRef
  writeIORef prevRef current
  let added = current `Set.difference` prev
      removed = prev `Set.difference` current
  mapM_ (cfg.ensurePin sink doc) (Set.toList added)
  -- Mark as watcher-initiated before unpinning, so onPinClosed skips the Release
  forM_ (Set.toList removed) $ \lock -> do
    let pid = cfg.lockPinId lock
    modifyIORef' cfg.watcherRemovedRef (Set.insert pid)
    unpinDialog sink pid

-- | Get all locks held by the current user+session.
myLocks :: LockWatchConfig -> Document -> Set Lock
myLocks cfg doc =
  Set.fromList
    [ lock
    | (lock, holder) <- Map.toList doc.locks
    , holder.userId == cfg.userId
    , holder.sessionId == cfg.sessionId
    ]

