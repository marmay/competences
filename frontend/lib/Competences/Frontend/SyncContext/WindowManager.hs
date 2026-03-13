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
  , WindowManagerRef (..)
  , AnyModal (..)
  , AnyPinnedDialog (..)
  , PinId -- no (..)
  , PinCategory (..)
  , PinMeta (..)
  , SortAtom (..)
  , SortKey (..)
  , PinVisibility (..)
  , Model (..)
  , WindowChange (..)

    -- * Construction
  , newWindowManager

    -- * Modal API (blocking dialogs)
  , openModal
  , closeModal

    -- * Pin API (persistent dialogs)
  , unpinDialog
  , minimizeDialog
  , restoreDialog
  , toggleDialog

    -- * Subscription
  , subscribeWindows

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

import Competences.Frontend.View.Icon qualified as Icon
import Control.Monad (forM_, when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)
import Miso.Subscription.Util (createSub)
import UnliftIO (MVar, modifyMVar, modifyMVar_, newMVar)

-- ---------------------------------------------------------------------------
-- Shared chrome types
-- ---------------------------------------------------------------------------

-- | Shared window chrome for both modals and pinned dialogs.
-- Defines the icon and title displayed in the title bar.
data WindowChrome = WindowChrome
  { title :: !MisoString
  , icon :: !Icon.Icon
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
  = PinCatAssignment
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

-- | Rendering context for a component. Constructor not exported — only
-- the mounting helpers ('inlineComponentWith', 'openFramedModalWith',
-- 'pinDialogWith') can create values, so the context is always correct.
data WindowMode = WindowMode
  { _contextTag :: !ContextTag
  , _closeAction :: !(IO ())
  }

-- | Internal tag — NOT exported.
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
data AnyPinnedDialog where
  AnyPinnedDialog
    :: (Eq m, Typeable m)
    => !(M.Component Model m a)
    -> !WindowChrome
    -> !PinMeta
    -> AnyPinnedDialog

instance Eq AnyPinnedDialog where
  AnyPinnedDialog c1 ch1 m1 == AnyPinnedDialog c2 ch2 m2 =
    ch1 == ch2 && m1 == m2 && case cast c2.model of
      Just m2' -> c1.model == m2'
      Nothing -> False

-- | Host model used as the parent type for all managed components.
-- Defined here to avoid circular imports with WindowHost.
data Model = Model
  { modalStack :: ![AnyModal]
  , pinnedDialogs :: !(Map.Map PinId (AnyPinnedDialog, PinVisibility))
  , pinOrder :: ![PinId]
  }
  deriving (Eq, Generic)

-- | Change notification sent to subscribers.
data WindowChange = WindowChange
  { modalStack :: ![AnyModal]
  , pinnedDialogs :: !(Map.Map PinId (AnyPinnedDialog, PinVisibility))
  , pinOrder :: ![PinId]
  , isInitial :: !Bool
  }

-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

data WindowState = WindowState
  { modals :: ![AnyModal]
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
        { modals = []
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
-- Pushes onto the modal stack; the new modal appears on top.
openModal :: WindowManagerRef -> AnyModal -> IO ()
openModal (WindowManagerRef ref) modal = do
  modifyMVar_ ref $ \s -> do
    let s' = s {modals = modal : s.modals}
    notifyHandlers s'
    pure s'

-- | Open a framed modal for a component that ignores 'WindowMode'.
-- Convenience wrapper: @openFramedModal ref cfg c = openFramedModalWith ref cfg (const c)@
openFramedModal
  :: (Eq m, Typeable m)
  => WindowManagerRef
  -> ModalConfig
  -> M.Component Model m a
  -> IO ()
openFramedModal ref cfg comp =
  openFramedModalWith ref cfg (const comp)

-- | Close the topmost modal. If multiple modals are stacked, reveals the next one.
closeModal :: WindowManagerRef -> IO ()
closeModal (WindowManagerRef ref) = do
  modifyMVar_ ref $ \s -> do
    let s' = s {modals = drop 1 s.modals}
    notifyHandlers s'
    pure s'

-- ---------------------------------------------------------------------------
-- Pin API
-- ---------------------------------------------------------------------------

-- | Pin a dialog (internal). If the 'PinId' already exists, the existing
-- dialog is made visible (restored). Otherwise it is added and made visible.
-- In both cases, any previously visible pin is minimized.
pinDialogRaw :: WindowManagerRef -> PinMeta -> AnyPinnedDialog -> IO ()
pinDialogRaw (WindowManagerRef ref) meta dialog = do
  let pid = toPinId meta
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
    { modalStack = s.modals
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
  => WindowManagerRef
  -> ModalConfig
  -> (WindowMode -> M.Component Model m a)
  -> IO ()
openFramedModalWith ref cfg mkComp =
  let mode = mkModalMode (closeModal ref)
   in openModal ref (AnyModal (mkComp mode) cfg)

-- | Pin a dialog, injecting pinned 'WindowMode' into the component.
pinDialogWith
  :: (Eq m, Typeable m)
  => WindowManagerRef
  -> PinMeta
  -> WindowChrome
  -> (WindowMode -> M.Component Model m a)
  -> IO ()
pinDialogWith ref meta chrome mkComp =
  let pid = toPinId meta
      mode = mkPinnedMode (unpinDialog ref pid)
   in pinDialogRaw ref meta (AnyPinnedDialog (mkComp mode) chrome meta)

-- | Pin a dialog for a component that ignores 'WindowMode'.
-- Convenience wrapper: @pinDialog ref meta ch c = pinDialogWith ref meta ch (const c)@
pinDialog
  :: (Eq m, Typeable m)
  => WindowManagerRef
  -> PinMeta
  -> WindowChrome
  -> M.Component Model m a
  -> IO ()
pinDialog ref meta chrome comp =
  pinDialogWith ref meta chrome (const comp)
