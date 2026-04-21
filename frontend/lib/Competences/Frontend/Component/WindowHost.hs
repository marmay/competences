{-# LANGUAGE CPP #-}

-- |
-- Module      : Competences.Frontend.Component.WindowHost
-- Description : Unified window host for modals and pinned dialogs
--
-- Owns all window state directly. The WindowEventSink dispatches events
-- to this component's action handler. Mount once in App.hs.
--
-- Rendering order (z-index):
--
-- 1. Pinned dialogs (z-30) -- floating overlay with frame
-- 2. Sidebar (z-40) -- icon strip on the right edge
-- 3. Modal (z-50) -- backdrop + centered content, always on top
module Competences.Frontend.Component.WindowHost
  ( windowHostComponent
  )
where

import Competences.Frontend.SyncContext.WindowManager
  ( AnyModal (..)
  , AnyPinnedDialog (..)
  , ModalConfig (..)
  , ModalId (..)
  , Model (..)
  , PinId
  , PinMeta (..)
  , PinVisibility (..)
  , WindowEvent (..)
  , WindowEventSinkInstaller
  , installWindowEventSink
  , mkPinId
  )
import Control.Monad (unless, when)
import Data.Dynamic (fromDynamic)
import Data.IORef (IORef, readIORef)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.WindowFrame (modalDialog, pinFrame, pinSidebarIcon)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Event (onClick)

-- | Actions for the WindowHost component.
data Action
  = WinEvent !WindowEvent
  | BackdropClicked
  | TogglePin !PinId
  | ClosePin !PinId

-- | The WindowHost component owns all window state directly.
-- The installer is invoked on mount to register the real handler with the
-- 'WindowEventSink'; any events emitted before mount are flushed in order.
windowHostComponent :: WindowEventSinkInstaller -> IORef (PinId -> IO ()) -> M.Component p Model Action
windowHostComponent installer onPinClosedRef =
  (M.component model update view)
    { M.subs = [fillSinkSub installer]
    }
  where
    model =
      Model
        { modalStack = []
        , pinnedDialogs = Map.empty
        , pinOrder = []
        , pinSaveStates = Map.empty
        , pinSaveGen = 0
        }

    update (WinEvent (WEOpenModal modal)) =
      M.modify $ \m -> m {modalStack = modal : m.modalStack}

    update (WinEvent WECloseTopModal) =
      M.modify $ \m -> m {modalStack = drop 1 m.modalStack}

    update (WinEvent (WEPinDialog dialog@(AnyPinnedDialog _ _ meta))) = do
      M.modify $ \m ->
        let pid = mkPinIdFromMeta meta
         in addPin pid dialog m
      when meta.isEditor $ M.io_ $ setBeforeUnloadGuard True

    update (WinEvent (WEUnpinDialog pid)) = do
      M.modify $ \m -> removePin pid m
      M.io_ $ do
        callback <- readIORef onPinClosedRef
        callback pid
      m <- M.get
      unless (anyEditorPinned m) $ M.io_ $ setBeforeUnloadGuard False

    update (WinEvent (WETogglePin pid)) =
      M.modify $ \m ->
        case Map.lookup pid m.pinnedDialogs of
          Just (_, PinVisible) -> setVisibility pid PinMinimized m
          Just (_, PinMinimized) -> makeVisible pid m
          Nothing -> m

    update (WinEvent (WEMinimizePin pid)) =
      M.modify $ \m -> setVisibility pid PinMinimized m

    update (WinEvent (WERestorePin pid)) =
      M.modify $ makeVisible pid

    update BackdropClicked =
      -- Close the top modal
      M.modify $ \m -> m {modalStack = drop 1 m.modalStack}

    update (TogglePin pid) =
      update (WinEvent (WETogglePin pid))

    update (ClosePin pid) =
      update (WinEvent (WEUnpinDialog pid))

    view m =
      MH.div_
        []
        [ renderPinBackdrop m
        , renderPinnedDialogs m
        , renderSidebar m
        , renderModal m
        ]

-- | Extract PinId from PinMeta.
mkPinIdFromMeta :: PinMeta -> PinId
mkPinIdFromMeta meta = mkPinId meta.key

-- | Does the model currently hold at least one pin marked as an editor?
anyEditorPinned :: Model -> Bool
anyEditorPinned m =
  any (\(AnyPinnedDialog _ _ meta, _vis) -> meta.isEditor)
      (Map.elems m.pinnedDialogs)

-- | Install the real action sink on component mount, flushing any events
-- that were emitted while the sink was buffering.
fillSinkSub :: WindowEventSinkInstaller -> M.Sub Action
fillSinkSub installer actionSink =
  installWindowEventSink installer (\ev -> actionSink (WinEvent ev))

-- | Set or clear 'window.onbeforeunload' so the browser prompts before
-- navigating away while pin editors are open. Called only by this
-- component, on pin open / last-pin close. Modern browsers ignore the
-- returned message and show their own generic prompt; a non-empty
-- string is enough to trigger it. Client-side (SPA) navigation via
-- 'M.pushURI' does not fire beforeunload, so internal links keep
-- working silently.
setBeforeUnloadGuard :: Bool -> IO ()
#ifdef WASM
setBeforeUnloadGuard True = js_enableBeforeUnloadGuard
setBeforeUnloadGuard False = js_disableBeforeUnloadGuard

foreign import javascript unsafe
  "window.onbeforeunload = function(e){return ''}"
  js_enableBeforeUnloadGuard :: IO ()

foreign import javascript unsafe
  "window.onbeforeunload = null"
  js_disableBeforeUnloadGuard :: IO ()
#else
setBeforeUnloadGuard _ = pure ()  -- JSaddle dev: no-op; real behaviour lives in WASM
#endif

-- | Add a pin to the model. If the PinId already exists, restore it.
-- Otherwise add it and make it visible.
addPin :: PinId -> AnyPinnedDialog -> Model -> Model
addPin pid dialog m =
  if Map.member pid m.pinnedDialogs
    then makeVisible pid m
    else
      let withNewPin = m
            { pinnedDialogs = Map.insert pid (dialog, PinMinimized) m.pinnedDialogs
            , pinOrder = m.pinOrder ++ [pid]
            }
       in makeVisible pid withNewPin

-- | Remove a pin from the model. Clears save state.
removePin :: PinId -> Model -> Model
removePin pid m = m
  { pinnedDialogs = Map.delete pid m.pinnedDialogs
  , pinOrder = filter (/= pid) m.pinOrder
  , pinSaveStates = Map.delete pid m.pinSaveStates
  }

-- | Make a pin visible, minimizing all others.
makeVisible :: PinId -> Model -> Model
makeVisible pid m = m
  { pinnedDialogs = Map.adjust (\(d, _) -> (d, PinVisible)) pid $
      Map.map (\(d, _) -> (d, PinMinimized)) m.pinnedDialogs
  }

-- | Set the visibility of a specific pin.
setVisibility :: PinId -> PinVisibility -> Model -> Model
setVisibility pid vis m = m
  { pinnedDialogs = Map.adjust (\(d, _) -> (d, vis)) pid m.pinnedDialogs
  }

-- | Render all pinned dialogs. Visible ones get the overlay frame styling.
-- Minimized ones are kept in the DOM with @class "hidden"@ to preserve state.
-- Sorted by (category, sortKey) for deterministic ordering.
renderPinnedDialogs :: Model -> M.View Model Action
renderPinnedDialogs m =
  MH.div_
    []
    (map renderOnePin (sortedPins m))
  where
    renderOnePin (pid, AnyPinnedDialog factory chrome meta, visibility) =
      case visibility of
        PinVisible ->
          let typedState = Map.lookup pid m.pinSaveStates >>= fromDynamic
              comp = factory typedState
           in MH.div_
                [ class_
                    "fixed inset-y-[2%] left-[1%] right-[calc(1%+4rem)] z-30 bg-popover text-popover-foreground border border-border rounded-xl shadow-lg flex flex-col"
                ]
                [ pinFrame
                    chrome
                    (TogglePin pid)
                    (ClosePin pid)
                    [MH.div_ [class_ "h-full"]
                      [M.mount_ [M.key_ (M.ms ("pin-" <> M.ms meta.key))] comp]]
                ]
        PinMinimized ->
          -- Full unmount: component is removed from DOM to save memory.
          -- State is preserved via bindings to pinSaveStates.
          M.text ""

-- | Render the sidebar icon strip on the right edge.
-- Only shown when there are pinned dialogs.
-- Sorted by (category, sortKey) for deterministic ordering.
renderSidebar :: Model -> M.View Model Action
renderSidebar m
  | null m.pinOrder = M.text ""
  | otherwise =
      MH.div_
        [ class_
            "relative z-40 w-16 h-screen flex-shrink-0 flex flex-col items-center gap-2 py-16 bg-muted/80 border-l border-border"
        ]
        (map renderSidebarEntry (sortedPins m))
  where
    renderSidebarEntry (pid, AnyPinnedDialog _ chrome meta, visibility) =
      let isActive = visibility == PinVisible
       in pinSidebarIcon chrome isActive meta.context (TogglePin pid)

-- | Extract and sort pinned dialogs by (category, sortKey).
sortedPins :: Model -> [(PinId, AnyPinnedDialog, PinVisibility)]
sortedPins m =
  sortOn (\(_pid, AnyPinnedDialog _ _ meta, _vis) -> (meta.category, meta.sortKey)) $
    [ (pid, dialog, visibility)
    | pid <- m.pinOrder
    , Just (dialog, visibility) <- [Map.lookup pid m.pinnedDialogs]
    ]

-- | Render a semi-transparent backdrop when any pinned dialog is visible.
renderPinBackdrop :: Model -> M.View Model Action
renderPinBackdrop m =
  let hasVisiblePin =
        any
          ( \pid -> case Map.lookup pid m.pinnedDialogs of
              Just (_, PinVisible) -> True
              _ -> False
          )
          m.pinOrder
   in if hasVisiblePin
        then MH.div_ [class_ "fixed inset-0 z-20 bg-foreground/30"] []
        else M.text ""

-- | Render the modal stack: single shared backdrop + all stacked modals.
-- Only the topmost modal is visible; others are hidden to preserve component state.
renderModal :: Model -> M.View Model Action
renderModal m = case m.modalStack of
  [] -> M.text ""
  modals ->
    MH.div_
      []
      ( MH.div_
          [class_ "fixed inset-0 z-50 bg-foreground/50", onClick BackdropClicked]
          []
          : zipWith renderOneModal (True : repeat False) modals
      )
  where
    renderOneModal isTop (AnyModal comp cfg) =
      let ModalId mid = cfg.modalId
          key = "modal-" <> M.ms mid
       in if isTop
            then modalDialog cfg BackdropClicked [key M.+> comp]
            else MH.div_ [class_ "hidden"] [key M.+> comp]
