-- |
-- Module      : Competences.Frontend.Component.WindowHost
-- Description : Unified window host for modals and pinned dialogs
--
-- Subscribes to the WindowManager and renders both modals and pinned dialogs.
-- Mount once in App.hs. Replaces the former ModalHost.
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
  , WindowChange (..)
  , WindowChrome (..)
  , WindowManagerRef
  , closeModal
  , subscribeWindows
  , toggleDialog
  , unpinDialog
  )
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.WindowFrame (modalFrame, pinFrame, pinSidebarIcon)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Miso qualified as M
import Miso.Html qualified as MH

-- | Actions for the WindowHost component.
data Action
  = WindowsChanged !WindowChange
  | BackdropClicked
  | TogglePin !PinId
  | ClosePin !PinId

-- | The WindowHost component subscribes to WindowManager and renders
-- all managed windows (modals and pinned dialogs).
windowHostComponent :: WindowManagerRef -> M.Component p Model Action
windowHostComponent ref =
  (M.component model update view)
    { M.subs = [subscribeWindows ref WindowsChanged]
    }
  where
    model =
      Model
        { activeModal = Nothing
        , pinnedDialogs = Map.empty
        , pinOrder = []
        }

    update (WindowsChanged change) =
      M.modify $ \_ ->
        Model
          { activeModal = change.activeModal
          , pinnedDialogs = change.pinnedDialogs
          , pinOrder = change.pinOrder
          }

    update BackdropClicked =
      M.io_ $ closeModal ref

    update (TogglePin pid) =
      M.io_ $ toggleDialog ref pid

    update (ClosePin pid) =
      M.io_ $ unpinDialog ref pid

    view m =
      MH.div_
        []
        [ renderPinBackdrop m
        , renderPinnedDialogs m
        , renderSidebar m
        , renderModal m
        ]

-- | Render all pinned dialogs. Visible ones get the overlay frame styling.
-- Minimized ones are kept in the DOM with @class "hidden"@ to preserve state.
-- Sorted by (category, sortKey) for deterministic ordering.
renderPinnedDialogs :: Model -> M.View Model Action
renderPinnedDialogs m =
  MH.div_
    []
    (map renderOnePin (sortedPins m))
  where
    renderOnePin (pid, AnyPinnedDialog comp chrome meta, visibility) =
      case visibility of
        PinVisible ->
          MH.div_
            [ class_
                "fixed inset-y-[2%] left-[1%] right-[calc(1%+4rem)] z-30 bg-popover text-popover-foreground border border-border rounded-xl shadow-lg flex flex-col"
            ]
            [ pinFrame
                chrome
                (TogglePin pid)
                (ClosePin pid)
                [MH.div_ [class_ "h-full"] [M.ms ("pin-" <> M.ms meta.key) M.+> comp]]
            ]
        PinMinimized ->
          -- Keep in DOM but hidden to preserve component state
          MH.div_
            [class_ "hidden"]
            [M.ms ("pin-" <> M.ms meta.key) M.+> comp]

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
       in pinSidebarIcon chrome.icon chrome.title isActive meta.context (TogglePin pid)

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

-- | Render the modal overlay (backdrop + centered dialog box with title bar).
-- Only shown when a modal is active.
renderModal :: Model -> M.View Model Action
renderModal m = case m.activeModal of
  Nothing -> M.text ""
  Just (AnyModal comp cfg) ->
    let ModalId mid = cfg.modalId
     in modalFrame cfg BackdropClicked [("modal-" <> M.ms mid) M.+> comp]
