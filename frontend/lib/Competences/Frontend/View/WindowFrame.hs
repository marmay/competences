-- |
-- Module      : Competences.Frontend.View.WindowFrame
-- Description : Shared window frame helpers for modals and pinned dialogs
--
-- Provides 'windowTitleBar' (shared title bar with icon + title + action buttons),
-- 'modalFrame' (modal overlay with backdrop), 'pinFrame' (pinned dialog frame),
-- and 'pinSidebarIcon' (sidebar icon button).
module Competences.Frontend.View.WindowFrame
  ( -- * Shared chrome
    windowTitleBar
  , closeButton
  , minimizeButton
  , pinButton
    -- * Modal frame
  , modalFrame
  , modalDialog
    -- * Pin frame
  , pinFrame
  , pinSidebarIcon
  )
where

import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalHeight (..), ModalWidth (..), WindowChrome (..))
import Data.Text (Text)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Competences.Frontend.View.Typography qualified as Typography
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Event (onClick)

-- | Shared title bar: icon + title on the left, action buttons on the right.
-- Used by both modal and pinned dialog frames.
windowTitleBar :: WindowChrome -> [M.View m a] -> M.View m a
windowTitleBar chrome actions =
  M.div_
    [class_ "border-b border-border px-4 py-3 bg-muted/50 rounded-t-xl flex-shrink-0"]
    [ Layout.hFlow
        (Layout.hFull <> Layout.crossCenter <> Layout.mainBetween)
        [ Layout.hFlow
            (Layout.gapS <> Layout.crossCenter)
            [ chromeIcon chrome
            , Typography.h4 chrome.title
            ]
        , Layout.hFlow
            (Layout.gapS <> Layout.crossCenter)
            actions
        ]
    ]

-- | Close button (X icon) with tooltip.
-- | Render the chrome icon, with optional badge overlay.
chromeIcon :: WindowChrome -> M.View m a
chromeIcon chrome = case chrome.iconBadge of
  Nothing -> Icon.iconV Icon.Ghost chrome.icon
  Just badge -> Icon.iconWithBadge chrome.icon badge

closeButton :: a -> M.View m a
closeButton action =
  withTooltip (PlainTooltip "Close") $
    Button.ghost (Button.button Icon.IcnCancel (Just action))

-- | Minimize button (shrink arrow) with tooltip.
minimizeButton :: a -> M.View m a
minimizeButton action =
  withTooltip (PlainTooltip "Minimize") $
    Button.ghost (Button.button Icon.IcnExpandShrinkArrowRight (Just action))

-- | Pin button (pin icon) with tooltip.
pinButton :: a -> M.View m a
pinButton action =
  withTooltip (PlainTooltip "Pin") $
    Button.ghost (Button.button Icon.IcnPin (Just action))

-- ============================================================================
-- MODAL FRAME
-- ============================================================================

-- | Modal overlay: backdrop + dialog box + title bar + content slot.
-- The close action is wired to both the backdrop click and the close button.
modalFrame :: ModalConfig -> a -> [M.View m a] -> M.View m a
modalFrame cfg closeAction content =
  M.div_
    [class_ "fixed inset-0 z-50 flex items-center justify-center"]
    [ -- Backdrop
      M.div_
        [ class_ "absolute inset-0 bg-foreground/50"
        , onClick closeAction
        ]
        []
    , -- Dialog box with chrome
      M.div_
        [class_ $ "relative z-10 flex flex-col " <> dialogClasses cfg]
        [ -- Title bar (shared format)
          windowTitleBar cfg.chrome
            [closeButton closeAction]
        , -- Content
          M.div_
            [class_ $ contentClasses cfg]
            content
        ]
    ]

-- | Modal dialog box without backdrop. Used by WindowHost for stacked rendering.
-- The positioning container uses @pointer-events-none@ so clicks pass through
-- to the backdrop, with @pointer-events-auto@ on the actual dialog box.
modalDialog :: ModalConfig -> a -> [M.View m a] -> M.View m a
modalDialog cfg closeAction content =
  M.div_
    [class_ "fixed inset-0 z-50 flex items-center justify-center pointer-events-none"]
    [ M.div_
        [class_ $ "relative pointer-events-auto flex flex-col " <> dialogClasses cfg]
        [ windowTitleBar cfg.chrome [closeButton closeAction]
        , M.div_ [class_ $ contentClasses cfg] content
        ]
    ]

-- | CSS classes for the outer dialog box (appearance + sizing).
dialogClasses :: ModalConfig -> Text
dialogClasses cfg =
  "bg-popover text-popover-foreground rounded-xl shadow-lg "
    <> widthClass cfg.width
    <> " "
    <> heightClass cfg.height

-- | CSS classes for the scrollable content area.
contentClasses :: ModalConfig -> Text
contentClasses cfg = case cfg.height of
  ModalFull -> "flex-1 min-h-0 overflow-y-auto"
  ModalAuto -> "overflow-y-auto"

widthClass :: ModalWidth -> Text
widthClass ModalWide = "w-[85vw] max-w-[1200px]"
widthClass ModalNarrow = "max-w-lg w-full mx-4"

heightClass :: ModalHeight -> Text
heightClass ModalFull = "h-[90vh]"
heightClass ModalAuto = "max-h-[90vh]"

-- ============================================================================
-- PIN FRAME
-- ============================================================================

-- | Pin dialog frame: shared title bar with minimize + close buttons.
-- Content is rendered below the title bar.
pinFrame :: WindowChrome -> a -> a -> [M.View m a] -> M.View m a
pinFrame chrome toggleAction closeAction content =
  M.div_
    [class_ "h-full"]
    [ Layout.vFlow Layout.hFull
        [ windowTitleBar chrome
            [ minimizeButton toggleAction
            , closeButton closeAction
            ]
        , M.div_
            [class_ "flex-1 min-h-0 overflow-auto p-4"]
            content
        ]
    ]

-- | Sidebar icon button for a pinned dialog.
--
-- Renders a ~48x48 icon button with the chrome's icon and optional
-- badge icon overlay (e.g. edit pen to mark an editor pin) plus an
-- optional context text badge underneath (e.g. assignment name).
-- Active (currently visible) dialogs are highlighted.
pinSidebarIcon
  :: WindowChrome
  -> Bool
  -- ^ Whether this pin is currently visible (active)
  -> Maybe M.MisoString
  -- ^ Optional context badge (e.g. assignment name)
  -> a
  -- ^ Click action
  -> M.View m a
pinSidebarIcon chrome isActive badgeText clickAction =
  M.div_
    [class_ "relative"]
    ( [ withTooltip (PlainTooltip chrome.title) $
          Button.toggleLg isActive (Button.button chrome.icon (Just clickAction))
      ]
        <> [ M.span_
              [ class_ "absolute -top-1 -right-1 inline-flex items-center justify-center rounded-full bg-primary text-primary-foreground ring-2 ring-background p-0.5 pointer-events-none"
              ]
              [Icon.iconS Icon.Small b]
           | Just b <- [chrome.iconBadge]
           ]
        <> [ M.span_
              [ class_ "absolute -bottom-1 left-1/2 -translate-x-1/2 bg-primary text-primary-foreground text-[10px] font-bold rounded-full max-w-12 truncate px-1 flex items-center justify-center"
              ]
              [M.text t]
           | Just t <- [badgeText]
           ]
    )
