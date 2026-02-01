-- |
-- Module      : Competences.Frontend.View.PinFrame
-- Description : Styling helpers for pinned dialog frames and sidebar icons
--
-- Provides the visual frame for pinned dialogs (title bar with pin-toggle
-- and close buttons) and sidebar icon buttons.
module Competences.Frontend.View.PinFrame
  ( pinFrame
  , pinSidebarIcon
  )
where

import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP

-- | Pin dialog frame: title bar with title, pin-toggle (minimize) button,
-- and close button. Content is rendered below the title bar.
pinFrame :: M.MisoString -> a -> a -> [M.View m a] -> M.View m a
pinFrame title toggleAction closeAction content =
  M.div_
    [class_ "flex flex-col h-full"]
    [ -- Title bar
      M.div_
        [class_ "flex items-center justify-between border-b border-border px-4 py-3 bg-muted/50 rounded-t-xl flex-shrink-0"]
        [ Typography.h4 title
        , M.div_
            [class_ "flex items-center gap-2"]
            [ -- Pin toggle (minimize) button
              M.button_
                [ class_ "text-muted-foreground hover:text-foreground transition-colors p-1 rounded hover:bg-muted"
                , M.onClick toggleAction
                , MP.title_ "Minimize"
                ]
                [icon [MP.width_ "18", MP.height_ "18"] IcnExpandShrinkArrowRight]
            , -- Close button
              M.button_
                [ class_ "text-muted-foreground hover:text-destructive transition-colors p-1 rounded hover:bg-muted"
                , M.onClick closeAction
                , MP.title_ "Close"
                ]
                [icon [MP.width_ "18", MP.height_ "18"] IcnCancel]
            ]
        ]
    , -- Content area
      M.div_
        [class_ "flex-1 min-h-0 overflow-auto p-4"]
        content
    ]

-- | Sidebar icon button for a pinned dialog.
--
-- Renders a ~48x48 icon button with optional numeric badge overlay.
-- Active (currently visible) dialogs are highlighted.
pinSidebarIcon
  :: Icon
  -- ^ The icon to display
  -> M.MisoString
  -- ^ Hover text (tooltip)
  -> Bool
  -- ^ Whether this pin is currently visible (active)
  -> Maybe Int
  -- ^ Optional numeric badge (for disambiguating duplicate icons)
  -> a
  -- ^ Click action
  -> M.View m a
pinSidebarIcon icn title isActive badgeNumber clickAction =
  M.div_
    [class_ "relative", MP.title_ title]
    [ M.button_
        [ class_ $
            "w-12 h-12 flex items-center justify-center rounded-lg transition-colors "
              <> if isActive
                then "bg-accent text-accent-foreground ring-2 ring-primary"
                else "text-muted-foreground hover:bg-muted hover:text-foreground"
        , M.onClick clickAction
        ]
        [icon [MP.width_ "24", MP.height_ "24"] icn]
    , -- Badge overlay
      case badgeNumber of
        Nothing -> M.text ""
        Just n ->
          M.span_
            [ class_ "absolute -bottom-0.5 -right-0.5 bg-primary text-primary-foreground text-xs font-bold rounded-full w-4 h-4 flex items-center justify-center"
            ]
            [M.text $ M.ms (show n)]
    ]
