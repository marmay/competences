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

import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Competences.Frontend.View.Typography qualified as Typography
import Miso qualified as M
import Miso.Html qualified as M

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
              withTooltip (PlainTooltip "Minimize") $
                Button.ghostSm (Button.button Icon.IcnExpandShrinkArrowRight (Just toggleAction))
            , -- Close button
              withTooltip (PlainTooltip "Close") $
                Button.ghostSm (Button.button Icon.IcnCancel (Just closeAction))
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
  :: Icon.Icon
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
    [class_ "relative"]
    [ withTooltip (PlainTooltip title) $
        Button.toggleLg isActive (Button.button icn (Just clickAction))
    , -- Badge overlay
      case badgeNumber of
        Nothing -> M.text ""
        Just n ->
          M.span_
            [ class_ "absolute -bottom-0.5 -right-0.5 bg-primary text-primary-foreground text-xs font-bold rounded-full w-4 h-4 flex items-center justify-center"
            ]
            [M.text $ M.ms (show n)]
    ]
