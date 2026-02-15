-- | Reusable CSS-only hover dropdown menu component.
--
-- Uses the @group-hover@ pattern (same as Tooltip.hs) to show a dropdown
-- panel when the user hovers over a trigger element.
module Competences.Frontend.View.HoverMenu
  ( hoverMenu
  , hoverMenuEntry
  , hoverMenuSeparator
  , hoverMenuHeading
  )
where

import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Miso (View)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)

-- | Wrap a trigger element with a hover dropdown.
--
-- The trigger is rendered normally. On hover, the dropdown items appear
-- below. Uses @pt-1@ padding (not margin) so the hover zone is contiguous
-- between trigger and dropdown panel.
hoverMenu :: View m a -> [View m a] -> View m a
hoverMenu trigger items =
  MH.div_
    [class_ "group/menu relative"]
    [ trigger
    , MH.div_
        [ class_
            "absolute left-0 top-full pt-1 z-50 \
            \hidden group-hover/menu:block"
        ]
        [ MH.div_
            [ class_
                "min-w-48 bg-popover text-popover-foreground \
                \border border-border rounded-md shadow-lg py-1"
            ]
            items
        ]
    ]

-- | A single clickable entry in a hover menu (icon + label).
hoverMenuEntry :: Bool -> Icon.Icon -> MisoString -> a -> View m a
hoverMenuEntry isActive icn label action =
  Button.toggleGhostSm isActive (Button.ButtonConfig (Button.IconText icn label) (Just action))

-- | Small uppercase section heading within a menu.
hoverMenuHeading :: MisoString -> View m a
hoverMenuHeading label =
  MH.div_
    [class_ "px-3 py-1 text-xs font-semibold uppercase text-muted-foreground tracking-wider"]
    [M.text label]

-- | Separator line between groups.
hoverMenuSeparator :: View m a
hoverMenuSeparator =
  MH.div_ [class_ "my-1 border-t border-border"] []
