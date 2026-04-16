-- | Reusable CSS-only hover dropdown menu component.
--
-- Uses the @group-hover@ pattern (same as Tooltip.hs) to show a dropdown
-- panel when the user hovers over a trigger element.
module Competences.Frontend.View.HoverMenu
  ( hoverMenu
  , hoverMenuRight
  , hoverMenuAboveRight
  , hoverMenuEntry
  , hoverMenuSeparator
  , hoverMenuHeading
  )
where

import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
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
hoverMenu = hoverMenuWith "left-0"

-- | Like 'hoverMenu' but the dropdown aligns to the right edge of the trigger.
hoverMenuRight :: View m a -> [View m a] -> View m a
hoverMenuRight = hoverMenuWith "right-0"

-- | Like 'hoverMenuRight' but the dropdown opens above the trigger.
hoverMenuAboveRight :: View m a -> [View m a] -> View m a
hoverMenuAboveRight trigger items =
  MH.div_
    [class_ "group/menu relative"]
    [ trigger
    , MH.div_
        [ class_
            "absolute right-0 bottom-full pb-1 z-50 \
            \hidden group-hover/menu:block"
        ]
        [ MH.div_
            [ class_
                "min-w-48 bg-popover text-popover-foreground \
                \border border-border rounded-md shadow-lg p-1 \
                \flex flex-col gap-0.5"
            ]
            items
        ]
    ]

hoverMenuWith :: Text -> View m a -> [View m a] -> View m a
hoverMenuWith align trigger items =
  MH.div_
    [class_ "group/menu relative"]
    [ trigger
    , MH.div_
        [ class_ $
            "absolute " <> align <> " top-full pt-1 z-50 \
            \hidden group-hover/menu:block"
        ]
        [ MH.div_
            [ class_
                "min-w-48 bg-popover text-popover-foreground \
                \border border-border rounded-md shadow-lg p-1 \
                \flex flex-col gap-0.5"
            ]
            items
        ]
    ]

-- | A single clickable entry in a hover menu (icon + label, full-width).
hoverMenuEntry :: Bool -> Icon.Icon -> MisoString -> a -> View m a
hoverMenuEntry isActive icn label action =
  let activeCls = if isActive then " bg-accent text-accent-foreground" else ""
   in MH.div_
        [ class_ $ "flex items-center gap-2 px-2 py-1.5 text-sm rounded-sm cursor-pointer hover:bg-accent hover:text-accent-foreground" <> activeCls
        , MH.onClick action
        ]
        [ Icon.iconS Icon.Small icn
        , M.text label
        ]

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
