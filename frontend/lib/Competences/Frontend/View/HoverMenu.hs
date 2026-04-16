-- | Reusable dropdown menu components.
--
-- Stateless variants use CSS hover. Click-based variants use explicit
-- open/close state managed by the caller.
module Competences.Frontend.View.HoverMenu
  ( hoverMenu
  , hoverMenuRight
  , hoverMenuAboveRight
  , clickMenuRight
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

-- ============================================================================
-- Stateless (CSS-only hover)
-- ============================================================================

hoverMenu :: View m a -> [View m a] -> View m a
hoverMenu = hoverMenuWith "left-0"

hoverMenuRight :: View m a -> [View m a] -> View m a
hoverMenuRight = hoverMenuWith "right-0"

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
        [dropdownPanel items]
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
        [dropdownPanel items]
    ]

-- ============================================================================
-- Click-based (explicit open/close)
-- ============================================================================

-- | Click-to-open dropdown aligned to the right.
-- When open, a fixed backdrop catches outside clicks to close.
clickMenuRight :: Bool -> a -> a -> View m a -> [View m a] -> View m a
clickMenuRight isOpen toggleAction closeAction trigger items =
  MH.div_
    [class_ "relative"]
    [ MH.div_ [MH.onClickWithOptions M.stopPropagation toggleAction] [trigger]
    , if isOpen
        then
          MH.div_
            []
            [ MH.div_ [class_ "fixed inset-0 z-40", MH.onClick closeAction] []
            , MH.div_
                [class_ "absolute right-0 top-full pt-1 z-50", MH.onClick closeAction]
                [dropdownPanel items]
            ]
        else M.text ""
    ]

-- ============================================================================
-- Shared
-- ============================================================================

dropdownPanel :: [View m a] -> View m a
dropdownPanel items =
  MH.div_
    [ class_
        "min-w-48 bg-popover text-popover-foreground \
        \border border-border rounded-md shadow-lg p-1 \
        \flex flex-col gap-0.5"
    ]
    items

-- | A single clickable entry in a hover menu (icon + label, full-width).
hoverMenuEntry :: Bool -> Icon.Icon -> MisoString -> a -> View m a
hoverMenuEntry isActive icn label action =
  let activeCls = if isActive then " bg-accent text-accent-foreground" else ""
   in MH.div_
        [ class_ $ "flex items-center gap-2 px-2 py-1.5 rounded-sm cursor-pointer hover:bg-accent hover:text-accent-foreground" <> activeCls
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
