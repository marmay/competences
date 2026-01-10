-- | Common patterns for selector/list views.
-- Provides reusable components for consistent selector UI across the application.
module Competences.Frontend.View.SelectorList
  ( -- * Header
    selectorHeader
  , selectorHeaderWithDropdown
  , dropdownItem
    -- * List Items
  , selectorItem
  , selectorItemWithBadge
  , selectorItemMultiLine
    -- * Search
  , selectorSearchField
    -- * Container
  , selectorList
  )
where

import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)
import Optics.Core ((&), (.~))

-- | Header with title and add button (icon-only)
selectorHeader
  :: MisoString
  -- ^ Title text
  -> Maybe action
  -- ^ Optional add action (Nothing = hide button)
  -> M.View m action
selectorHeader title mAddAction =
  M.div_
    [class_ "flex items-center justify-between"]
    [ Typography.h3 title
    , case mAddAction of
        Just action ->
          M.button_
            [ class_ "btn btn-secondary h-9 px-3"
            , M.onClick action
            ]
            [icon [class_ "w-5 h-5"] IcnAdd]
        Nothing -> M.text ""
    ]

-- | Header with title and dropdown menu for multiple add options
selectorHeaderWithDropdown
  :: MisoString
  -- ^ Title text
  -> Bool
  -- ^ Is dropdown open?
  -> action
  -- ^ Toggle dropdown action
  -> [M.View m action]
  -- ^ Dropdown menu items
  -> M.View m action
selectorHeaderWithDropdown title isOpen toggleAction menuItems =
  M.div_
    [class_ "flex items-center justify-between"]
    [ Typography.h3 title
    , M.div_
        [class_ "relative"]
        [ M.button_
            [ class_ "btn btn-secondary h-9 px-3"
            , M.onClick toggleAction
            ]
            [icon [class_ "w-5 h-5"] IcnAdd]
        , if isOpen
            then
              M.div_
                [class_ "absolute right-0 top-full mt-1 z-50 min-w-48 rounded-md border bg-popover p-1 shadow-md"]
                menuItems
            else M.text ""
        ]
    ]

-- | Dropdown menu item
dropdownItem :: Icon -> MisoString -> action -> M.View m action
dropdownItem icn label action =
  M.button_
    [ class_ "flex w-full items-center gap-2 rounded-sm px-2 py-1.5 text-sm hover:bg-accent hover:text-accent-foreground cursor-pointer overflow-hidden"
    , M.onClick action
    ]
    [ icon [class_ "w-4 h-4 shrink-0"] icn
    , M.span_ [class_ "truncate"] [M.text label]
    ]

-- | Single-line selectable list item with icon
selectorItem
  :: Bool
  -- ^ Is selected?
  -> Icon
  -- ^ Item icon
  -> MisoString
  -- ^ Label text
  -> action
  -- ^ Click action
  -> M.View m action
selectorItem isSelected icn label action =
  M.div_
    [ class_ $
        "flex items-center gap-2 px-3 py-2 rounded cursor-pointer transition-colors "
          <> if isSelected then "bg-primary/10 text-primary" else "hover:bg-muted"
    , M.onClick action
    ]
    [ icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] icn
    , M.span_ [class_ "text-sm truncate"] [M.text label]
    ]

-- | Single-line selectable list item with icon and optional badge on the right
selectorItemWithBadge
  :: Bool
  -- ^ Is selected?
  -> Icon
  -- ^ Item icon
  -> MisoString
  -- ^ Label text
  -> Maybe (M.View m action)
  -- ^ Optional badge view on the right
  -> action
  -- ^ Click action
  -> M.View m action
selectorItemWithBadge isSelected icn label mBadge action =
  M.div_
    [ class_ $
        "flex items-center gap-2 px-3 py-2 rounded cursor-pointer transition-colors "
          <> if isSelected then "bg-primary/10 text-primary" else "hover:bg-muted"
    , M.onClick action
    ]
    [ icon [class_ "w-4 h-4 text-muted-foreground shrink-0"] icn
    , M.span_ [class_ "text-sm truncate flex-1"] [M.text label]
    , case mBadge of
        Just badgeView -> badgeView
        Nothing -> M.text ""
    ]

-- | Multi-line selectable list item (e.g., for Evidence with date + type + users)
selectorItemMultiLine
  :: Bool
  -- ^ Is selected?
  -> [M.View m action]
  -- ^ Content rows
  -> action
  -- ^ Click action
  -> M.View m action
selectorItemMultiLine isSelected content action =
  M.div_
    [ class_ $
        "flex flex-col gap-1 px-3 py-2 rounded cursor-pointer transition-colors "
          <> if isSelected then "bg-primary/10 text-primary" else "hover:bg-muted"
    , M.onClick action
    ]
    content

-- | Search/filter input field
selectorSearchField
  :: MisoString
  -- ^ Current value
  -> MisoString
  -- ^ Placeholder text
  -> (MisoString -> action)
  -- ^ OnInput handler
  -> M.View m action
selectorSearchField value placeholder onInput =
  Input.defaultInput
    & Input.withValue value
    & Input.withPlaceholder placeholder
    & Input.withOnInput onInput
    & Input.renderInput

-- | Scrollable list container (grows to fill available space)
selectorList :: [M.View m action] -> M.View m action
selectorList items =
  V.viewFlow
    (V.vFlow & (#gap .~ V.SmallSpace) & (#extraAttrs .~ [class_ "flex-1", V.overflowYScroll, V.minH0]))
    items
