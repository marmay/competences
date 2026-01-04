{- |
Module: Competences.Frontend.View.TagInput
Description: Tag input component for multi-value selection

A component that looks like a text input but displays selected values as
inline badges/tags. Used for multi-stage selectors like competence levels
and observations.
-}
module Competences.Frontend.View.TagInput
  ( -- * Tag Input Component
    TagInputConfig (..)
  , tagInput
  , tagInputDisabled
  )
where

import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M

-- | Configuration for a tag input component
data TagInputConfig m a = TagInputConfig
  { badges :: ![M.View m a]
  -- ^ List of badge views to display
  , inputArea :: !(M.View m a)
  -- ^ The input cursor area (for keyboard capture)
  , popover :: !(Maybe (M.View m a))
  -- ^ Optional popover content (shown when hasFocus is True)
  , hasFocus :: !Bool
  -- ^ Whether the input has focus (for showing popover)
  , onKeyDown :: !(Maybe (M.KeyInfo -> a))
  -- ^ Keyboard event handler
  , onFocus :: !(Maybe a)
  -- ^ Focus handler
  , onBlur :: !(Maybe a)
  -- ^ Blur handler
  }
  deriving (Generic)

-- | Render a tag input component (enabled mode with keyboard input)
--
-- Visual structure:
-- @
-- ┌──────────────────────────────────────────────────────┐
-- │ [Badge 1 ×] [Badge 2 ×] [Badge 3 ×] |1.2▁           │
-- └──────────────────────────────────────────────────────┘
--          ↑ Popover appears above when focused ↑
-- ┌──────────────────────────────────────────────────────┐
-- │ 1: Competence Grid A                                 │
-- │ 2: Competence Grid B                                 │
-- └──────────────────────────────────────────────────────┘
-- @
--
-- Note: Uses CSS focus-within for styling, so focus ring appears automatically
-- when the container or any child receives focus.
--
-- IMPORTANT: The popover is always rendered but hidden/shown with CSS to keep
-- the DOM structure stable and prevent focus loss during re-renders.
tagInput :: TagInputConfig m a -> M.View m a
tagInput config =
  M.div_
    [class_ "relative w-full"]
    [ -- Popover (above the input, shown/hidden with CSS based on hasFocus)
      -- We always render it to keep DOM structure stable and preserve focus
      case config.popover of
        Nothing -> M.text ""
        Just popoverContent ->
          viewPopover config.hasFocus popoverContent
    , -- Main container (looks like an input)
      M.div_
        ( [ class_ containerClasses
          , M.intProp "tabindex" 0
          ]
            <> maybe [] (\h -> [M.onKeyDownWithInfo h]) config.onKeyDown
            <> maybe [] (\h -> [M.onFocus h]) config.onFocus
            <> maybe [] (\h -> [M.onBlur h]) config.onBlur
        )
        ( config.badges
            <> [config.inputArea]
        )
    ]
  where
    -- Uses focus-within for automatic focus ring styling
    -- overflow-visible ensures badge tooltips aren't clipped
    containerClasses =
      "flex flex-wrap gap-1.5 items-center min-h-9 w-full rounded-md border \
      \border-input bg-background px-3 py-1.5 shadow-xs cursor-text \
      \focus-within:ring-2 focus-within:ring-ring focus-within:border-ring \
      \focus:outline-none focus:ring-2 focus:ring-ring focus:border-ring \
      \overflow-visible"

-- | Render a disabled tag input (view-only, no keyboard input)
tagInputDisabled :: [M.View m a] -> M.View m a
tagInputDisabled badges =
  M.div_
    [class_ disabledClasses]
    badges
  where
    disabledClasses =
      "flex flex-wrap gap-1.5 items-center min-h-9 w-full rounded-md border \
      \border-input bg-muted px-3 py-1.5"

-- | Render the popover above the input
-- Uses CSS to show/hide based on visibility flag to keep DOM structure stable
viewPopover :: Bool -> M.View m a -> M.View m a
viewPopover isVisible content =
  M.div_
    [ class_ $
        "absolute bottom-full left-0 right-0 mb-2 bg-popover border border-border \
        \rounded-md shadow-lg p-2 z-50 max-h-32 overflow-y-auto "
          <> if isVisible then "" else "hidden"
    ]
    [content]
