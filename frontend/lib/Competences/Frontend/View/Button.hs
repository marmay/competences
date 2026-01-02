{-# LANGUAGE OverloadedStrings #-}

module Competences.Frontend.View.Button
  ( -- * Basecoat-style buttons (new approach)
    ButtonVariant (..)
  , ButtonSize (..)
  , BasecoatButton
  , button
  , buttonPrimary
  , buttonSecondary
  , buttonDestructive
  , buttonGhost
  , buttonLink
  , buttonOutline
  , buttonIcon
  , withSize
  , withDisabled
  , withFullWidth
  , withIcon
  , withIconRight
  , withClick
  , withStopPropagation
  , renderButton

    -- * Convenience button constructors
  , applyButton'
  , cancelButton'
  , deleteButton'
  , editButton'
  , moveButton'
  , toggleButton

    -- * Button groups
  , buttonGroup
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)
import Optics.Core ((&))

-- ============================================================================
-- BASECOAT-STYLE BUTTONS (New Approach)
-- ============================================================================

-- | Button variant following Basecoat design system
data ButtonVariant
  = Primary      -- ^ Primary action (sky-600 background)
  | Secondary    -- ^ Secondary action (stone-200 background)
  | Destructive  -- ^ Destructive action (red-600 background)
  | Ghost        -- ^ Ghost button (transparent, hover background)
  | Link         -- ^ Link-style button (underline on hover)
  | Outline      -- ^ Outline button (border, transparent bg) - for toggle unselected state
  deriving (Eq, Show)

-- | Button size
data ButtonSize
  = Small   -- ^ h-9, px-3, text-sm
  | Medium  -- ^ h-10, px-4
  | Large   -- ^ h-11, px-8
  deriving (Eq, Show)

-- | Basecoat-style button configuration
data BasecoatButton model action = BasecoatButton
  { variant :: !ButtonVariant
  , size :: !ButtonSize
  , disabled :: !Bool
  , fullWidth :: !Bool
  , iconLeft :: !(Maybe Icon)
  , iconRight :: !(Maybe Icon)
  , attrs :: ![M.Attribute action]
  , onClick :: !(Maybe action)
  , children :: ![M.View model action]
  }
  deriving (Generic)

-- | Create a button with text content
button :: ButtonVariant -> MisoString -> BasecoatButton model action
button v text = BasecoatButton
  { variant = v
  , size = Medium
  , disabled = False
  , fullWidth = False
  , iconLeft = Nothing
  , iconRight = Nothing
  , attrs = []
  , onClick = Nothing
  , children = [M.text text]
  }

-- | Convenient button constructors
buttonPrimary, buttonSecondary, buttonDestructive, buttonGhost, buttonLink, buttonOutline
  :: MisoString -> BasecoatButton model action
buttonPrimary = button Primary
buttonSecondary = button Secondary
buttonDestructive = button Destructive
buttonGhost = button Ghost
buttonLink = button Link
buttonOutline = button Outline

-- | Set button size
withSize :: ButtonSize -> BasecoatButton m a -> BasecoatButton m a
withSize s (BasecoatButton v _ d f il ir as oc ch) = BasecoatButton v s d f il ir as oc ch

-- | Set disabled state
withDisabled :: Bool -> BasecoatButton m a -> BasecoatButton m a
withDisabled d (BasecoatButton v s _ f il ir as oc ch) = BasecoatButton v s d f il ir as oc ch

-- | Make button full width
withFullWidth :: BasecoatButton m a -> BasecoatButton m a
withFullWidth (BasecoatButton v s d _ il ir as oc ch) = BasecoatButton v s d True il ir as oc ch

-- | Add icon to button
withIcon :: Icon -> BasecoatButton m a -> BasecoatButton m a
withIcon i (BasecoatButton v s d f _ ir as oc ch) = BasecoatButton v s d f (Just i) ir as oc ch

-- | Add click handler
withClick :: a -> BasecoatButton m a -> BasecoatButton m a
withClick action (BasecoatButton v s d f il ir as _ ch) = BasecoatButton v s d f il ir as (Just action) ch

-- | Render a Basecoat-style button
renderButton :: BasecoatButton model action -> M.View model action
renderButton b =
  M.button_
    ( [class_ $ Text.unwords $ filter (not . Text.null)
          [ baseClasses
          , variantClasses b.variant
          , sizeClasses b.size
          , if b.disabled then disabledClasses else ""
          , if b.fullWidth then "w-full" else ""
          ]
      ] <> (if b.disabled then [MP.disabled_] else [])
        <> maybe [] (\a -> [M.onClick a]) b.onClick
        <> b.attrs
    )
    (renderChildren b)
  where
    -- Base classes from Basecoat
    baseClasses = "inline-flex items-center justify-center rounded-md font-medium transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-50"

    -- Variant classes (extracted from Basecoat)
    variantClasses Primary = "bg-sky-600 text-white hover:bg-sky-700 focus-visible:ring-sky-600"
    variantClasses Secondary = "bg-stone-200 text-stone-900 hover:bg-stone-300 focus-visible:ring-stone-400"
    variantClasses Destructive = "bg-red-600 text-white hover:bg-red-700 focus-visible:ring-red-600"
    variantClasses Ghost = "hover:bg-stone-100 focus-visible:ring-stone-400"
    variantClasses Link = "text-sky-600 underline-offset-4 hover:underline"
    variantClasses Outline = "border border-stone-300 bg-transparent text-stone-900 hover:bg-stone-100 focus-visible:ring-stone-400"

    -- Size classes
    sizeClasses Small = "h-9 px-3 gap-1.5 text-sm"
    sizeClasses Medium = "h-10 px-4 gap-2"
    sizeClasses Large = "h-11 px-8 gap-2"

    disabledClasses = "opacity-50 cursor-not-allowed"

    renderChildren btn = case (btn.iconLeft, btn.iconRight) of
      (Nothing, Nothing) -> btn.children
      (Just i, Nothing) -> icon [] i : btn.children
      (Nothing, Just i) -> btn.children <> [icon [] i]
      (Just il, Just ir) -> icon [] il : btn.children <> [icon [] ir]

-- | Create an icon-only button (no text)
buttonIcon :: ButtonVariant -> Icon -> BasecoatButton model action
buttonIcon v i = BasecoatButton
  { variant = v
  , size = Medium
  , disabled = False
  , fullWidth = False
  , iconLeft = Just i
  , iconRight = Nothing
  , attrs = []
  , onClick = Nothing
  , children = []
  }

-- | Add icon to the right side of button
withIconRight :: Icon -> BasecoatButton m a -> BasecoatButton m a
withIconRight i (BasecoatButton v s d f il _ as oc ch) = BasecoatButton v s d f il (Just i) as oc ch

-- | Add stopPropagation to click handler (useful in nested contexts)
withStopPropagation :: BasecoatButton m a -> BasecoatButton m a
withStopPropagation b = b { attrs = b.attrs }  -- TODO: Need to modify onClick handling

-- ============================================================================
-- CONVENIENCE BUTTON CONSTRUCTORS
-- ============================================================================

-- | Apply/confirm button (primary with checkmark icon)
applyButton' :: a -> M.View model a
applyButton' action =
  buttonPrimary (translate' LblApply)
    & withIcon IcnApply
    & withClick action
    & renderButton

-- | Cancel button (destructive with X icon)
cancelButton' :: a -> M.View model a
cancelButton' action =
  buttonDestructive (translate' LblCancel)
    & withIcon IcnCancel
    & withClick action
    & renderButton

-- | Delete button (destructive with trash icon)
deleteButton' :: a -> M.View model a
deleteButton' action =
  buttonDestructive (translate' LblDelete)
    & withIcon IcnDelete
    & withClick action
    & renderButton

-- | Edit button (secondary with edit icon)
editButton' :: a -> M.View model a
editButton' action =
  buttonSecondary (translate' LblEdit)
    & withIcon IcnEdit
    & withClick action
    & renderButton

-- | Move/reorder button (secondary with reorder icon)
moveButton' :: a -> M.View model a
moveButton' action =
  buttonSecondary (translate' LblMove)
    & withIcon IcnReorder
    & withClick action
    & renderButton

-- | Toggle button (selected = Primary filled, unselected = Outline)
-- Use for single-select or multi-select button groups
toggleButton :: Bool -> MisoString -> action -> M.View model action
toggleButton isSelected text action =
  (if isSelected then buttonPrimary else buttonOutline) text
    & withClick action
    & renderButton

-- ============================================================================
-- BUTTON GROUPS
-- ============================================================================

-- | Button group with connected edges (Basecoat pattern)
-- Uses role="group" for accessibility
buttonGroup :: [M.View model action] -> M.View model action
buttonGroup btns =
  M.div_
    [ class_ "button-group"
    , M.textProp "role" "group"
    ]
    btns
