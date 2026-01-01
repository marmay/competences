{-# LANGUAGE OverloadedStrings #-}

module Competences.Frontend.View.Button
  ( -- * Basecoat-style buttons (new approach)
    ButtonVariant (..)
  , ButtonSize (..)
  , button
  , buttonPrimary
  , buttonSecondary
  , buttonDestructive
  , buttonGhost
  , buttonLink
  , withSize
  , withDisabled
  , withFullWidth
  , withIcon
  , withClick
  , renderButton

    -- * Legacy button API (backward compatibility)
  , iconButton
  , iconButton'
  , iconLabelButton
  , iconLabelButton'
  , labelButton
  , labelButton'
  , textButton
  , textButton'
  , hButtons
  , vButtons
  , viewButton
  , viewButtons
  , link
  , applyButton
  , cancelButton
  , applyLabelButton
  , deleteButton
  , editButton
  , cancelLabelButton
  , toTriState
  , contentsButton
  , Button (..)
  , Buttons (..)
  , ButtonContents (..)
  , ToggleState (..)
  , TriState (..)
  , ButtonType (..)
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Layout (FlowDirection (..))
import Competences.Frontend.View.Layout qualified as V
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tailwind qualified as T
import Competences.Frontend.View.Text qualified as V
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)
import Optics.Core ((&), (.~))

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
buttonPrimary, buttonSecondary, buttonDestructive, buttonGhost, buttonLink
  :: MisoString -> BasecoatButton model action
buttonPrimary = button Primary
buttonSecondary = button Secondary
buttonDestructive = button Destructive
buttonGhost = button Ghost
buttonLink = button Link

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

-- ============================================================================
-- LEGACY BUTTON API (Backward Compatibility)
-- ============================================================================

data ButtonContents
  = ButtonIcon !Icon !M.MisoString
  | ButtonText !M.MisoString
  | ButtonIconAndText !Icon !M.MisoString
  deriving (Eq, Show)

data ButtonType
  = RegularButton
  | AlertButton

data ButtonRoundedness
  = ButtonRoundedNone
  | ButtonRoundedLeft
  | ButtonRoundedRight
  | ButtonRoundedTop
  | ButtonRoundedBottom
  | ButtonRoundedAll

data ButtonFill
  = HorizontalFill
  | VerticalFill
  | BothFill
  | NoFill

data Button s a = Button
  { contents :: ButtonContents
  , state :: !s
  , buttonType :: !ButtonType
  , roundedness :: !ButtonRoundedness
  , fill :: !ButtonFill
  , minimumWidth :: !(Maybe Int)
  , onClick :: !a
  }
  deriving (Generic)

contentsButton :: ButtonContents -> s -> a -> Button s a
contentsButton c s a =
  Button
    { contents = c
    , state = s
    , buttonType = RegularButton
    , roundedness = ButtonRoundedAll
    , fill = NoFill
    , minimumWidth = Nothing
    , onClick = a
    }

textButton :: M.MisoString -> s -> a -> Button s a
textButton = contentsButton . ButtonText

textButton' :: M.MisoString -> a -> Button () a
textButton' t = textButton t ()

labelButton :: Label -> s -> a -> Button s a
labelButton l = contentsButton (ButtonText $ translate' l)

labelButton' :: Label -> a -> Button () a
labelButton' l = labelButton l ()

iconButton :: Icon -> Label -> s -> a -> Button s a
iconButton i l = contentsButton (ButtonIcon i (translate' l))

iconButton' :: Icon -> Label -> a -> Button () a
iconButton' i l = iconButton i l ()

iconLabelButton :: Icon -> Label -> s -> a -> Button s a
iconLabelButton i l = contentsButton (ButtonIconAndText i (translate' l))

iconLabelButton' :: Icon -> Label -> a -> Button () a
iconLabelButton' i l = iconLabelButton i l ()

viewButton :: (ToTriState s) => Button s a -> M.View m a
viewButton b =
  M.button_
    [ T.tailwind $ mconcat [rounded, colors, fill]
    , M.onClickWithOptions M.stopPropagation b.onClick
    ]
    [ V.viewFlow (V.hFlow & (#margin .~ V.TinySpace) & (#gap .~ V.SmallSpace)) $
        viewButtonContents b.contents
    ]
  where
    rounded = case b.roundedness of
      ButtonRoundedNone -> []
      ButtonRoundedLeft -> [T.RoundedLeft]
      ButtonRoundedRight -> [T.RoundedRight]
      ButtonRoundedTop -> [T.RoundedTop]
      ButtonRoundedBottom -> [T.RoundedBottom]
      ButtonRoundedAll -> [T.Rounded]
    colors = case (b.buttonType, toTriState b.state) of
      (RegularButton, TriStateOff) -> [T.RegularButtonOff]
      (RegularButton, TriStateOn) -> [T.RegularButtonOn]
      (RegularButton, TriStateIndeterminate) -> [T.RegularButtonIndeterminate]
      (AlertButton, TriStateOff) -> [T.AlertButtonOff]
      (AlertButton, TriStateOn) -> [T.AlertButtonOn]
      (AlertButton, TriStateIndeterminate) -> [T.AlertButtonIndeterminate]
    fill = case b.fill of
      NoFill -> []
      HorizontalFill -> [T.HFull]
      VerticalFill -> [T.WFull]
      BothFill -> [T.HFull, T.WFull]
    viewButtonContents (ButtonIcon i t) = [icon [MP.alt_ t] i]
    viewButtonContents (ButtonText t) = [V.buttonText_ t]
    viewButtonContents (ButtonIconAndText i t) =
      [ icon [] i
      , V.viewFlow (V.vFlow & (#expandDirection .~ V.Expand V.Center)) [V.buttonText_ t]
      ]

data Buttons = Buttons
  { direction :: !FlowDirection
  , alignment :: !V.Alignment
  , compact :: !Bool
  }
  deriving (Eq, Generic, Show)

buttons :: FlowDirection -> Buttons
buttons d = Buttons d V.Start False

hButtons, vButtons :: Buttons
hButtons = buttons HorizontalFlow
vButtons = buttons VerticalFlow

viewButtons :: (ToTriState s) => Buttons -> [Button s a] -> M.View m a
viewButtons b bs =
  V.viewFlow
    ( V.flow b.direction
        & (#gap .~ (if b.compact then V.NoSpace else V.SmallSpace))
        & (#expandDirection .~ expansion)
    )
    (viewButtons' bs)
  where
    viewButtons' [] = []
    viewButtons' [btn] = [viewButton (btn & #roundedness .~ ButtonRoundedAll)]
    viewButtons' (btn : btns) = viewButton (btn & buttonRoundedFirst & fill) : viewButtons'' btns
    viewButtons'' [] = []
    viewButtons'' [btn] = [viewButton (btn & buttonRoundedLast & fill)]
    viewButtons'' (btn : btns) = viewButton (btn & buttonRoundedMiddle & fill) : viewButtons'' btns
    buttonRoundedFirst =
      if b.compact
        then case b.direction of
          HorizontalFlow -> #roundedness .~ ButtonRoundedLeft
          VerticalFlow -> #roundedness .~ ButtonRoundedTop
        else #roundedness .~ ButtonRoundedAll
    buttonRoundedLast =
      if b.compact
        then case b.direction of
          HorizontalFlow -> #roundedness .~ ButtonRoundedRight
          VerticalFlow -> #roundedness .~ ButtonRoundedBottom
        else #roundedness .~ ButtonRoundedAll
    buttonRoundedMiddle =
      if b.compact
        then #roundedness .~ ButtonRoundedNone
        else #roundedness .~ ButtonRoundedAll
    fill =
      if b.compact
        then case b.direction of
          HorizontalFlow -> #fill .~ HorizontalFill
          VerticalFlow -> #fill .~ VerticalFill
        else #fill .~ NoFill

    expansion = case b.alignment of
      V.Start -> V.NoExpand
      V.Center -> V.Expand V.Center
      V.End -> V.Expand V.End

link :: [M.Attribute action] -> MisoString -> M.View m action
link attrs label = M.button_ (T.tailwind [T.LinkButton] : attrs) [V.buttonText_ label]

data ToggleState
  = ToggleOff
  | ToggleOn
  deriving (Eq, Show, Ord)

data TriState
  = TriStateOff
  | TriStateOn
  | TriStateIndeterminate
  deriving (Eq, Show, Ord)

applyButton :: a -> Button () a
applyButton = iconButton' IcnApply LblApply

cancelButton :: a -> Button () a
cancelButton a = iconButton' IcnCancel LblCancel a & (#buttonType .~ AlertButton)

deleteButton :: a -> Button () a
deleteButton a = iconButton' IcnDelete LblDelete a & (#buttonType .~ AlertButton)

editButton :: a -> Button () a
editButton = iconButton' IcnEdit LblEdit

applyLabelButton :: a -> Button () a
applyLabelButton = iconLabelButton' IcnApply LblApply

cancelLabelButton :: a -> Button () a
cancelLabelButton a = iconLabelButton' IcnCancel LblCancel a & (#buttonType .~ AlertButton)

class ToTriState a where
  toTriState :: a -> TriState

instance ToTriState TriState where
  toTriState = id

instance ToTriState Bool where
  toTriState False = TriStateOff
  toTriState True = TriStateOn

instance ToTriState [Bool] where
  toTriState xs
    | and xs = TriStateOn
    | all not xs = TriStateOff
    | otherwise = TriStateIndeterminate

instance ToTriState ToggleState where
  toTriState ToggleOff = TriStateOff
  toTriState ToggleOn = TriStateOn

instance ToTriState [ToggleState] where
  toTriState xs
    | all (== ToggleOn) xs = TriStateOn
    | all (== ToggleOff) xs = TriStateOff
    | otherwise = TriStateIndeterminate

instance ToTriState () where
  toTriState _ = TriStateOn
