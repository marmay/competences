{-# LANGUAGE OverloadedStrings #-}

module Competences.Frontend.View.Button
  ( -- * Basecoat-style buttons (new approach)
    ButtonVariant (..)
  , ButtonSize (..)
  , ButtonContents (..)
  , ButtonContentsStyle (..)
  , ButtonDisabled (..)
  , ToButtonContents (..)
  , ToAction (..)
  , button
  , button'
  , render
  , primary
  , primarySm
  , primaryLg
  , secondary
  , secondarySm
  , secondaryLg
  , destructive
  , destructiveSm
  , destructiveLg
  , ghost
  , ghostSm
  , ghostLg
  , link
  , linkSm
  , linkLg
  , outline
  , outlineSm
  , outlineLg
  , toggle
  , toggleSm
  , toggleLg
  , applyButtonC
  , cancelButtonC
  , deleteButtonC
  , editButtonC
  , moveButtonC
  , applyButton
  , cancelButton
  , deleteButton
  , editButton
  , moveButton
  , buttonGroup
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Data.Maybe (maybeToList)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, intercalate)

-- | Button variant following Basecoat design system
data ButtonVariant
  = -- | Primary action (sky-600 background)
    Primary
  | -- | Secondary action (stone-200 background)
    Secondary
  | -- | Destructive action (red-600 background)
    Destructive
  | -- | Ghost button (transparent, hover background)
    Ghost
  | -- | Link-style button (underline on hover)
    Link
  | -- | Outline button (border, transparent bg) - for toggle unselected state
    Outline
  deriving (Eq, Show)

-- | Button size
data ButtonSize
  = Small
  | Regular
  | Large
  deriving (Eq, Show)

-- | Contents of the button
data ButtonContents
  = TextOnly !MisoString
  | IconOnly !Icon
  | IconText !Icon !MisoString
  deriving (Eq, Show)

data ButtonContentsStyle
  = TextOnlyS
  | IconOnlyS
  | IconTextS
  deriving (Eq, Show)

data ButtonDisabled = Disabled

class ToButtonContents a where
  toButtonContents :: a -> ButtonContents

instance ToButtonContents ButtonContents where
  toButtonContents = id

instance ToButtonContents Icon where
  toButtonContents = IconOnly

instance ToButtonContents MisoString where
  toButtonContents = TextOnly

instance ToButtonContents Label where
  toButtonContents = TextOnly . translate'

instance ToButtonContents (Icon, MisoString) where
  toButtonContents (i, t) = IconText i t

instance ToButtonContents (Icon, Label) where
  toButtonContents (i, l) = IconText i (translate' l)

instance ToButtonContents (ButtonContentsStyle, Icon, MisoString) where
  toButtonContents = toButtonContents'

instance ToButtonContents (ButtonContentsStyle, Icon, Label) where
  toButtonContents = toButtonContents' . (\(s, i, l) -> (s, i, translate' l))

toButtonContents' :: (ButtonContentsStyle, Icon, MisoString) -> ButtonContents
toButtonContents' (TextOnlyS, _, t) = TextOnly t
toButtonContents' (IconOnlyS, i, _) = IconOnly i
toButtonContents' (IconTextS, i, t) = IconText i t

class ToAction a' a where
  toAction :: a' -> Maybe a

instance ToAction a a where
  toAction = Just

instance ToAction (Maybe a) a where
  toAction = id

instance ToAction (Bool, a) a where
  toAction (tf, a)
    | tf = Just a
    | otherwise = Nothing

instance ToAction ButtonDisabled a where
  toAction Disabled = Nothing

data ButtonConfig a = ButtonConfig
  { contents :: !ButtonContents
  , action :: !(Maybe a)
  , tooltip :: !(Maybe MisoString)
  }

button :: (ToButtonContents c, ToAction a' a) => c -> a' -> MisoString -> ButtonConfig a
button c a t =
  ButtonConfig
    { contents = toButtonContents c
    , action = toAction a
    , tooltip = Just t
    }

button' :: (ToButtonContents c, ToAction a' a) => c -> a' -> ButtonConfig a
button' c a =
  ButtonConfig
    { contents = toButtonContents c
    , action = toAction a
    , tooltip = Nothing
    }

render :: ButtonVariant -> ButtonSize -> ButtonConfig a -> M.View m a
render v s ButtonConfig {contents = c, action = a, tooltip = t} =
  M.button_ attrs [renderContents c]
  where
    attrs = btnAttrs <> tooltipAttrs
    btnAttrs = case a of
      (Just a') -> [M.onClick a', MP.class_ activeClass]
      Nothing -> [MP.disabled_, MP.class_ disabledClass]
    tooltipAttrs = case t of
      (Just t') -> [MP.title_ t']
      Nothing -> []
    -- Basecoat button class naming: btn[-size][-icon][-variant]
    -- Primary has no variant suffix (btn = primary)
    activeClass =
      intercalate "-" $
        ["btn"]
          <> maybeToList (sizeClass s)
          <> maybeToList (iconClass c)
          <> maybeToList (variantClass v)
      where
        variantClass Primary = Nothing
        variantClass Secondary = Just "secondary"
        variantClass Destructive = Just "destructive"
        variantClass Ghost = Just "ghost"
        variantClass Link = Just "link"
        variantClass Outline = Just "outline"

        sizeClass Small = Just "sm"
        sizeClass Regular = Nothing
        sizeClass Large = Just "lg"

        iconClass (IconOnly _) = Just "icon"
        iconClass _ = Nothing
    disabledClass =
      let baseClass = case s of Small -> "btn-sm"; Regular -> "btn"; Large -> "btn-lg"
       in (baseClass <> " bg-gray-300 hover:bg-gray-300 cursor-not-allowed")

    renderContents :: ButtonContents -> M.View m a
    renderContents (TextOnly t') = M.text_ [t']
    renderContents (IconOnly i) = icon [] i
    renderContents (IconText i t') = M.div_ [MP.class_ "flex items-center gap-2"] [icon [] i, M.span_ [] [M.text_ [t']]]

primary
  , primarySm
  , primaryLg
  , secondary
  , secondarySm
  , secondaryLg
  , destructive
  , destructiveSm
  , destructiveLg
  , ghost
  , ghostSm
  , ghostLg
  , link
  , linkSm
  , linkLg
  , outline
  , outlineSm
  , outlineLg
    :: ButtonConfig a -> M.View m a
primary = render Primary Regular
primarySm = render Primary Small
primaryLg = render Primary Large
secondary = render Secondary Regular
secondarySm = render Secondary Small
secondaryLg = render Secondary Large
destructive = render Destructive Regular
destructiveSm = render Destructive Small
destructiveLg = render Destructive Large
ghost = render Ghost Regular
ghostSm = render Ghost Small
ghostLg = render Ghost Large
link = render Link Regular
linkSm = render Link Small
linkLg = render Link Large
outline = render Outline Regular
outlineSm = render Outline Small
outlineLg = render Outline Large

toggle' :: Bool -> ButtonSize -> ButtonConfig a -> M.View m a
toggle' True = render Primary
toggle' False = render Outline

toggle, toggleSm, toggleLg :: Bool -> ButtonConfig a -> M.View m a
toggle t = toggle' t Regular
toggleSm t = toggle' t Small
toggleLg t = toggle' t Large

applyButtonC
  , cancelButtonC
  , deleteButtonC
  , editButtonC
  , moveButtonC
    :: (ToAction a' a) => a' -> ButtonConfig a
applyButtonC = button' (IcnApply, LblApply)
cancelButtonC = button' (IcnCancel, LblCancel)
deleteButtonC = button' (IcnDelete, LblDelete)
editButtonC = button' (IcnEdit, LblEdit)
moveButtonC = button' (IcnReorder, LblMove)

applyButton, cancelButton, deleteButton, editButton, moveButton :: (ToAction a' a) => a' -> M.View m a
applyButton = primary . applyButtonC
cancelButton = destructive . cancelButtonC
deleteButton = destructive . deleteButtonC
editButton = secondary . editButtonC
moveButton = secondary . moveButtonC

-- | Button group with connected edges (Basecoat pattern)
-- Uses role="group" for accessibility
buttonGroup :: [M.View model action] -> M.View model action
buttonGroup =
  M.div_
    [ class_ "button-group"
    , M.textProp "role" "group"
    ]
