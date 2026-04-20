{-# LANGUAGE OverloadedStrings #-}

module Competences.Frontend.View.Button
  ( -- * Basecoat-style buttons (new approach)
    ButtonVariant (..)
  , ButtonSize (..)
  , ButtonHSize (..)
  , ButtonVSize (..)
  , ButtonContents (..)
  , ButtonContentsStyle (..)
  , ButtonConfig (..)
  , ButtonDisabled (..)
  , ToButtonContents (..)
  , ToAction (..)
  , button
  , render
  , renderActive
  , renderDisabled
  , renderDisabledPulse
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
  , toggleGhost
  , toggleGhostSm
  , toggleGhostLg
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
  , regularButtonSize
  , smallButtonSize
  , largeButtonSize
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
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

data ButtonSize = ButtonSize ButtonVSize ButtonHSize
  deriving (Eq, Show)

data ButtonVSize
  = Small
  | Regular
  | Large
  deriving (Eq, Show)

data ButtonHSize
  = Adjust
  | Full
  deriving (Eq, Show)

-- | Contents of the button
data ButtonContents
  = TextOnly !MisoString
  | IconOnly !Icon.Icon
  | SizedIcon !Icon.Size !Icon.Icon -- ^ Custom-sized icon, no text
  | IconText !Icon.Icon !MisoString
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

instance ToButtonContents Icon.Icon where
  toButtonContents = IconOnly

instance ToButtonContents MisoString where
  toButtonContents = TextOnly

instance ToButtonContents Label where
  toButtonContents = TextOnly . translate'

instance ToButtonContents (Icon.Size, Icon.Icon) where
  toButtonContents (s, i) = SizedIcon s i

instance ToButtonContents (Icon.Icon, MisoString) where
  toButtonContents (i, t) = IconText i t

instance ToButtonContents (Icon.Icon, Label) where
  toButtonContents (i, l) = IconText i (translate' l)

instance ToButtonContents (ButtonContentsStyle, Icon.Icon, MisoString) where
  toButtonContents = toButtonContents'

instance ToButtonContents (ButtonContentsStyle, Icon.Icon, Label) where
  toButtonContents = toButtonContents' . (\(s, i, l) -> (s, i, translate' l))

toButtonContents' :: (ButtonContentsStyle, Icon.Icon, MisoString) -> ButtonContents
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
  }

button :: (ToButtonContents c, ToAction a' a) => c -> a' -> ButtonConfig a
button c a =
  ButtonConfig
    { contents = toButtonContents c
    , action = toAction a
    }

render :: ButtonVariant -> ButtonSize -> ButtonConfig a -> M.View m a
render v s ButtonConfig {contents = c, action = a} =
  case a of
    (Just a') -> renderActive v s c [M.onClick a']
    Nothing -> renderDisabled s c

renderActive :: ButtonVariant -> ButtonSize -> ButtonContents -> [M.Attribute a] -> M.View m a
renderActive v (ButtonSize vSize hSize) c attrs =
  M.button_ (MP.class_ activeClass : hSizeAttrs hSize <> attrs) [renderContents vSize c]
  where
    activeClass =
      intercalate "-" $
        ["btn"]
          <> maybeToList (sizeClass vSize)
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
        iconClass (SizedIcon _ _) = Just "icon"
        iconClass _ = Nothing

renderDisabled :: ButtonSize -> ButtonContents -> M.View m a
renderDisabled (ButtonSize vSize hSize) c =
  M.button_ ([MP.disabled_, MP.class_ disabledClass] <> hSizeAttrs hSize)
    [renderContents vSize c]
  where
    disabledClass =
      let baseClass = case vSize of Small -> "btn-sm"; Regular -> "btn"; Large -> "btn-lg"
       in (baseClass <> " bg-gray-300 hover:bg-gray-300 cursor-not-allowed")

renderDisabledPulse :: ButtonSize -> ButtonContents -> M.View m a
renderDisabledPulse sz c = Layout.addClass "animate-pulse" $ renderDisabled sz c


renderContents :: ButtonVSize -> ButtonContents -> M.View m a
renderContents _s (TextOnly t') = M.text_ [t']
renderContents s (IconOnly i) = Icon.iconS (toIconSize s) i
renderContents _s (SizedIcon sz i) = Icon.iconS sz i
renderContents s (IconText i t') = Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter) [Icon.iconS (toIconSize s) i, M.span_ [] [M.text_ [t']]]

toIconSize :: ButtonVSize -> Icon.Size
toIconSize Small = Icon.Small
toIconSize Regular = Icon.Regular
toIconSize Large = Icon.Large

hSizeAttrs :: ButtonHSize -> [M.Attribute a]
hSizeAttrs Adjust = []
hSizeAttrs Full = [MP.class_ "w-full"]

regularButtonSize
  , smallButtonSize
  , largeButtonSize
    :: ButtonSize
regularButtonSize = ButtonSize Regular Adjust
smallButtonSize = ButtonSize Small Adjust
largeButtonSize = ButtonSize Large Adjust

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
primary = render Primary regularButtonSize
primarySm = render Primary smallButtonSize
primaryLg = render Primary largeButtonSize
secondary = render Secondary regularButtonSize
secondarySm = render Secondary smallButtonSize
secondaryLg = render Secondary largeButtonSize
destructive = render Destructive regularButtonSize
destructiveSm = render Destructive smallButtonSize
destructiveLg = render Destructive largeButtonSize
ghost = render Ghost regularButtonSize
ghostSm = render Ghost smallButtonSize
ghostLg = render Ghost largeButtonSize
link = render Link regularButtonSize
linkSm = render Link smallButtonSize
linkLg = render Link largeButtonSize
outline = render Outline regularButtonSize
outlineSm = render Outline smallButtonSize
outlineLg = render Outline largeButtonSize

toggle' :: Bool -> ButtonSize -> ButtonConfig a -> M.View m a
toggle' True = render Primary
toggle' False = render Outline

toggle, toggleSm, toggleLg :: Bool -> ButtonConfig a -> M.View m a
toggle t = toggle' t regularButtonSize
toggleSm t = toggle' t smallButtonSize
toggleLg t = toggle' t largeButtonSize

toggleGhost' :: Bool -> ButtonSize -> ButtonConfig a -> M.View m a
toggleGhost' True = render Secondary
toggleGhost' False = render Ghost

toggleGhost, toggleGhostSm, toggleGhostLg :: Bool -> ButtonConfig a -> M.View m a
toggleGhost t = toggleGhost' t regularButtonSize
toggleGhostSm t = toggleGhost' t smallButtonSize
toggleGhostLg t = toggleGhost' t largeButtonSize

applyButtonC
  , cancelButtonC
  , deleteButtonC
  , editButtonC
  , moveButtonC
    :: (ToAction a' a) => a' -> ButtonConfig a
applyButtonC = button (Icon.IcnApply, LblApply)
cancelButtonC = button (Icon.IcnCancel, LblCancel)
deleteButtonC = button (Icon.IcnDelete, LblDelete)
editButtonC = button (Icon.IcnEdit, LblEdit)
moveButtonC = button (Icon.IcnReorder, LblMove)

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
