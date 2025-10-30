module Competences.Frontend.View.Layout
  ( hFlow
  , vFlow
  , flow
  , empty
  , flowSpring
  , hBorder
  , hScrollable
  , vBorder
  , vScrollable
  , viewFlow
  , visibleIf
  , fixedWidth
  , flexGrow
  , sideMenu
  , Expand (..)
  , FlowDirection (..)
  , LayoutSpace (..)
  , Alignment (..)
  , FlowLayout (..)
  )
where

import Competences.Frontend.View.Tailwind qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MS
import Miso.Html qualified as M
import Optics.Core ((&), (.~))

data FlowDirection = HorizontalFlow | VerticalFlow
  deriving (Eq, Show)

data Expand = Expand Alignment | NoExpand
  deriving (Eq, Show)

data LayoutSpace = NoSpace | TinySpace | SmallSpace | MediumSpace | LargeSpace
  deriving (Eq, Show)

data Alignment = Start | Center | End
  deriving (Eq, Show)

data FlowLayout a = FlowLayout
  { direction :: !FlowDirection
  , expandDirection :: !Expand
  , expandOrthogonal :: !Expand
  , gap :: !LayoutSpace
  , margin :: !LayoutSpace
  , extraAttrs :: [M.Attribute a]
  }
  deriving (Generic)

flowSpring :: M.View m a
flowSpring = M.div_ [T.tailwind [T.FlexGrow]] []

flow :: FlowDirection -> FlowLayout a
flow d = FlowLayout d NoExpand NoExpand NoSpace NoSpace []

hFlow, vFlow :: FlowLayout a
hFlow = flow HorizontalFlow
vFlow = flow VerticalFlow

viewFlow :: FlowLayout a -> [M.View m a] -> M.View m a
viewFlow l =
  M.div_
    ( [T.tailwind $ mconcat [[T.Flex], direction, expandDirectional, expandOrthogonal, gap, margin]]
        <> l.extraAttrs
    )
  where
    direction = case l.direction of
      HorizontalFlow -> [T.FlexRow]
      VerticalFlow -> [T.FlexCol]
    expandDirectional = case (l.direction, l.expandDirection) of
      (HorizontalFlow, Expand a) -> T.WFull : alignD a
      (VerticalFlow, Expand a) -> alignD a
      (_, NoExpand) -> []
    expandOrthogonal = case (l.direction, l.expandOrthogonal) of
      (HorizontalFlow, Expand a) -> T.HFull : alignO a
      (VerticalFlow, Expand a) -> T.WFull : alignO a
      (_, NoExpand) -> []
    gap = case l.gap of
      NoSpace -> []
      TinySpace -> [T.Gap1]
      SmallSpace -> [T.Gap2]
      MediumSpace -> [T.Gap4]
      LargeSpace -> [T.Gap8]
    margin = case l.margin of
      NoSpace -> []
      TinySpace -> [T.M1]
      SmallSpace -> [T.M2]
      MediumSpace -> [T.M4]
      LargeSpace -> [T.M8]
    alignD a = case a of
      Start -> [T.JustifyStart]
      Center -> [T.JustifyCenter]
      End -> [T.JustifyEnd]
    alignO a = case a of
      Start -> [T.ItemsStart]
      Center -> [T.ItemsLastBaseline]
      End -> [T.ItemsEnd]

visibleIf :: Bool -> M.View m a -> M.View m a
visibleIf True v = v
visibleIf False v = M.div_ [T.tailwind [T.Hidden]] [v]

fixedWidth :: Int -> M.View m a -> M.View m a
fixedWidth w v = M.div_ [MS.style_ [("width", M.ms (show w) <> "px")]] [v]

flexGrow :: M.View m a -> M.View m a
flexGrow v = M.span_ [T.tailwind [T.FlexGrow]] [v]

hScrollable, vScrollable :: M.View m a -> M.View m a
hScrollable v = M.div_ [T.tailwind [T.OverflowXAuto, T.WFull]] [v]
vScrollable v = M.div_ [T.tailwind [T.OverflowYAuto, T.HFull]] [v]

hBorder, vBorder :: M.View m a
hBorder = M.div_ [T.tailwind [T.HBorder]] []
vBorder = M.div_ [T.tailwind [T.VBorder]] []

empty :: M.View m a
empty = M.div_ [] []

sideMenu :: M.View m a -> M.View m a -> M.View m a
sideMenu side main =
  viewFlow
    (hFlow & (#expandDirection .~ Expand Start) & (#expandOrthogonal .~ Expand Start))
    [ M.div_ [T.tailwind [T.HFull]] [side]
    , M.div_ [T.tailwind [T.HFull, T.FlexGrow]] [main]
    ]
