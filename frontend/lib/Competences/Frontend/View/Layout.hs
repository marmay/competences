module Competences.Frontend.View.Layout
  ( -- * Flow Layout (Foundation)
    hFlow
  , vFlow
  , flow
  , viewFlow
  , Expand (..)
  , FlowDirection (..)
  , LayoutSpace (..)
  , Alignment (..)
  , FlowLayout (..)

    -- * Higher-level layouts
  , pageLayout
  , splitView
  , formLayout
  , section

    -- * Utilities
  , empty
  , flowSpring
  , hBorder
  , hScrollable
  , vBorder
  , vScrollable
  , visibleIf
  , fixedWidth
  , flexGrow
  , sideMenu
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

-- ============================================================================
-- HIGHER-LEVEL LAYOUT PRIMITIVES
-- ============================================================================

-- | Page layout with optional header, main content, and optional footer
-- Provides consistent full-height layout with scrollable content area
pageLayout
  :: Maybe (M.View m a) -- ^ Optional header
  -> M.View m a -- ^ Main content (scrollable)
  -> Maybe (M.View m a) -- ^ Optional footer
  -> M.View m a
pageLayout maybeHeader content maybeFooter =
  viewFlow
    (vFlow & (#expandDirection .~ Expand Start) & (#extraAttrs .~ [T.tailwind [T.HScreen]]))
    $ catMaybes
      [ fmap (\h -> M.header_ [T.class_ "border-b border-stone-200 bg-white"] [h]) maybeHeader
      , Just $ M.main_ [T.class_ "flex-1 overflow-y-auto"] [content]
      , fmap (\f -> M.footer_ [T.class_ "border-t border-stone-200 bg-white"] [f]) maybeFooter
      ]
  where
    catMaybes = foldr (\mx xs -> maybe xs (: xs) mx) []

-- | Two-column split view with flexible sizing
-- Left column has fixed/minimum width, right column grows to fill space
splitView
  :: Int -- ^ Left column width in pixels (or minimum width)
  -> M.View m a -- ^ Left content
  -> M.View m a -- ^ Right content
  -> M.View m a
splitView leftWidth left right =
  viewFlow
    (hFlow & (#gap .~ SmallSpace) & (#expandDirection .~ Expand Start))
    [ M.div_ [T.class_ ("flex-shrink-0"), MS.style_ [("min-width", M.ms (show leftWidth) <> "px")]] [left]
    , M.div_ [T.tailwind [T.FlexGrow]] [right]
    ]

-- | Form layout with consistent spacing and structure
-- Stacks form fields vertically with appropriate gaps
formLayout :: [M.View m a] -> M.View m a
formLayout fields =
  M.form_
    [T.class_ "space-y-4"]
    fields

-- | Content section with optional title
-- Provides consistent spacing and visual grouping
section
  :: Maybe M.MisoString -- ^ Optional section title
  -> [M.View m a] -- ^ Section content
  -> M.View m a
section maybeTitle content =
  M.section_
    [T.class_ "space-y-3"]
    $ maybe [] (\title -> [M.h3_ [T.class_ "text-lg font-semibold text-stone-900"] [M.text title]]) maybeTitle
      <> content
