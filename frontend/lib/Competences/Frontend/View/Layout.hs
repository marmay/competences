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
  , centeredContent
  , sideMenu

    -- * Sizing attributes (for extraAttrs)
  , fullHeight
  , fullScreen
  , minH0
  , shrinkAttr
  , overflowYScroll
  , overflowYAuto
  , fullWidth
  )
where

import Competences.Frontend.View.Tailwind (class_, classes)
import Data.Text qualified as T
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
flowSpring = M.div_ [class_ "flex-grow"] []

flow :: FlowDirection -> FlowLayout a
flow d = FlowLayout d NoExpand NoExpand NoSpace NoSpace []

hFlow, vFlow :: FlowLayout a
hFlow = flow HorizontalFlow
vFlow = flow VerticalFlow

viewFlow :: FlowLayout a -> [M.View m a] -> M.View m a
viewFlow l =
  M.div_
    ( [classes $ filter (not . T.null)
        [ "flex"
        , directionClass
        , expandDirectionalClass
        , expandOrthogonalClass
        , gapClass
        , marginClass
        ]
      ] <> l.extraAttrs
    )
  where
    directionClass = case l.direction of
      HorizontalFlow -> "flex-row"
      VerticalFlow -> "flex-col"
    expandDirectionalClass = case (l.direction, l.expandDirection) of
      (HorizontalFlow, Expand a) -> "w-full " <> alignD a
      (VerticalFlow, Expand a) -> alignD a
      (_, NoExpand) -> ""
    expandOrthogonalClass = case (l.direction, l.expandOrthogonal) of
      (HorizontalFlow, Expand a) -> "h-full " <> alignO a
      (VerticalFlow, Expand a) -> "w-full " <> alignO a
      (_, NoExpand) -> ""
    gapClass = case l.gap of
      NoSpace -> ""
      TinySpace -> "gap-1"
      SmallSpace -> "gap-2"
      MediumSpace -> "gap-4"
      LargeSpace -> "gap-8"
    marginClass = case l.margin of
      NoSpace -> ""
      TinySpace -> "m-1"
      SmallSpace -> "m-2"
      MediumSpace -> "m-4"
      LargeSpace -> "m-8"
    alignD a = case a of
      Start -> "justify-start"
      Center -> "justify-center"
      End -> "justify-end"
    alignO a = case a of
      Start -> "items-start"
      Center -> "items-center"
      End -> "items-end"

visibleIf :: Bool -> M.View m a -> M.View m a
visibleIf True v = v
visibleIf False v = M.div_ [class_ "hidden"] [v]

fixedWidth :: Int -> M.View m a -> M.View m a
fixedWidth w v = M.div_ [MS.style_ [("width", M.ms (show w) <> "px")]] [v]

flexGrow :: M.View m a -> M.View m a
flexGrow v = M.div_ [class_ "flex-grow"] [v]

centeredContent :: M.View m a -> M.View m a
centeredContent v = M.div_ [class_ "w-full flex justify-center"] [v]

hScrollable, vScrollable :: M.View m a -> M.View m a
hScrollable v = M.div_ [class_ "overflow-x-auto w-full"] [v]
vScrollable v = M.div_ [class_ "overflow-y-auto h-full"] [v]

hBorder, vBorder :: M.View m a
hBorder = M.div_ [class_ "h-1 w-full bg-sky-800"] []
vBorder = M.div_ [class_ "w-1 h-full bg-sky-800"] []

empty :: M.View m a
empty = M.div_ [] []

sideMenu :: M.View m a -> M.View m a -> M.View m a
sideMenu side main =
  viewFlow
    (hFlow & (#extraAttrs .~ [class_ "flex-1 h-full"]))
    [ M.div_ [class_ "w-[280px] h-full min-h-0 flex-shrink-0 border-r border-border pr-4"] [side]
    , M.div_ [class_ "h-full min-h-0 flex-grow overflow-y-auto pl-4"] [main]
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
    (vFlow & (#expandDirection .~ Expand Start) & (#extraAttrs .~ [class_ "h-screen"]))
    $ catMaybes
      [ fmap (\h -> M.header_ [class_ "border-b border-border bg-card"] [h]) maybeHeader
      , Just $ M.main_ [class_ "flex-1 overflow-y-auto bg-background"] [content]
      , fmap (\f -> M.footer_ [class_ "border-t border-border bg-card"] [f]) maybeFooter
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
    [ M.div_ [class_ "flex-shrink-0", MS.style_ [("min-width", M.ms (show leftWidth) <> "px")]] [left]
    , M.div_ [class_ "flex-grow"] [right]
    ]

-- | Form layout with consistent spacing and structure
-- Stacks form fields vertically with appropriate gaps
formLayout :: [M.View m a] -> M.View m a
formLayout = M.form_ [class_ "space-y-4"]

-- | Content section with optional title
-- Provides consistent spacing and visual grouping
section
  :: Maybe M.MisoString -- ^ Optional section title
  -> [M.View m a] -- ^ Section content
  -> M.View m a
section maybeTitle content =
  M.section_
    [class_ "space-y-3"]
    $ maybe [] (\title -> [M.h3_ [class_ "text-lg font-semibold text-stone-900"] [M.text title]]) maybeTitle
      <> content

-- ============================================================================
-- SIZING ATTRIBUTES (for use with extraAttrs)
-- ============================================================================

-- | Full height (h-full)
fullHeight :: M.Attribute a
fullHeight = class_ "h-full"

-- | Full screen height (h-screen)
fullScreen :: M.Attribute a
fullScreen = class_ "h-screen"

-- | Minimum height 0 (for flex children to allow shrinking)
minH0 :: M.Attribute a
minH0 = class_ "min-h-0"

-- | Shrink flex item
shrinkAttr :: M.Attribute a
shrinkAttr = class_ "shrink"

-- | Vertical scroll overflow
overflowYScroll :: M.Attribute a
overflowYScroll = class_ "overflow-y-scroll"

-- | Vertical auto overflow
overflowYAuto :: M.Attribute a
overflowYAuto = class_ "overflow-y-auto"

-- | Full width (w-full)
fullWidth :: M.Attribute a
fullWidth = class_ "w-full"
