module Competences.Frontend.View.Layout
  ( -- * Flow Layout
    FlowDirection (..)
  , FlowMod
    -- ** Gap modifiers
  , gapMicro
  , gapT
  , gapS
  , gapM
  , gapL
    -- ** Main-axis alignment modifiers
  , mainCenter
  , mainEnd
  , mainBetween
    -- ** Cross-axis alignment modifiers
  , crossStart
  , crossCenter
  , crossEnd
    -- ** Wrapping
  , flexWrap
    -- ** Container sizing
  , wFull
  , hFull
    -- ** Combinators
  , viewFlow
  , hFlow
  , vFlow
  , hFlow'
  , vFlow'
    -- ** View modifiers (Layer 2)
  , addClass
  , grow
  , shrink0

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
  )
where

import Competences.Frontend.View.Tailwind (class_, classes)
import Data.Text qualified as T
import Miso (Attribute (..), NS (..), View (..))
import Miso qualified as M
import Miso.CSS qualified as MS
import Miso.Html qualified as M

data FlowDirection = HorizontalFlow | VerticalFlow
  deriving (Eq, Show)

-- ============================================================================
-- FLOW LAYOUT API
-- ============================================================================

data Gap = NoGap | MicroGap | TinyGap | SmallGap | MediumGap | LargeGap

data MainAlign = MainDefault | MainCenter | MainEnd | MainBetween

data CrossAlign = CrossDefault | CrossStart | CrossCenter | CrossEnd

data FlowConfig = FlowConfig
  { direction :: !FlowDirection
  , gap :: !Gap
  , mainAlign :: !MainAlign
  , crossAlign :: !CrossAlign
  , wrap :: !Bool
  , wFull :: !Bool
  , hFull :: !Bool
  }

defaultFlowConfig :: FlowDirection -> FlowConfig
defaultFlowConfig d =
  FlowConfig
    { direction = d
    , gap = NoGap
    , mainAlign = MainDefault
    , crossAlign = CrossDefault
    , wrap = False
    , wFull = False
    , hFull = False
    }

-- | Monoid-based modifier for FlowConfig.
-- Combines left-to-right: @gapS <> crossCenter@ applies gapS first, then crossCenter.
newtype FlowMod = FlowMod (FlowConfig -> FlowConfig)

instance Semigroup FlowMod where
  FlowMod f <> FlowMod g = FlowMod (g . f)

instance Monoid FlowMod where
  mempty = FlowMod id

-- Gap modifiers
gapMicro, gapT, gapS, gapM, gapL :: FlowMod
gapMicro = FlowMod $ \c -> c{gap = MicroGap}
gapT = FlowMod $ \c -> c{gap = TinyGap}
gapS = FlowMod $ \c -> c{gap = SmallGap}
gapM = FlowMod $ \c -> c{gap = MediumGap}
gapL = FlowMod $ \c -> c{gap = LargeGap}

-- Main-axis alignment modifiers
mainCenter, mainEnd, mainBetween :: FlowMod
mainCenter = FlowMod $ \c -> c{mainAlign = MainCenter}
mainEnd = FlowMod $ \c -> c{mainAlign = MainEnd}
mainBetween = FlowMod $ \c -> c{mainAlign = MainBetween}

-- Cross-axis alignment modifiers
crossStart, crossCenter, crossEnd :: FlowMod
crossStart = FlowMod $ \c -> c{crossAlign = CrossStart}
crossCenter = FlowMod $ \c -> c{crossAlign = CrossCenter}
crossEnd = FlowMod $ \c -> c{crossAlign = CrossEnd}

-- Wrapping modifier
flexWrap :: FlowMod
flexWrap = FlowMod $ \c -> c{wrap = True}

-- Container sizing modifiers
wFull, hFull :: FlowMod
wFull = FlowMod $ \c -> c{wFull = True}
hFull = FlowMod $ \c -> c{hFull = True}

-- | Render a flow layout from a FlowConfig.
viewFlow :: FlowConfig -> [M.View m a] -> M.View m a
viewFlow c =
  M.div_
    [ classes $
        filter
          (not . T.null)
          [ "flex"
          , dirCls
          , gapCls
          , mainCls
          , crossCls
          , wrapCls
          , wCls
          , hCls
          ]
    ]
  where
    dirCls = case c.direction of
      HorizontalFlow -> "flex-row"
      VerticalFlow -> "flex-col"
    gapCls = case c.gap of
      NoGap -> ""
      MicroGap -> "gap-0.5"
      TinyGap -> "gap-1"
      SmallGap -> "gap-2"
      MediumGap -> "gap-4"
      LargeGap -> "gap-8"
    mainCls = case c.mainAlign of
      MainDefault -> ""
      MainCenter -> "justify-center"
      MainEnd -> "justify-end"
      MainBetween -> "justify-between"
    crossCls = case c.crossAlign of
      CrossDefault -> ""
      CrossStart -> "items-start"
      CrossCenter -> "items-center"
      CrossEnd -> "items-end"
    wrapCls = if c.wrap then "flex-wrap" else ""
    wCls = if c.wFull then "w-full" else ""
    hCls = if c.hFull then "h-full" else ""

-- | Horizontal flow with modifiers.
hFlow :: FlowMod -> [M.View m a] -> M.View m a
hFlow (FlowMod f) = viewFlow (f $ defaultFlowConfig HorizontalFlow)

-- | Vertical flow with modifiers.
vFlow :: FlowMod -> [M.View m a] -> M.View m a
vFlow (FlowMod f) = viewFlow (f $ defaultFlowConfig VerticalFlow)

-- | Horizontal flow with no modifiers.
hFlow' :: [M.View m a] -> M.View m a
hFlow' = hFlow mempty

-- | Vertical flow with no modifiers.
vFlow' :: [M.View m a] -> M.View m a
vFlow' = vFlow mempty

-- ============================================================================
-- VIEW MODIFIERS (Layer 2)
-- ============================================================================

-- | Add a CSS class to a View node.
-- For VNode, injects the class directly into the attribute list.
-- For VText/VComp, wraps in a span.
addClass :: T.Text -> M.View m a -> M.View m a
addClass cls (VNode ns tag attrs children) =
  VNode ns tag (attrs <> [ClassList [M.ms cls]]) children
addClass cls v =
  VNode HTML "span" [ClassList [M.ms cls]] [v]

-- | Make a flex child grow to fill available space (flex-1).
grow :: M.View m a -> M.View m a
grow = addClass "flex-1"

-- | Prevent a flex child from shrinking (flex-shrink-0).
shrink0 :: M.View m a -> M.View m a
shrink0 = addClass "flex-shrink-0"

-- ============================================================================
-- UTILITIES
-- ============================================================================

flowSpring :: M.View m a
flowSpring = M.div_ [class_ "flex-grow"] []

visibleIf :: Bool -> M.View m a -> M.View m a
visibleIf True v = v
visibleIf False v = M.div_ [class_ "hidden"] [v]

fixedWidth :: Int -> M.View m a -> M.View m a
fixedWidth w v = M.div_ [MS.style_ [("width", M.ms (show w) <> "px")]] [v]

flexGrow :: M.View m a -> M.View m a
flexGrow v = M.div_ [class_ "flex-grow w-full"] [v]

centeredContent :: M.View m a -> M.View m a
centeredContent v = M.div_ [class_ "w-full h-full min-h-0 flex justify-center"] [v]

hScrollable, vScrollable :: M.View m a -> M.View m a
hScrollable = addClass "overflow-x-scroll min-w-0"
vScrollable = addClass "overflow-y-scroll min-h-0"

hBorder, vBorder :: M.View m a
hBorder = M.div_ [class_ "h-1 w-full bg-sky-800"] []
vBorder = M.div_ [class_ "w-1 h-full bg-sky-800"] []

empty :: M.View m a
empty = M.div_ [] []

sideMenu :: M.View m a -> M.View m a -> M.View m a
sideMenu side main =
  M.div_
    [class_ "flex-1 h-full"]
    [ hFlow hFull
        [ M.div_ [class_ "w-[280px] h-full min-h-0 flex-shrink-0 flex flex-col border-r border-border pr-4 print:hidden"] [side]
        , M.div_ [class_ "h-full min-h-0 flex-grow overflow-y-auto pl-4"] [main]
        ]
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
  M.div_
    [class_ "h-screen"]
    [ vFlow
        mempty
        $ catMaybes
          [ fmap (\h -> M.header_ [class_ "border-b border-border bg-card"] [h]) maybeHeader
          , Just $ M.main_ [class_ "flex-1 overflow-y-auto bg-background"] [content]
          , fmap (\f -> M.footer_ [class_ "border-t border-border bg-card"] [f]) maybeFooter
          ]
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
  hFlow
    (gapS <> wFull)
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
