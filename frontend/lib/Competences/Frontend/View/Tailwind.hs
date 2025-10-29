module Competences.Frontend.View.Tailwind
  ( tailwind
  , tailwind'
  , tailwindColors
  , requiredClasses
  , TailwindCls (..)
  , ColorUtility (..)
  , Color (..)
  , ColorStep (..)
  )
where

import Data.Set qualified as Set
import Data.Text qualified as T
import Miso qualified as M
import qualified Miso.Html.Property as M

tailwind :: [TailwindCls] -> M.Attribute a
tailwind clses = M.class_ $ M.ms $ T.intercalate " " $ concatMap tailwindNames clses

tailwind' :: TailwindCls -> M.Attribute a
tailwind' cls = M.class_ $ M.ms $ T.intercalate " " $ tailwindNames cls

tailwindColors :: [(ColorUtility, Color, ColorStep, Opacity)] -> M.Attribute a
tailwindColors = M.class_ . M.ms . T.intercalate " " . map (\(u, c, s, o) -> useColor u c s o)

requiredClasses :: Set.Set T.Text
requiredClasses = Set.fromList $ concatMap tailwindNames [minBound .. maxBound]

-- | Each TailwindCls is a bundle of native Tailwind classes.
data TailwindCls
  = Absolute
  | AlertButtonOn
  | AlertButtonOff
  | AlertButtonIndeterminate
  | AlignMiddle
  | BottomFull
  | ButtonText
  | Flex
  | FlexRow
  | FlexCol
  | FlexGrow
  | FontBold
  | Gap1
  | Gap2
  | Gap4
  | Gap8
  | HFull
  | Hidden
  | HBorder
  | H96
  | IconButton
  | IconLabelButton
  | ItemsStart
  | ItemsLastBaseline
  | ItemsCenter
  | ItemsEnd
  | JustifyStart
  | JustifyCenter
  | JustifyEnd
  | LinkButton
  | M1
  | M2
  | M4
  | M8
  | ModalHost
  | ModalDialog
  | OverflowXAuto
  | OverflowYAuto
  | OverflowYScroll
  | P1
  | P2
  | P4
  | RegularBorder
  | RegularButtonOn
  | RegularButtonOff
  | RegularButtonIndeterminate
  | Relative
  | Rounded
  | RoundedLeft
  | RoundedRight
  | RoundedTop
  | RoundedBottom
  | Shrink
  | SizeFit
  | TableCell
  | TableFixed
  | TextCenter
  | TextSm
  | TextLg
  | TextRight
  | TextXl
  | TooltipBox
  | VBorder
  | W16
  | W24
  | W32
  | W40
  | W96
  | WThird
  | WHalf
  | WFull
  | WQuarterVh
  deriving (Eq, Show, Enum, Bounded)

tailwindNames :: TailwindCls -> [T.Text]
tailwindNames Absolute = ["absolute"]
tailwindNames BottomFull = ["bottom-full"]
tailwindNames AlertButtonOn =
  [ useColor Border Red I800 O100
  , useColor Bg Red I500 O100
  , useColor Text Stone I50 O100
  , useColor Stroke Stone I50 O100
  , useColor Fill Stone I100 O0
  , "hover:" <> useColor Bg Red I600 O100
  ]
tailwindNames AlertButtonOff =
  [ useColor Border Red I800 O100
  , useColor Bg Red I500 O100
  , useColor Text Stone I50 O100
  , useColor Stroke Stone I50 O100
  , useColor Fill Stone I100 O0
  , "hover:" <> useColor Bg Red I600 O100
  ]
tailwindNames AlertButtonIndeterminate =
  [ useColor Border Red I800 O100
  , useColor Bg Red I500 O100
  , useColor Text Stone I50 O100
  , useColor Stroke Stone I50 O100
  , useColor Fill Stone I100 O0
  , "hover:" <> useColor Bg Red I600 O100
  ]
tailwindNames AlignMiddle = ["align-middle"]
tailwindNames ButtonText = ["text-sm/5"]
tailwindNames Flex = ["flex"]
tailwindNames FlexRow = ["flex-row"]
tailwindNames FlexCol = ["flex-col"]
tailwindNames FlexGrow = ["flex-grow"]
tailwindNames FontBold = ["font-bold"]
tailwindNames Gap1 = ["gap-1"]
tailwindNames Gap2 = ["gap-2"]
tailwindNames Gap4 = ["gap-4"]
tailwindNames Gap8 = ["gap-8"]
tailwindNames HBorder = ["h-1", "w-full", useColor Bg Sky I800 O100]
tailwindNames HFull = ["h-full"]
tailwindNames H96 = ["h-96"]
tailwindNames Hidden = ["hidden"]
tailwindNames IconButton = ["border", "rounded"]
tailwindNames IconLabelButton = ["border", "rounded", "p-1"]
tailwindNames ItemsStart = ["items-start"]
tailwindNames ItemsLastBaseline = ["items-last-baseline"]
tailwindNames ItemsCenter = ["items-center"]
tailwindNames ItemsEnd = ["items-end"]
tailwindNames JustifyStart = ["justify-start"]
tailwindNames JustifyCenter = ["justify-center"]
tailwindNames JustifyEnd = ["justify-end"]
tailwindNames LinkButton = []
tailwindNames M1 = ["m-1"]
tailwindNames M2 = ["m-2"]
tailwindNames M4 = ["m-4"]
tailwindNames M8 = ["m-8"]
tailwindNames ModalHost = ["fixed", "inset-0", "z-50", "overflow-y-auto", useColor Bg Gray I500 O50]
tailwindNames ModalDialog =
  [ "relative"
  , "top-20"
  , "mx-auto"
  , "p-5"
  , "border"
  , "shadow-lg"
  , "rounded-md"
  , "w-1/2"
  , useColor Bg Stone I50 O100
  , useColor Border Gray I200 O100
  ]
tailwindNames OverflowXAuto = ["overflow-x-auto"]
tailwindNames OverflowYAuto = ["overflow-y-auto"]
tailwindNames OverflowYScroll = ["overflow-y-scroll"]
tailwindNames P1 = ["p-1"]
tailwindNames P2 = ["p-2"]
tailwindNames P4 = ["p-4"]
tailwindNames RegularBorder = ["border", "rounded", useColor Border Gray I200 O100]
tailwindNames RegularButtonOn =
  [ useColor Border Sky I800 O100
  , useColor Bg Sky I500 O100
  , useColor Text Stone I50 O100
  , useColor Stroke Stone I50 O100
  , useColor Fill Stone I100 O0
  , "hover:" <> useColor Bg Sky I600 O100
  ]
tailwindNames RegularButtonOff =
  [ useColor Border Sky I200 O100
  , useColor Bg Sky I300 O100
  , useColor Text Stone I50 O100
  , useColor Stroke Stone I50 O100
  , useColor Fill Stone I100 O0
  , "hover:" <> useColor Bg Sky I300 O100
  ]
tailwindNames RegularButtonIndeterminate =
  [ useColor Border Yellow I500 O100
  , useColor Bg Gray I500 O100
  , useColor Text Stone I50 O100
  , useColor Stroke Stone I50 O100
  , useColor Fill Stone I100 O0
  , "hover:" <> useColor Bg Sky I600 O100
  ]
tailwindNames Relative = ["relative"]
tailwindNames Rounded = ["rounded-md"]
tailwindNames RoundedLeft = ["rounded-l-md"]
tailwindNames RoundedRight = ["rounded-r-md"]
tailwindNames RoundedTop = ["rounded-t-md"]
tailwindNames RoundedBottom = ["rounded-b-md"]
tailwindNames Shrink = ["shrink"]
tailwindNames SizeFit = ["size-fit"]
tailwindNames TableCell = ["border", useColor Border Sky I800 O100]
tailwindNames TableFixed = ["table-fixed"]
tailwindNames TextCenter = ["text-center"]
tailwindNames TextSm = ["text-sm"]
tailwindNames TextLg = ["text-lg"]
tailwindNames TextXl = ["text-xl"]
tailwindNames TooltipBox =
  [ useColor Border Stone I50 O100
  , useColor Bg Gray I100 O100
  ]
tailwindNames TextRight = ["text-right"]
tailwindNames VBorder = ["w-1", "h-full", useColor Bg Sky I800 O100]
tailwindNames W16 = ["w-16"]
tailwindNames W24 = ["w-24"]
tailwindNames W32 = ["w-32"]
tailwindNames W40 = ["w-40"]
tailwindNames W96 = ["w-96"]
tailwindNames WThird = ["w-1/3"]
tailwindNames WHalf = ["w-1/2"]
tailwindNames WFull = ["w-full"]
tailwindNames WQuarterVh = ["w-[25vh]"]

data ColorUtility
  = Bg
  | Text
  | Decoration
  | Border
  | Outline
  | Shadow
  | InsetShadow
  | Ring
  | InsetRing
  | Accent
  | Caret
  | Fill
  | Stroke
  deriving (Eq, Show, Enum, Bounded)

data Color
  = Red
  | Orange
  | Amber
  | Yellow
  | Lime
  | Green
  | Emerald
  | Teal
  | Cyan
  | Sky
  | Blue
  | Indigo
  | Violet
  | Purple
  | Fuchsia
  | Pink
  | Rose
  | Gray
  | Slate
  | Zinc
  | Neutral
  | Stone
  deriving (Eq, Show, Enum, Bounded)

data ColorStep = I50 | I100 | I200 | I300 | I400 | I500 | I600 | I700 | I800 | I900 | I950
  deriving (Eq, Show, Enum, Bounded)

data Opacity = O0 | O10 | O20 | O30 | O40 | O50 | O60 | O70 | O80 | O90 | O100
  deriving (Eq, Show, Enum, Bounded)

useColor :: ColorUtility -> Color -> ColorStep -> Opacity -> T.Text
useColor utility color step opacity = T.concat [utility', color', step', opacity']
  where
    utility' = case utility of
      Bg -> "bg-"
      Text -> "text-"
      Decoration -> "decoration-"
      Border -> "border-"
      Outline -> "outline-"
      Shadow -> "shadow-"
      InsetShadow -> "shadow-inset-"
      Ring -> "ring-"
      InsetRing -> "ring-inset-"
      Accent -> "accent-"
      Caret -> "caret-"
      Fill -> "fill-"
      Stroke -> "stroke-"
    color' = case color of
      Red -> "red-"
      Orange -> "orange-"
      Amber -> "amber-"
      Yellow -> "yellow-"
      Lime -> "lime-"
      Green -> "green-"
      Emerald -> "emerald-"
      Teal -> "teal-"
      Cyan -> "cyan-"
      Sky -> "sky-"
      Blue -> "blue-"
      Indigo -> "indigo-"
      Violet -> "violet-"
      Purple -> "purple-"
      Fuchsia -> "fuchsia-"
      Pink -> "pink-"
      Rose -> "rose-"
      Gray -> "gray-"
      Slate -> "slate-"
      Zinc -> "zinc-"
      Neutral -> "neutral-"
      Stone -> "stone-"
    step' = case step of
      I50 -> "50"
      I100 -> "100"
      I200 -> "200"
      I300 -> "300"
      I400 -> "400"
      I500 -> "500"
      I600 -> "600"
      I700 -> "700"
      I800 -> "800"
      I900 -> "900"
      I950 -> "950"
    opacity' = case opacity of
      O0 -> "/0"
      O10 -> "/10"
      O20 -> "/20"
      O30 -> "/30"
      O40 -> "/40"
      O50 -> "/50"
      O60 -> "/60"
      O70 -> "/70"
      O80 -> "/80"
      O90 -> "/90"
      O100 -> ""
