module Competences.Frontend.View.Tailwind
  ( -- * Direct Tailwind class helpers
    class_
  , classes
    -- * Color utilities
  , tailwindColors
  , useColor
  , ColorUtility (..)
  , Color (..)
  , ColorStep (..)
  , Opacity(..)
  )
where

import Data.Text qualified as T
import Miso qualified as M
import Miso.Html.Property qualified as M

-- | Apply Tailwind classes directly using a Text string.
--   This is the recommended approach for flexibility and simplicity.
--
--   Example: class_ "flex items-center gap-2 rounded-md"
class_ :: T.Text -> M.Attribute a
class_ = M.class_ . M.ms

-- | Combine multiple class strings into a single class attribute.
--   Useful for breaking up long class lists for readability.
--
--   Example: classes ["flex items-center", "gap-2", "rounded-md"]
classes :: [T.Text] -> M.Attribute a
classes cs = class_ $ T.unwords cs

-- | Generate dynamic color classes from structured color values.
tailwindColors :: [(ColorUtility, Color, ColorStep, Opacity)] -> M.Attribute a
tailwindColors = M.class_ . M.ms . T.intercalate " " . map (\(u, c, s, o) -> useColor u c s o)

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
