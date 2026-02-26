-- |
-- Module      : Competences.Markdown.Geometry.Palette
-- Description : Named color palettes for the geometry DSL
--
-- Curated (stroke, fill) color pairs using Tailwind CSS variables.
-- Stroke colors use the @-600@ shade; fill colors use the @-100@ shade.
-- Unknown names are rejected at parse time; internal names (\"gray\",
-- \"lightgray\") bypass the parser and fall through as raw CSS.
module Competences.Markdown.Geometry.Palette
  ( paletteMap
  , paletteNames
  , resolveStrokeColor
  , resolveFillColor
  )
where

import Competences.Markdown.Geometry.AST (Color (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)

-- | Palette entries: name -> (strokeHex, fillHex)
--
-- Stroke/text colors use the -600 shade; fill colors use the -100 shade.
-- Hex values work in both SVG attributes and LaTeX @\\color{}@, so we use
-- a single color map for both purposes.
paletteMap :: Map Text (Text, Text)
paletteMap =
  Map.fromList
    [ ("red", ("#dc2626", "#fee2e2")) -- red-600 / red-100
    , ("blue", ("#2563eb", "#dbeafe")) -- blue-600 / blue-100
    , ("green", ("#16a34a", "#dcfce7")) -- green-600 / green-100
    , ("orange", ("#ea580c", "#ffedd5")) -- orange-600 / orange-100
    , ("purple", ("#9333ea", "#f3e8ff")) -- purple-600 / purple-100
    ]

-- | Set of valid palette color names (for parser validation).
paletteNames :: Set Text
paletteNames = Map.keysSet paletteMap

-- | Resolve a 'Color' to a CSS string for strokes and text.
-- Palette entries map to the @-600@ shade; non-palette names pass through raw.
resolveStrokeColor :: Color -> Text
resolveStrokeColor CurrentColor = "currentColor"
resolveStrokeColor (NamedColor name) =
  case Map.lookup name paletteMap of
    Just (stroke, _) -> stroke
    Nothing -> name

-- | Resolve a 'Color' to a CSS string for fills.
-- Palette entries map to the @-100@ shade; non-palette names pass through raw.
resolveFillColor :: Color -> Text
resolveFillColor CurrentColor = "currentColor"
resolveFillColor (NamedColor name) =
  case Map.lookup name paletteMap of
    Just (_, fill) -> fill
    Nothing -> name
