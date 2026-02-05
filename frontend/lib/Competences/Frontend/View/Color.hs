module Competences.Frontend.View.Color
  ( ColorPalette (..)
  , green
  , lime
  , yellow
  , red
  )
where

import Data.Text (Text)

data ColorPalette = ColorPalette
  { background :: !Text -- e.g. "bg-green-100"
  , foreground :: !Text -- e.g. "text-green-800"
  , border :: !Text -- e.g. "border-green-300"
  }

green, lime, yellow, red :: ColorPalette
green = ColorPalette "bg-green-100" "text-green-800" "border-green-300"
lime = ColorPalette "bg-lime-100" "text-lime-800" "border-lime-300"
yellow = ColorPalette "bg-yellow-100" "text-yellow-800" "border-yellow-300"
red = ColorPalette "bg-red-100" "text-red-800" "border-red-300"
