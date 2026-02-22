module Competences.Frontend.Component.PrintEngine.Types
  ( PaperSize (..)
  , Orientation (..)
  , PrintSettings (..)
  , defaultPrintSettings
  , pageSizeCSS
  , pageSizeMm
  , pageMarginMm
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Supported paper sizes
data PaperSize = A4 | A5 | A6
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | Page orientation
data Orientation = Portrait | Landscape
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | Print configuration
data PrintSettings = PrintSettings
  { paperSize :: !PaperSize
  , orientation :: !Orientation
  }
  deriving (Eq, Show, Generic)

-- | Default: A5 portrait (fits well for single-task worksheets)
defaultPrintSettings :: PrintSettings
defaultPrintSettings = PrintSettings {paperSize = A5, orientation = Portrait}

-- | CSS @page size value (e.g. "A5", "A6 landscape")
pageSizeCSS :: PaperSize -> Orientation -> Text
pageSizeCSS ps Portrait = paperSizeName ps
pageSizeCSS ps Landscape = paperSizeName ps <> " landscape"

paperSizeName :: PaperSize -> Text
paperSizeName A4 = "A4"
paperSizeName A5 = "A5"
paperSizeName A6 = "A6"

-- | Page dimensions in mm (width, height), accounting for orientation
pageSizeMm :: PaperSize -> Orientation -> (Double, Double)
pageSizeMm ps Portrait = portraitDimensions ps
pageSizeMm ps Landscape = let (w, h) = portraitDimensions ps in (h, w)

portraitDimensions :: PaperSize -> (Double, Double)
portraitDimensions A4 = (210.0, 297.0)
portraitDimensions A5 = (148.0, 210.0)
portraitDimensions A6 = (105.0, 148.0)

-- | Page margin in mm (depends on paper size only)
pageMarginMm :: PaperSize -> Double
pageMarginMm A4 = 20.0
pageMarginMm A5 = 15.0
pageMarginMm A6 = 10.0
