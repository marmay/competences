module Competences.Frontend.Component.PrintEngine.Types
  ( PageSize (..)
  , PrintSettings (..)
  , defaultPrintSettings
  , pageSizeCSS
  , pageSizeMm
  , pageMarginMm
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Supported page sizes for the print engine
data PageSize = A5Portrait | A4Portrait
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | Print configuration
data PrintSettings = PrintSettings
  { pageSize :: !PageSize
  }
  deriving (Eq, Show, Generic)

-- | Default: A5 portrait (fits well for single-task worksheets)
defaultPrintSettings :: PrintSettings
defaultPrintSettings = PrintSettings {pageSize = A5Portrait}

-- | CSS @page size value
pageSizeCSS :: PageSize -> Text
pageSizeCSS A5Portrait = "A5"
pageSizeCSS A4Portrait = "A4"

-- | Page dimensions in mm (width, height)
pageSizeMm :: PageSize -> (Double, Double)
pageSizeMm A5Portrait = (148.0, 210.0)
pageSizeMm A4Portrait = (210.0, 297.0)

-- | Page margin in mm
pageMarginMm :: PageSize -> Double
pageMarginMm A5Portrait = 15.0
pageMarginMm A4Portrait = 20.0
