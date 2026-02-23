module Competences.Frontend.Component.PrintEngine.Types
  ( PaperSize (..)
  , Orientation (..)
  , TaskLayout (..)
  , TaskHeaderStyle (..)
  , GridConfig (..)
  , PrintSettings (..)
  , defaultPrintSettings
  , pageSizeCSS
  , pageSizeMm
  , pageMarginMm
  , cellsPerPage
  , cellSizeMm
  , expandTaskSequence
  , chunksOf
  , taskNumFromIdx
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Supported paper sizes
data PaperSize = A4 | A5
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | Page orientation
data Orientation = Portrait | Landscape
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | How the task header (h2) is rendered
data TaskHeaderStyle = HeaderNumber | HeaderTitle | HeaderBoth
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | How tasks are laid out on the page
data TaskLayout
  = -- | Tasks flow continuously, no forced page breaks
    Continuous
  | -- | Tasks arranged in a grid (rows x cols per page)
    Grid !GridConfig
  deriving (Eq, Show, Generic)

-- | Grid dimensions (rows and columns per page)
data GridConfig = GridConfig
  { rows :: !Int
  , cols :: !Int
  }
  deriving (Eq, Show, Generic)

-- | Print configuration
data PrintSettings = PrintSettings
  { paperSize :: !PaperSize
  , orientation :: !Orientation
  , baseFontSize :: !Double
  , taskLayout :: !TaskLayout
  , groupedCopies :: !Int
  , totalCopies :: !Int
  , showTitle :: !Bool
  , showHeader :: !Bool
  , showFooter :: !Bool
  , showNameField :: !Bool
  , taskHeaderStyle :: !TaskHeaderStyle
  }
  deriving (Eq, Show, Generic)

-- | Default: A4 portrait, continuous layout, 1 copy each
defaultPrintSettings :: PrintSettings
defaultPrintSettings =
  PrintSettings
    { paperSize = A4
    , orientation = Portrait
    , baseFontSize = 10.0
    , taskLayout = Continuous
    , groupedCopies = 1
    , totalCopies = 1
    , showTitle = True
    , showHeader = True
    , showFooter = True
    , showNameField = True
    , taskHeaderStyle = HeaderBoth
    }

-- | CSS @page size value (e.g. "A5", "A4 landscape")
pageSizeCSS :: PaperSize -> Orientation -> Text
pageSizeCSS ps Portrait = paperSizeName ps
pageSizeCSS ps Landscape = paperSizeName ps <> " landscape"

paperSizeName :: PaperSize -> Text
paperSizeName A4 = "A4"
paperSizeName A5 = "A5"

-- | Page dimensions in mm (width, height), accounting for orientation
pageSizeMm :: PaperSize -> Orientation -> (Double, Double)
pageSizeMm ps Portrait = portraitDimensions ps
pageSizeMm ps Landscape = let (w, h) = portraitDimensions ps in (h, w)

portraitDimensions :: PaperSize -> (Double, Double)
portraitDimensions A4 = (210.0, 297.0)
portraitDimensions A5 = (148.0, 210.0)

-- | Page margin in mm (depends on paper size only)
pageMarginMm :: PaperSize -> Double
pageMarginMm A4 = 20.0
pageMarginMm A5 = 15.0

-- | Number of cells (tasks) per page in grid mode
cellsPerPage :: GridConfig -> Int
cellsPerPage gc = gc.rows * gc.cols

-- | Cell size in mm (width, height) for a grid layout.
-- Each cell is (pageWidth / cols, pageHeight / rows).
cellSizeMm :: PaperSize -> Orientation -> GridConfig -> (Double, Double)
cellSizeMm ps orient gc =
  let (pw, ph) = pageSizeMm ps orient
   in (pw / fromIntegral gc.cols, ph / fromIntegral gc.rows)

-- | Expand a task sequence according to grouped and total copies.
-- Given tasks [A,B,C], groupedCopies=2, totalCopies=2:
-- Result: [A,A,B,B,C,C, A,A,B,B,C,C]
expandTaskSequence :: Int -> Int -> [a] -> [a]
expandTaskSequence grouped total tasks =
  let gc = max 1 grouped
      tc = max 1 total
      groupExpanded = concatMap (replicate gc) tasks
   in concat (replicate tc groupExpanded)

-- | Split a list into chunks of at most n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let n' = max 1 n
      (chunk, rest) = splitAt n' xs
   in chunk : chunksOf n' rest

-- | Compute the 1-based original task number from an expanded index.
-- With groupedCopies=2 and tasks [A,B,C], expanded is [A,A,B,B,C,C,...].
-- Both copies of task A get number 1, both copies of B get number 2, etc.
taskNumFromIdx :: Int -> Int -> Int -> Int
taskNumFromIdx groupedCopies originalCount idx =
  let gc = max 1 groupedCopies
   in (idx `mod` (originalCount * gc)) `div` gc + 1

