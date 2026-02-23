module Competences.Frontend.Component.PrintEngine.Measure
  ( PageGroup (..)
  , PageGrouping
  , groupIntoPages
  , measureTaskHeights
  , contentHeightPx
  )
where

import Competences.Frontend.Component.PrintEngine.Types
  ( Orientation
  , PaperSize
  , pageMarginMm
  , pageSizeMm
  )
import Miso.DSL (JSVal, fromJSVal, isNull, jsg, toJSVal, (!), (#))
import Miso.String (MisoString)

-- | A page of tasks with their indices and the gap between them.
data PageGroup = PageGroup
  { indices :: ![Int]
  , gapPx :: !Double
  }
  deriving (Eq, Show)

-- | Each element describes one page: which tasks and how to space them.
type PageGrouping = [PageGroup]

-- | Greedy bin-packing with adjustable gaps.
--
-- Task heights are pure content (no spacing). The algorithm uses @minGap@
-- between tasks when deciding whether the next task fits. Once a page is
-- complete, the actual gap is stretched to distribute remaining space
-- evenly between tasks.
--
-- Invariant: at least one task per page (handles tasks taller than a page).
groupIntoPages :: Double -> Double -> [Double] -> PageGrouping
groupIntoPages _ _ [] = []
groupIntoPages avail minGap heights = go 0.0 [] (zip [0 ..] heights)
  where
    go :: Double -> [(Int, Double)] -> [(Int, Double)] -> PageGrouping
    go _ acc [] = [finishPage avail acc]
    go used acc ((idx, h) : rest)
      -- First task on the page: always place it
      | null acc = go h [(idx, h)] rest
      -- Fits with minimum gap
      | used + minGap + h <= avail = go (used + minGap + h) ((idx, h) : acc) rest
      -- Doesn't fit: close this page, start a new one
      | otherwise = finishPage avail acc : go h [(idx, h)] rest

    -- Build a PageGroup, computing the actual gap that fills the page.
    finishPage :: Double -> [(Int, Double)] -> PageGroup
    finishPage pageAvail revAcc =
      let acc = reverse revAcc
          idxs = map fst acc
          taskHeights = map snd acc
          totalContent = sum taskHeights
          n = length idxs
          gap
            | n <= 1 = 0.0
            | otherwise =
                let remaining = pageAvail - totalContent
                    gaps = fromIntegral (n - 1)
                 in max minGap (remaining / gaps)
       in PageGroup {indices = idxs, gapPx = gap}

-- | Available content height in CSS px for a given paper size and orientation.
-- Subtracts top + bottom margins, then converts from mm to px at 96 DPI.
contentHeightPx :: PaperSize -> Orientation -> Double
contentHeightPx ps orient =
  let (_w, h) = pageSizeMm ps orient
      margin = pageMarginMm ps
      contentMm = h - 2.0 * margin
   in contentMm * 96.0 / 25.4

-- | Read rendered heights of all direct children of the measurement container.
-- Tasks are rendered without spacing so the returned heights are pure content.
-- Returns an empty list if the container is not found.
measureTaskHeights :: IO [Double]
measureTaskHeights = do
  doc <- jsg ("document" :: MisoString)
  container <- doc # ("getElementById" :: MisoString) $ [toJSVal ("print-measure-container" :: MisoString)]
  containerIsNull <- isNull container
  if containerIsNull
    then pure []
    else do
      children <- container ! ("children" :: MisoString)
      len <- children ! ("length" :: MisoString) >>= fromJSVal @Int
      case len of
        Nothing -> pure []
        Just n -> mapM (childHeight children) [0 .. n - 1]

-- | Get the rendered height of a single child element via getBoundingClientRect().
childHeight :: JSVal -> Int -> IO Double
childHeight children idx = do
  child <- children # ("item" :: MisoString) $ [toJSVal idx]
  rect <- child # ("getBoundingClientRect" :: MisoString) $ ([] :: [MisoString])
  mh <- rect ! ("height" :: MisoString) >>= fromJSVal @Double
  pure (maybe 0.0 id mh)
