module Competences.Frontend.Component.PrintEngine.Measure
  ( PageGroup (..)
  , PageGrouping
  , groupIntoPages
  , measureTaskHeights
  , measureFooterHeight
  , adjustForFooter
  , contentHeightPx
  , nameFieldPx
  , firstPageHeaderPx
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
-- @firstPageAvail@ is the available height for the first page (may differ
-- from subsequent pages due to headers/name fields). @restAvail@ is used
-- for all subsequent pages.
--
-- When @distributeLastPage@ is False, the last page uses @minGap@ instead
-- of stretching the gap to fill the page.
--
-- Invariant: at least one task per page (handles tasks taller than a page).
groupIntoPages :: Double -> Double -> Double -> Bool -> [Double] -> PageGrouping
groupIntoPages _ _ _ _ [] = []
groupIntoPages firstPageAvail restAvail minGap distributeLastPage heights =
  let pages = go True 0.0 [] (zip [0 ..] heights)
   in if distributeLastPage
        then pages
        else case pages of
          [] -> []
          _ -> init pages <> [useMinGapForPage (last pages)]
  where
    go :: Bool -> Double -> [(Int, Double)] -> [(Int, Double)] -> PageGrouping
    go _ _ acc [] = [finishPage (currentAvail (null acc)) acc]
      where
        -- If acc is empty we're on the first page still
        currentAvail True = firstPageAvail
        currentAvail False = if length acc == length heights then firstPageAvail else restAvail
    go isFirst used acc ((idx, h) : rest)
      -- First task on the page: always place it
      | null acc = go isFirst h [(idx, h)] rest
      -- Fits with minimum gap
      | used + minGap + h <= avail = go isFirst (used + minGap + h) ((idx, h) : acc) rest
      -- Doesn't fit: close this page, start a new one
      | otherwise = finishPage avail acc : go False h [(idx, h)] rest
      where
        avail = if isFirst then firstPageAvail else restAvail

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

    -- Replace the gap with minGap for a page (used for last page when not distributing)
    useMinGapForPage :: PageGroup -> PageGroup
    useMinGapForPage pg
      | length pg.indices <= 1 = pg
      | otherwise = pg {gapPx = minGap}

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

-- | Estimated height of the name field in CSS px.
-- Label + underline + 1em top/bottom margin = ~4 lines.
nameFieldPx :: Double -> Double
nameFieldPx fontSizePt = 4.0 * fontSizePt * 96.0 / 72.0

-- | Estimated height of the first-page title in CSS px.
-- Title (1.3em) + date (0.85em) + margin-bottom (0.5em) ≈ 4 lines.
firstPageHeaderPx :: Double -> Double
firstPageHeaderPx fontSizePt = 4.0 * fontSizePt * 96.0 / 72.0

-- | Read the rendered height of the footer measurement container.
-- Returns 0.0 if the element is not found.
measureFooterHeight :: IO Double
measureFooterHeight = do
  doc <- jsg ("document" :: MisoString)
  el <- doc # ("getElementById" :: MisoString) $ [toJSVal ("print-footer-measure" :: MisoString)]
  elIsNull <- isNull el
  if elIsNull
    then pure 0.0
    else do
      rect <- el # ("getBoundingClientRect" :: MisoString) $ ([] :: [MisoString])
      mh <- rect ! ("height" :: MisoString) >>= fromJSVal @Double
      pure (maybe 0.0 id mh)

-- | Adjust page grouping to reserve space on the last page for the custom footer.
-- If the footer doesn't fit, tasks are moved to additional pages.
adjustForFooter :: Double -> Double -> Double -> Double -> Bool -> PageGrouping -> [Double] -> PageGrouping
adjustForFooter footerH firstAvail restAvail minGap distLast pages taskHeights
  | footerH <= 0 = pages
  | null pages = pages
  | otherwise =
      let initPages = init pages
          lastPg = last pages
          -- Available height on the last page minus footer and a gap
          isOnlyPage = null initPages
          pageAvail = (if isOnlyPage then firstAvail else restAvail) - footerH - minGap
          -- Get heights for the last page's tasks
          lastTaskHeights = [taskHeights !! i | i <- lastPg.indices, i < length taskHeights]
          -- Re-group just the last page's tasks with reduced available
          reGrouped = groupIntoPages pageAvail pageAvail minGap distLast lastTaskHeights
          -- Re-map indices back to original
          reMapped = map (\pg -> pg {indices = map (\localIdx -> lastPg.indices !! localIdx) pg.indices}) reGrouped
       in initPages <> reMapped
