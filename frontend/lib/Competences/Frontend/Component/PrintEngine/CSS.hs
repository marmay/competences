module Competences.Frontend.Component.PrintEngine.CSS
  ( printStyleView
  )
where

import Competences.Frontend.Component.PrintEngine.Types
  ( GridConfig (..)
  , PrintSettings (..)
  , TaskLayout (..)
  , pageSizeCSS
  , pageSizeMm
  , pageMarginMm
  )
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (ms)

-- | Generate a <style> element with @page and print media rules
-- for the given print settings.
printStyleView :: PrintSettings -> M.View model action
printStyleView settings =
  M.nodeHtml
    "style"
    [MP.type_ "text/css"]
    [M.text $ ms $ printCSS settings]

printCSS :: PrintSettings -> Text
printCSS settings = case settings.taskLayout of
  Continuous -> continuousCSS settings
  Grid gc -> gridCSS settings gc

-- | Shared visual styling: font-size, line-height, headings, task margins.
-- These rules apply on screen (preview, measurement) and in print alike.
sharedCSS :: PrintSettings -> Text
sharedCSS settings =
  let fontSize = T.pack (show settings.baseFontSize) <> "pt"
   in T.unlines
        [ ".page-print-content { font-size: " <> fontSize <> "; line-height: 1.5; }"
        , ".page-print-content h2 { font-size: 1.3em; font-weight: 600; margin-bottom: 0.3em; }"
        , ".page-print-content .print-page { display: flex; flex-direction: column; }"
        ]

-- | Continuous mode: normal @page with margins, tasks flow naturally
continuousCSS :: PrintSettings -> Text
continuousCSS settings =
  let size = pageSizeCSS settings.paperSize settings.orientation
      margin = showMm (pageMarginMm settings.paperSize)
   in T.unlines
        [ sharedCSS settings
        , "@page { size: " <> size <> "; margin: " <> margin <> "; }"
        , "@media print {"
        , "  .page-print-content { display: block !important; }"
        , "  .page-print-content .print-page { break-after: page; }"
        , "  .page-print-content .print-page:last-child { break-after: auto; }"
        , "  .page-print-content .geometry-scene { max-width: 100% !important; height: auto !important; }"
        , "}"
        ]

-- | Grid mode: zero-margin @page, CSS grid per page, each cell padded
gridCSS :: PrintSettings -> GridConfig -> Text
gridCSS settings gc =
  let size = pageSizeCSS settings.paperSize settings.orientation
      (pw, ph) = pageSizeMm settings.paperSize settings.orientation
      cellPad = showMm (pageMarginMm settings.paperSize)
      cols = T.pack (show gc.cols)
      rows = T.pack (show gc.rows)
   in T.unlines
        [ sharedCSS settings
        , ".page-print-content .print-cell {"
        , "  padding: " <> cellPad <> ";"
        , "  overflow: hidden;"
        , "}"
        , "@page { size: " <> size <> "; margin: 0; }"
        , "@media print {"
        , "  .page-print-content { display: block !important; }"
        , "  .page-print-content .print-page {"
        , "    width: " <> showMm pw <> ";"
        , "    height: " <> showMm ph <> ";"
        , "    display: grid;"
        , "    grid-template-columns: repeat(" <> cols <> ", 1fr);"
        , "    grid-template-rows: repeat(" <> rows <> ", 1fr);"
        , "    break-after: page;"
        , "    overflow: hidden;"
        , "  }"
        , "  .page-print-content .print-page:last-child { break-after: auto; }"
        , "  .page-print-content .geometry-scene { max-width: 100% !important; height: auto !important; }"
        , "}"
        ]

showMm :: Double -> Text
showMm d = T.pack (show d) <> "mm"
