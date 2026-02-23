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
        , ".page-print-content h2 { font-size: 1em; font-weight: 600; margin-bottom: 0.2em; }"
        , ".page-print-content .print-page { display: flex; flex-direction: column; box-sizing: border-box; }"
        , ".page-print-content .print-margin-top { flex-shrink: 0; display: flex; flex-direction: column; justify-content: flex-end; }"
        , ".page-print-content .print-margin-bottom { flex-shrink: 0; display: flex; flex-direction: column; justify-content: flex-start; }"
        , ".page-print-content .print-content-area { flex: 1; display: flex; flex-direction: column; min-height: 0; }"
        , ".page-print-content .print-page-header { margin-bottom: 0.5em; text-align: center; }"
        , ".page-print-content .print-page-header-title { font-size: 1.3em; font-weight: 600; }"
        , ".page-print-content .print-page-header-date { font-size: 0.85em; color: #666; }"
        , ".page-print-content .print-page-header-compact { display: flex; justify-content: space-between; align-items: baseline; font-size: 0.85em; color: #666; border-bottom: 1px solid #ccc; padding-bottom: 0.3em; }"
        , ".page-print-content .print-page-footer { text-align: center; font-size: 0.75em; color: #999; }"
        , ".page-print-content .print-name-field { font-size: 0.85em; text-align: center; margin: 1.5em 0; }"
        , ".page-print-content .print-name-field-line { display: inline-block; border-bottom: 1px solid #333; width: 60%; vertical-align: bottom; }"
        ]

-- | Continuous mode: zero-margin @page, explicit page dimensions and structural margins.
-- Each .print-page is full paper size; margin areas hold header/footer;
-- the content area fills the middle.
continuousCSS :: PrintSettings -> Text
continuousCSS settings =
  let size = pageSizeCSS settings.paperSize settings.orientation
      (pw, ph) = pageSizeMm settings.paperSize settings.orientation
      margin = pageMarginMm settings.paperSize
   in T.unlines
        [ sharedCSS settings
        , "@page { size: " <> size <> "; margin: 0; }"
        , "@media print {"
        , "  .page-print-content { display: block !important; overflow: hidden; }"
        , "  .page-print-content .print-page {"
        , "    width: " <> showMm pw <> ";"
        , "    height: " <> showMm ph <> ";"
        , "    padding-left: " <> showMm margin <> ";"
        , "    padding-right: " <> showMm margin <> ";"
        , "    break-after: page;"
        , "  }"
        , "  .page-print-content .print-page:last-child { break-after: avoid; }"
        , "  .page-print-content .print-margin-top {"
        , "    height: " <> showMm margin <> ";"
        , "  }"
        , "  .page-print-content .print-margin-bottom {"
        , "    height: " <> showMm margin <> ";"
        , "  }"
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
        , "  .page-print-content { display: block !important; overflow: hidden; }"
        , "  .page-print-content .print-page {"
        , "    width: " <> showMm pw <> ";"
        , "    height: " <> showMm ph <> ";"
        , "    display: grid;"
        , "    grid-template-columns: repeat(" <> cols <> ", 1fr);"
        , "    grid-template-rows: repeat(" <> rows <> ", 1fr);"
        , "    overflow: hidden;"
        , "  }"
        , "  .page-print-content .print-page:not(:first-child) { break-before: page; }"
        , "  .page-print-content .print-page:last-child { break-after: avoid; }"
        , "  .page-print-content .geometry-scene { max-width: 100% !important; height: auto !important; }"
        , "}"
        ]

showMm :: Double -> Text
showMm d = T.pack (show d) <> "mm"
