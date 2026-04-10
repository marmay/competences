module Competences.Frontend.Component.PrintEngine.CSS
  ( printStyleView
  )
where

import Competences.Frontend.Component.PrintEngine.Types
  ( ContentSettings (..)
  , FontFamily (..)
  , GridConfig (..)
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
-- for the given print and content settings.
printStyleView :: PrintSettings -> ContentSettings -> M.View model action
printStyleView settings _cs =
  M.nodeHtml
    "style"
    [MP.type_ "text/css"]
    [M.text $ ms $ printCSS settings]

printCSS :: PrintSettings -> Text
printCSS settings = case settings.taskLayout of
  Continuous -> continuousCSS settings
  Grid gc -> gridCSS settings gc

-- | CSS font-family value for the given FontFamily setting
fontFamilyCSS :: FontFamily -> Text
fontFamilyCSS DefaultFont = ""
fontFamilyCSS IwonaFont = "font-family: 'Iwona', sans-serif;"

-- | Line-height per font family.  Iwona's generous vertical metrics
-- need a tighter value to match the default font's visual density.
lineHeightCSS :: FontFamily -> Text
lineHeightCSS DefaultFont = "1.5"
lineHeightCSS IwonaFont = "1.3"

-- | Shared visual styling: font-size, line-height, headings, task margins.
-- These rules apply on screen (preview, measurement) and in print alike.
sharedCSS :: PrintSettings -> Text
sharedCSS settings =
  let fontSize = T.pack (show settings.baseFontSize) <> "pt"
      fontFam = fontFamilyCSS settings.fontFamily
      fontRule = if T.null fontFam then "" else " " <> fontFam
      lh = lineHeightCSS settings.fontFamily
   in T.unlines
        [ ".page-print-content { font-size: " <> fontSize <> "; line-height: " <> lh <> ";" <> fontRule <> " }"
        , ".page-print-content h2 { font-size: 1em; font-weight: 600; margin-bottom: 0.2em; }"
        , ".page-print-content .print-page { display: flex; flex-direction: column; box-sizing: border-box; }"
        , ".page-print-content .print-margin-top { flex-shrink: 0; display: flex; flex-direction: column; justify-content: flex-end; }"
        , ".page-print-content .print-margin-bottom { flex-shrink: 0; display: flex; flex-direction: column; justify-content: flex-start; }"
        , ".page-print-content .print-content-area { flex: 1; display: flex; flex-direction: column; min-height: 0; }"
        , ".page-print-content .print-page-header { margin-bottom: 0.5em; text-align: center; }"
        , ".page-print-content .print-page-header-title { font-size: 1.3em; font-weight: 600; }"
        , ".page-print-content .print-page-header-date { font-size: 0.85em; color: #666; }"
        , ".page-print-content .print-page-header-compact { display: flex; justify-content: space-between; align-items: baseline; font-size: 0.85em; color: #666; border-bottom: 1px solid #ccc; padding-bottom: 0.3em; }"
        , ".page-print-content .print-page-footer { text-align: center; font-size: 0.75em; color: #999; margin-top: 0.8em; }"
        , ".page-print-content .print-name-field { font-size: 0.85em; text-align: center; margin: 1.5em 0; }"
        , ".page-print-content .print-name-field-line { display: inline-block; border-bottom: 1px solid #333; width: 60%; vertical-align: bottom; }"
        , -- Multi-column letter lists (grid for row-first flow)
          ".page-print-content .print-columns-2 ol { display: grid; grid-template-columns: repeat(2, 1fr); column-gap: 1em; }"
        , ".page-print-content .print-columns-3 ol { display: grid; grid-template-columns: repeat(3, 1fr); column-gap: 1em; }"
        , ".page-print-content .print-columns-4 ol { display: grid; grid-template-columns: repeat(4, 1fr); column-gap: 1em; }"
        , ".page-print-content .print-columns-2 ol li, .page-print-content .print-columns-3 ol li, .page-print-content .print-columns-4 ol li { margin-top: 0.25em !important; }"
        , -- Inline answer field: flex layout on <li> with ::after grid
          ".page-print-content .print-inline-answer ol { list-style: none; padding-left: 0; margin-left: 0; counter-reset: letter-counter; }"
        , ".page-print-content .print-inline-answer ol li { display: flex; align-items: center; counter-increment: letter-counter; }"
        , ".page-print-content .print-inline-answer ol li::before { content: counter(letter-counter, lower-alpha) \") \"; flex-shrink: 0; font-weight: 500; color: #57534e; margin-right: 0.25em; }"
        , ".page-print-content .print-inline-answer ol li::after { content: \"\"; flex: 1; height: 10mm; min-width: 20mm; background-image: url(\"data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='100%25' height='100%25'><defs><pattern id='g' patternUnits='userSpaceOnUse' width='5mm' height='5mm' x='2.5mm' y='2.5mm'><line x1='2.5mm' y1='0' x2='2.5mm' y2='5mm' stroke='%23ccc' stroke-width='0.1mm'/><line x1='0' y1='2.5mm' x2='5mm' y2='2.5mm' stroke='%23ccc' stroke-width='0.1mm'/></pattern></defs><rect width='100%25' height='100%25' fill='url(%23g)'/></svg>\"); background-size: round(down, 100%, 5mm) 100%; background-position: right top; border: 0.1mm solid #ccc; border-left: none; print-color-adjust: exact; -webkit-print-color-adjust: exact; image-rendering: crisp-edges; margin-left: 0.5em; }"
        , -- Points display
          ".page-print-content .print-task-points { float: right; font-size: 0.85em; color: #666; font-weight: normal; }"
        , -- Per-image print layout: override flex parent when it contains a floated image
          ".page-print-content .flex:has(> .print-image-float-right) { display: block !important; }"
        , -- Contain floats within print tasks
          ".page-print-content .print-task { overflow: hidden; }"
        , -- Pure black text for print (overrides prose-stone / text-stone-800)
          ".page-print-content { color: black; }"
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
