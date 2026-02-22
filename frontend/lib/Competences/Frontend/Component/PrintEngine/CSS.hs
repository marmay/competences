module Competences.Frontend.Component.PrintEngine.CSS
  ( printStyleView
  )
where

import Competences.Frontend.Component.PrintEngine.Types
  ( PrintSettings (..)
  , pageSizeCSS
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
printCSS settings =
  let size = pageSizeCSS settings.paperSize settings.orientation
      margin = T.pack $ show (pageMarginMm settings.paperSize) <> "mm"
   in T.unlines
        [ "@page { size: " <> size <> "; margin: " <> margin <> "; }"
        , "@media print {"
        , "  .page-print-content { display: block !important; }"
        , "  .page-print-content .print-task { break-after: page; }"
        , "  .page-print-content .print-task:last-child { break-after: auto; }"
        , "  .page-print-content { font-size: 11pt; line-height: 1.5; }"
        , "  .page-print-content .geometry-scene { max-width: 100% !important; height: auto !important; }"
        , "}"
        ]
