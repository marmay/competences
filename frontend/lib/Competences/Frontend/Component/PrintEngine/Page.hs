module Competences.Frontend.Component.PrintEngine.Page
  ( renderContinuousPage
  , renderGridPage
  , renderFirstPageHeader
  , renderCompactHeader
  , renderPageFooter
  , renderNameField
  , duplexMargins
  , showPx
  , showMmAttr
  )
where

import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.PrintEngine.Measure (PageGroup (..))
import Competences.Frontend.Component.PrintEngine.Types
  ( ContentSettings (..)
  , GridConfig (..)
  , Orientation
  , PaperSize
  , PrintSettings (..)
  , cellsPerPage
  , pageMarginMm
  , pageSizeMm
  )
import Competences.Frontend.View.Tailwind (class_)
import Data.Text qualified as T
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as M
import Miso.String (MisoString, ms)

-- | Compute left and right padding for duplex layouts.
-- Even page indices (0-based) get wider left margin (odd pages in 1-based),
-- odd indices get wider right margin.
duplexMargins :: Bool -> Double -> Int -> (Double, Double)
duplexMargins isDuplex margin pageIdx
  | isDuplex, even pageIdx = (margin * 1.5, margin * 0.75)
  | isDuplex = (margin * 0.75, margin * 1.5)
  | otherwise = (margin, margin)

-- | Render a continuous-mode page with mm-based dimensions.
-- The 3-section layout (margin-top, content-area, margin-bottom) handles
-- header/footer placement, title, name field, and custom footer.
-- The @renderTask@ callback receives an expanded-task index and should
-- return a view wrapped in a @.print-task@ container.
renderContinuousPage
  :: PrintSettings
  -> ContentSettings
  -> MisoString
  -> MisoString
  -> Int
  -> (Int -> M.View model action)
  -> Maybe (M.View model action)
  -> Int
  -> PageGroup
  -> M.View model action
renderContinuousPage settings cs title date totalPages renderTask mCustomFooter pageIdx pg =
  let isFirst = pageIdx == 0
      (pw, ph) = pageSizeMm settings.paperSize settings.orientation
      margin = pageMarginMm settings.paperSize
      (padLeft, padRight) = duplexMargins settings.duplexLayout margin pageIdx
      marginStyle = MC.style_ [("height", showMmAttr margin)]
      pageStyle =
        MC.style_
          [ ("width", showMmAttr pw)
          , ("height", showMmAttr ph)
          , ("padding-left", showMmAttr padLeft)
          , ("padding-right", showMmAttr padRight)
          ]
      marginTopContent
        | isFirst && cs.showTitle = []
        | not settings.showHeader = []
        | otherwise = [renderCompactHeader title date]
      firstPageTitleView
        | cs.showTitle && isFirst = [renderFirstPageHeader title date]
        | otherwise = []
      nameView
        | cs.showNameField && isFirst = [renderNameField]
        | otherwise = []
      isLastPage = pageIdx == totalPages - 1
      hasCustomFooter = case mCustomFooter of Just _ -> True; Nothing -> False
      marginBottomContent
        | isLastPage, hasCustomFooter = []
        | settings.showFooter = [renderPageFooter (pageIdx + 1) totalPages]
        | otherwise = []
      footerView = case mCustomFooter of
        Just v | isLastPage -> [v]
        _ -> []
   in M.div_
        [class_ "print-page", pageStyle]
        [ M.div_ [class_ "print-margin-top", marginStyle] marginTopContent
        , M.div_
            [class_ "print-content-area"]
            ( firstPageTitleView
                <> nameView
                <> [ M.div_
                       [ class_ "flex flex-col"
                       , MC.style_ [("gap", showPx pg.gapPx)]
                       ]
                       [renderTask idx | idx <- pg.indices]
                   ]
                <> footerView
            )
        , M.div_ [class_ "print-margin-bottom", marginStyle] marginBottomContent
        ]

-- | Render a grid-mode page with inline grid layout and mm-based dimensions.
-- The @renderTask@ callback should wrap each task in a @.print-cell@ container.
-- Empty cells are appended to fill the grid.
renderGridPage
  :: PaperSize
  -> Orientation
  -> GridConfig
  -> (Int -> M.View model action)
  -> [Int]
  -> M.View model action
renderGridPage ps orient gc renderTask indices =
  let (pw, ph) = pageSizeMm ps orient
      cpp = cellsPerPage gc
      cells =
        map renderTask indices
          <> replicate (cpp - length indices) (M.div_ [class_ "print-cell"] [])
   in M.div_
        [ class_ "print-page"
        , MC.style_
            [ ("width", showMmAttr pw)
            , ("height", showMmAttr ph)
            , ("display", "grid")
            , ("grid-template-columns", ms $ "repeat(" <> T.pack (show gc.cols) <> ", 1fr)")
            , ("grid-template-rows", ms $ "repeat(" <> T.pack (show gc.rows) <> ", 1fr)")
            ]
        ]
        cells

-- | First-page header: large title + date below
renderFirstPageHeader :: MisoString -> MisoString -> M.View model action
renderFirstPageHeader title date =
  M.div_
    [class_ "print-page-header"]
    [ M.div_ [class_ "print-page-header-title"] [M.text title]
    , M.div_ [class_ "print-page-header-date"] [M.text date]
    ]

-- | Compact header: title + date on same line
renderCompactHeader :: MisoString -> MisoString -> M.View model action
renderCompactHeader title date =
  M.div_
    [class_ "print-page-header-compact"]
    [ M.span_ [] [M.text title]
    , M.span_ [] [M.text date]
    ]

-- | Page footer: centered page number "X / Y"
renderPageFooter :: Int -> Int -> M.View model action
renderPageFooter pageNum totalPages =
  M.div_
    [class_ "print-page-footer"]
    [M.text $ ms (show pageNum) <> " / " <> ms (show totalPages)]

-- | Name field: "Name: ________________"
renderNameField :: M.View model action
renderNameField =
  M.div_
    [class_ "print-name-field"]
    [ M.text $ C.translate' C.LblStudentName <> ": "
    , M.span_ [class_ "print-name-field-line"] [M.text "\xA0"]
    ]

-- | Format a Double as CSS px value
showPx :: Double -> MisoString
showPx d = ms (show (round d :: Int)) <> "px"

-- | Format a Double as CSS mm value
showMmAttr :: Double -> MisoString
showMmAttr d = ms (show d <> "mm")
