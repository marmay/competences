{- |
Module: Competences.Frontend.View.Table
Description: Basecoat-inspired table components

This module provides table components with consistent Basecoat styling.
-}
module Competences.Frontend.View.Table
  ( Table (..)
  , TableColumnWidth (..)
  , TableColumnSpec (..)
  , TableCellSpec (..)
  , cellContents
  , cellContentsWithSpec
  , defCellSpec
  , defTable
  , viewTable
  , tableHeader_
  , tableRow
  , tableRowWithSpec
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html as M

data TableColumnWidth
  = SingleActionColumn
  | DoubleActionColumn
  | TripleActionColumn
  | AutoSizedColumn
  | EqualWidthColumn
  -- ^ Equal width column - shares remaining space equally with other
  -- EqualWidthColumn columns. When used, table switches to fixed layout.
  deriving (Eq, Show)

data TableColumnSpec = TableColumnSpec
  { width :: !TableColumnWidth
  , title :: !M.MisoString
  } deriving (Eq, Show)

-- | Specification for a single table cell including styling
data TableCellSpec m action = TableCellSpec
  { cellClasses :: !Text
  -- ^ Additional CSS classes for the td element
  , cellStyle :: ![(M.MisoString, M.MisoString)]
  -- ^ Inline styles for the td element
  , cellContent :: M.View m action
  -- ^ The content to render inside the td
  }

-- | Create a default cell spec with just content (no extra classes/styles)
defCellSpec :: M.View m action -> TableCellSpec m action
defCellSpec = TableCellSpec T.empty []

data Table col row m action = Table
  { columns :: [col]
  , rows :: [row]
  , columnSpec :: col -> TableColumnSpec
  , rowContents :: [col] -> row -> M.View m action
  }

cellContents :: (row -> col -> M.View m action) -> [col] -> row -> M.View m action
cellContents perCell cols row = tableRow $ map (perCell row) cols

-- | Like cellContents but allows per-cell styling on the td element
cellContentsWithSpec :: (row -> col -> TableCellSpec m action) -> [col] -> row -> M.View m action
cellContentsWithSpec perCell cols row = tableRowWithSpec $ map (perCell row) cols

tableRow :: [M.View m action] -> M.View m action
tableRow cells =
  M.tr_
    [class_ "border-b border-border"]
    -- No padding on td - let cell content control full styling via wrapper div
    $ map (\cell -> M.td_ [class_ "text-sm align-middle"] [cell]) cells

-- | Like tableRow but with per-cell styling support
tableRowWithSpec :: [TableCellSpec m action] -> M.View m action
tableRowWithSpec cells =
  M.tr_
    [class_ "border-b border-border"]
    $ map renderCell cells
  where
    renderCell spec =
      let baseClasses = "text-sm align-middle" :: Text
          allClasses = if T.null spec.cellClasses
                       then baseClasses
                       else baseClasses <> " " <> spec.cellClasses
          attrs = [class_ allClasses]
                  <> if null spec.cellStyle then [] else [MC.style_ spec.cellStyle]
       in M.td_ attrs [spec.cellContent]

defTable :: Table col row m action
defTable =
  Table
    { columns = []
    , rows = []
    , columnSpec = const (TableColumnSpec AutoSizedColumn "")
    , rowContents = cellContents (\_ _ -> M.text_ [])
    }

viewTable :: forall col row m action. Table col row m action -> M.View m action
viewTable t =
  M.div_
    [class_ "overflow-x-auto rounded-lg border border-border"]
    [ M.table_
        [class_ $ "w-full border-collapse bg-card text-sm" <> tableLayoutClass]
        [ M.colgroup_
            []
            $ map
              (viewColumnWidth . (.width) . t.columnSpec)
              t.columns
        , M.thead_
            [class_ "bg-muted/50"]
            [ M.tr_
                []
                $ map
                  (viewColumnHeader . (.title) . t.columnSpec)
                  t.columns
            ]
        , M.tbody_ [] $ map (t.rowContents t.columns) t.rows
        ]
    ]
  where
    -- Use table-fixed layout if any column uses EqualWidthColumn
    hasEqualWidth = any ((== EqualWidthColumn) . (.width) . t.columnSpec) t.columns
    tableLayoutClass = if hasEqualWidth then " table-fixed" else ""

    viewColumnWidth :: TableColumnWidth -> M.View m action
    viewColumnWidth AutoSizedColumn = M.col_ []
    viewColumnWidth SingleActionColumn = M.col_ [class_ "w-16"]
    viewColumnWidth DoubleActionColumn = M.col_ [class_ "w-24"]
    viewColumnWidth TripleActionColumn = M.col_ [class_ "w-32"]
    viewColumnWidth EqualWidthColumn = M.col_ [] -- shares remaining space equally

    viewColumnHeader :: M.MisoString -> M.View m action
    viewColumnHeader col =
      M.th_
        [class_ "px-4 py-3 text-left text-xs font-semibold text-foreground border-b border-border"]
        [M.text_ [col]]

tableHeader_ :: M.MisoString -> M.View m action
tableHeader_ header =
  M.th_
    [class_ "px-4 py-3 text-left text-xs font-semibold text-foreground"]
    [M.text_ [header]]
