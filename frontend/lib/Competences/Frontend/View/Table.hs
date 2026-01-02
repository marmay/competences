{- |
Module: Competences.Frontend.View.Table
Description: Basecoat-inspired table components

This module provides table components with consistent Basecoat styling.
-}
module Competences.Frontend.View.Table
  ( Table (..)
  , TableColumnWidth (..)
  , TableColumnSpec (..)
  , cellContents
  , defTable
  , viewTable
  , tableHeader_
  , tableRow
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html as M

data TableColumnWidth
  = SingleActionColumn
  | DoubleActionColumn
  | TripleActionColumn
  | AutoSizedColumn
  deriving (Eq, Show)

data TableColumnSpec = TableColumnSpec
  { width :: !TableColumnWidth
  , title :: !M.MisoString
  } deriving (Eq, Show)

data Table col row m action = Table
  { columns :: [col]
  , rows :: [row]
  , columnSpec :: col -> TableColumnSpec
  , rowContents :: [col] -> row -> M.View m action
  }

cellContents :: (row -> col -> M.View m action) -> [col] -> row -> M.View m action
cellContents perCell cols row = tableRow $ map (perCell row) cols

tableRow :: [M.View m action] -> M.View m action
tableRow cells =
  M.tr_
    [class_ "border-b border-border hover:bg-muted/50"]
    $ map (\cell -> M.td_ [class_ "px-4 py-3 text-sm"] [cell]) cells

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
        [class_ "w-full border-collapse bg-card text-sm"]
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
    viewColumnWidth :: TableColumnWidth -> M.View m action
    viewColumnWidth AutoSizedColumn = M.col_ []
    viewColumnWidth SingleActionColumn = M.col_ [class_ "w-16"]
    viewColumnWidth DoubleActionColumn = M.col_ [class_ "w-24"]
    viewColumnWidth TripleActionColumn = M.col_ [class_ "w-32"]

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
