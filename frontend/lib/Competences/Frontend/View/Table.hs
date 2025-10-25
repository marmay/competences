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

import Competences.Frontend.View.Tailwind qualified as T
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
  M.tr_ [T.tailwind []] $ map (\cell -> M.td_ [T.tailwind [T.RegularBorder]] [cell]) cells

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
  M.table_
    [T.tailwind [T.TableFixed, T.WFull]]
    [ M.colgroup_
        []
        $ map
          (viewColumnWidth . (.width) . t.columnSpec)
          t.columns
    , M.thead_
        []
        $ map
          (viewColumnHeader . (.title) . t.columnSpec)
          t.columns
    , M.tbody_ [] $ map (t.rowContents t.columns) t.rows
    ]
  where
    viewColumnWidth :: TableColumnWidth -> M.View m action
    viewColumnWidth AutoSizedColumn = M.col_ []
    viewColumnWidth SingleActionColumn = M.col_ [T.tailwind [T.W16]]
    viewColumnWidth DoubleActionColumn = M.col_ [T.tailwind [T.W24]]
    viewColumnWidth TripleActionColumn = M.col_ [T.tailwind [T.W32]]

    viewColumnHeader :: M.MisoString -> M.View m action
    viewColumnHeader col =
      M.th_
        [T.tailwind [T.RegularBorder]]
        [M.text_ [col]]

tableHeader_ :: M.MisoString -> M.View m action
tableHeader_ header =
  M.th_ [] [M.text_ [header]]
