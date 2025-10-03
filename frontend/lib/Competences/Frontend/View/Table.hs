module Competences.Frontend.View.Table
  ( Table (..)
  , TableColumnWidth (..)
  , defTable
  , viewTable
  , tableHeader_
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

data Table col row m action = Table
  { columns :: [col]
  , rows :: [row]
  , columnSpec :: col -> TableColumnWidth
  , columnHeader :: col -> M.MisoString
  , cellContents :: row -> col -> M.View m action
  }

defTable :: Table col row m action
defTable =
  Table
    { columns = []
    , rows = []
    , columnSpec = const AutoSizedColumn
    , columnHeader = const ""
    , cellContents = const $ const $ M.text_ []
    }

viewTable :: forall col row m action. Table col row m action -> M.View m action
viewTable t =
  M.table_
    [T.tailwind [T.TableFixed, T.WFull]]
    [ M.colgroup_
        []
        $ map
          (viewColumnSpec . t.columnSpec)
          t.columns
    , M.thead_
        []
        $ map
          (viewColumnHeader . t.columnHeader)
          t.columns
    , M.tbody_ [] $ map (viewRow t.columns) t.rows
    ]
  where
    viewColumnSpec :: TableColumnWidth -> M.View m action
    viewColumnSpec AutoSizedColumn = M.col_ []
    viewColumnSpec SingleActionColumn = M.col_ [T.tailwind [T.W10]]
    viewColumnSpec DoubleActionColumn = M.col_ [T.tailwind [T.W20]]
    viewColumnSpec TripleActionColumn = M.col_ [T.tailwind [T.W30]]

    viewColumnHeader :: M.MisoString -> M.View m action
    viewColumnHeader col =
      M.th_
        [T.tailwind [T.RegularBorder]]
        [M.text_ [col]]

    viewRow :: [col] -> row -> M.View m action
    viewRow cols row =
      let cellContents' = t.cellContents row
       in M.tr_ [T.tailwind []] $ map (\col -> M.td_ [T.tailwind [T.RegularBorder]] [cellContents' col]) cols

tableHeader_ :: M.MisoString -> M.View m action
tableHeader_ header =
  M.th_ [] [M.text_ [header]]
