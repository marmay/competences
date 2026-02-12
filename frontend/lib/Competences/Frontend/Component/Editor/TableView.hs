module Competences.Frontend.Component.Editor.TableView
  ( editorTableRowView
  , editorTableRowView'
  )
where

import Competences.Frontend.Component.Editor.View
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Table qualified as Table
import Data.Foldable (toList)
import Miso qualified as M

data TableRowEditorColumn n
  = TableRowEditorNamedColumn n
  | TableRowEditorActionColumn

editorTableRowView
  :: (Foldable f) => (n -> Table.TableColumnSpec) -> Table.TableColumnSpec -> EditorView a patch f n
editorTableRowView specOf actionSpec viewData =
  Table.viewTable $
    Table.Table
      { columns = map TableRowEditorNamedColumn viewData.fields <> [TableRowEditorActionColumn]
      , rows = toList viewData.items
      , columnSpec = \case
          TableRowEditorNamedColumn n -> specOf n
          TableRowEditorActionColumn -> actionSpec
      , rowContents = \_ r ->
          -- We know that cols matches the fields.
          Table.tableRow $ map snd r.fieldData <> [Layout.viewFlow Layout.hFlow{Layout.gap = Layout.SmallSpace} $ compactButtons r]
      }

editorTableRowView' :: (Foldable f) => EditorView a patch f M.MisoString
editorTableRowView' =
  editorTableRowView (Table.TableColumnSpec Table.AutoSizedColumn) (Table.TableColumnSpec Table.TripleActionColumn "")
