module Competences.Frontend.Component.Editor.TableView
  ( editorTableRowView
  , editorTableRowView'
  )
where

import Competences.Frontend.Component.Editor.View
import Competences.Frontend.View qualified as V
import Data.Foldable (toList)
import Miso qualified as M
import Optics.Core ((&), (.~))

data TableRowEditorColumn n
  = TableRowEditorNamedColumn n
  | TableRowEditorActionColumn

editorTableRowView
  :: (Foldable f) => (n -> V.TableColumnSpec) -> V.TableColumnSpec -> EditorView a f n
editorTableRowView specOf actionSpec viewData =
  V.viewTable $
    V.Table
      { columns = map TableRowEditorNamedColumn viewData.fields <> [TableRowEditorActionColumn]
      , rows = toList viewData.items
      , columnSpec = \case
          TableRowEditorNamedColumn n -> specOf n
          TableRowEditorActionColumn -> actionSpec
      , rowContents = \_ r ->
          -- We know that cols matches the fields.
          V.tableRow $ map snd r.fieldData <> [V.viewFlow (V.hFlow & #gap .~ V.SmallSpace) $ compactButtons r]
      }

editorTableRowView' :: (Foldable f) => EditorView a f M.MisoString
editorTableRowView' =
  editorTableRowView (V.TableColumnSpec V.AutoSizedColumn) (V.TableColumnSpec V.TripleActionColumn "")
