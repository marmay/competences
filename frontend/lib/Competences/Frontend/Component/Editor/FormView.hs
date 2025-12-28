module Competences.Frontend.Component.Editor.FormView
  ( editorFormView
  , editorFormView'
  , editorFormViewS
  )
where

import Competences.Frontend.Component.Editor.View
import Competences.Frontend.View.Form qualified as V
import Data.Tuple (Solo (..))
import Miso qualified as M
import Competences.Frontend.Component.Editor.Types (Model, Action)
import qualified Competences.Frontend.View as V
import qualified Competences.Frontend.Common as C

editorFormView :: Foldable f => M.View (Model a patch f) (Action a patch) -> M.MisoString -> (n -> M.MisoString) -> EditorView a patch f n
editorFormView onMissing title toText viewData =
  let maybeFirst = foldr (\x _ -> Just x) Nothing viewData.items
  in maybe onMissing (showForm title toText) maybeFirst

editorFormView' :: Foldable f => M.MisoString -> (n -> M.MisoString) -> EditorView a patch f n
editorFormView' = editorFormView onMissing
  where onMissing = V.text_ (C.translate' C.LblPleaseSelectItem)

editorFormViewS :: M.MisoString -> (n -> M.MisoString) -> EditorView a patch Solo n
editorFormViewS title toText viewData =
  let (MkSolo item) = viewData.items
   in showForm title toText item

showForm :: M.MisoString -> (n -> M.MisoString) -> EditorViewItem a patch f n -> M.View (Model a patch f) (Action a patch)
showForm title toText item =
   V.form_
        title
        (map (\(n, f) -> V.formField_ (toText n) f) item.fieldData)
        (extendedButtons item)
