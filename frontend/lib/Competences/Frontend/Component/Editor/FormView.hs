module Competences.Frontend.Component.Editor.FormView
  ( editorFormView
  )
where

import Competences.Frontend.Component.Editor.View
import Competences.Frontend.View.Form qualified as V
import Data.Tuple (Solo (..))
import Miso qualified as M

editorFormView :: M.MisoString -> (n -> M.MisoString) -> EditorView a Solo n
editorFormView title toText viewData =
  let (MkSolo item) = viewData.items
   in V.form_
        title
        (map (\(n, f) -> V.formField_ (toText n) f) item.fieldData)
        (extendedButtons item)
