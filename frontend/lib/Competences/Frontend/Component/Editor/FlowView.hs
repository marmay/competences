module Competences.Frontend.Component.Editor.FlowView
  ( editorFlowView
  )
where

import Competences.Frontend.Component.Editor.View
import Competences.Frontend.View qualified as V
import Data.Tuple (Solo (..))
import Optics.Core ((&), (.~), (^.))

editorFlowView :: EditorView a Solo n
editorFlowView viewData =
  let (MkSolo item) = viewData ^. #items
   in V.viewFlow
        (V.hFlow & #expandDirection .~ V.Expand V.Start & #gap .~ V.SmallSpace)
        (map snd item.fieldData <> compactButtons item)
