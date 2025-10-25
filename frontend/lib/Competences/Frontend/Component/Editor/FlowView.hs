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
  let
    (MkSolo item) = viewData ^. #items
    itemActionsOf item = []
  in V.viewFlow
        (V.hFlow & #gap .~ V.SmallSpace)
        (map snd item.fieldData <> itemActionsOf item)
