module Competences.Frontend.Component.Editor.FlowView
  ( editorFlowView
  )
where

import Competences.Frontend.Component.Editor.View
import Competences.Frontend.View.Layout qualified as Layout
import Data.Tuple (Solo (..))
import Optics.Core ((^.))

editorFlowView :: EditorView a patch Solo n
editorFlowView viewData =
  let (MkSolo item) = viewData ^. #items
   in Layout.viewFlow
        Layout.hFlow{Layout.expandDirection = Layout.Expand Layout.Start, Layout.gap = Layout.SmallSpace}
        (map snd item.fieldData <> compactButtons item)
