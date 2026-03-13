module Competences.Frontend.Component.Editor.FlowView
  ( editorFlowView
  )
where

import Competences.Frontend.Component.Editor.View
import Competences.Frontend.View.Layout qualified as Layout
import Data.Tuple (Solo (..))
import Optics.Core ((^.))

editorFlowView :: (Eq a) => EditorView a patch Solo n
editorFlowView viewData =
  let (MkSolo item) = viewData ^. #items
   in Layout.hFlow
        (Layout.gapS <> Layout.wFull)
        (map snd item.fieldData <> compactButtons item)
