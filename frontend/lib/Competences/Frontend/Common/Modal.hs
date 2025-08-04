module Competences.Frontend.Common.Modal
  ( modal
  , maybeModal
  )
where

import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Miso qualified as M

modal :: [M.Attribute a] -> [M.View m a] -> M.View m a
modal attrs children = M.div_ (styledClass ClsModal : attrs) children

maybeModal :: Maybe m -> (m -> M.View m' a) -> M.View m' a
maybeModal (Just m) f = modal [] [f m]
maybeModal Nothing _ = M.div_ [] []
