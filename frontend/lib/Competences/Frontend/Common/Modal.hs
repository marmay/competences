module Competences.Frontend.Common.Modal
  ( modal
  , maybeModal
  )
where

import Competences.Frontend.Common.Style (ClassName (..), styledClass)
import Miso qualified as M

modal :: [M.Attribute a] -> [M.View a] -> M.View a
modal attrs children = M.div_ (styledClass ClsModal : attrs) children

maybeModal :: Maybe m -> (m -> M.View a) -> M.View a
maybeModal (Just m) f = modal [] [f m]
maybeModal Nothing _ = M.div_ [] []
