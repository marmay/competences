module Competences.Frontend.View.Component
  ( component
  , componentA
  )
where

import Miso qualified as M
import Miso.Html qualified as M

componentA :: (Eq m) => M.MisoString -> [M.Attribute a'] -> M.Component p m a -> M.View p a'
componentA name attrs c =
  M.div_ attrs [name M.+> c]

component :: (Eq m) => M.MisoString -> M.Component p m a -> M.View p a'
component name c =
  M.div_ [] [name M.+> c]
