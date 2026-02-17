module Competences.Frontend.View.Component
  ( component
  , componentA
  , componentIf
  )
where

import Miso qualified as M
import Miso.Html qualified as M

componentA :: (Eq m) => M.MisoString -> [M.Attribute a'] -> M.Component p m a -> M.View p a'
componentA name attrs c =
  M.div_ attrs [name M.+> c]

component :: (Eq m) => M.MisoString -> M.Component p m a -> M.View p a'
component name = componentA name []

componentIf :: (Eq m) => Bool -> M.MisoString -> M.Component p m a -> M.View p a'
componentIf False _ _ = M.text ""
componentIf True name c = component name c
