module Competences.Frontend.View.Component
  ( component
  , component'
  )
where

import Miso qualified as M
import Miso.Html qualified as M

component :: (Eq m) => M.MisoString -> M.Component p m a -> M.View p a'
component name c =
  M.div_ [M.key_ name] M.+> c

component' :: (Eq m) => M.Component p m a -> M.View p a'
component' c =
  M.div_ [] M.+> c
