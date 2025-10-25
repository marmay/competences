module Competences.Frontend.Component.Static
  ( StaticComponent
  , StaticComponent'
  , StaticView
  , StaticView'
  , staticComponent
  , staticComponent'
  )
where

import Miso qualified as M

data Action

type StaticComponent p a = M.Component p () a
type StaticComponent' p = M.Component p () Action

type StaticView a = M.View () a
type StaticView' = M.View () Action

staticComponent :: forall p a. (a -> M.Effect p () a) -> StaticView a -> StaticComponent p a
staticComponent update view =
  M.component () update (const view)

staticComponent' :: forall p. StaticView' -> StaticComponent' p
staticComponent' = staticComponent (\_ -> pure ())
