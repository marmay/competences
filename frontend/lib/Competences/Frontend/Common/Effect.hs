module Competences.Frontend.Common.Effect
  ( GEffect
  , liftEffect
  , liftEffect_
  )
where

import Control.Monad.RWS (RWS, runRWS, rws)
import Miso qualified as M
import Optics.Core (Lens', (&), (.~), (^.))

type GEffect parent model action a = RWS (M.ComponentInfo parent) [M.Schedule action] model a

liftEffect
  :: forall parent model action model' action' a
   . Lens' model model'
  -> (action' -> action)
  -> GEffect parent model' action' a
  -> GEffect parent model action a
liftEffect modelLens liftAction eff = rws $ \r s ->
  let (a, s', w) = runRWS eff r (s ^. modelLens)
   in (a, s & (modelLens .~ s'), fmap liftSchedule w)
  where
    liftSchedule :: M.Schedule action' -> M.Schedule action
    liftSchedule (M.Schedule s sinkEff) = M.Schedule s $ \a -> sinkEff (a . liftAction)

-- | Like 'liftEffect' but discards the return value.
liftEffect_
  :: Lens' model model'
  -> (action' -> action)
  -> GEffect parent model' action' a
  -> GEffect parent model action ()
liftEffect_ modelLens liftAction eff = () <$ liftEffect modelLens liftAction eff
