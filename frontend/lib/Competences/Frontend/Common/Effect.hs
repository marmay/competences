module Competences.Frontend.Common.Effect
  ( GEffect
  , liftEffect
  , liftEffect_
  , FragmentDef (..)
  , EmbeddedFragment (..)
  , embedFragment
  , toComponent
  , mapSub
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

liftEffect_
  :: Lens' model model'
  -> (action' -> action)
  -> GEffect parent model' action' a
  -> GEffect parent model action ()
liftEffect_ modelLens liftAction eff = () <$ liftEffect modelLens liftAction eff

mapSub :: (a -> b) -> M.Sub a -> M.Sub b
mapSub f sub = \sink -> sub (sink . f)

data FragmentDef parent model action view = FragmentDef
  { initialModel :: model
  , update :: action -> GEffect parent model action ()
  , view :: model -> view
  , subs :: [M.Sub action]
  }

data EmbeddedFragment parent parentModel parentAction fragmentModel fragmentAction view = EmbeddedFragment
  { initialModel :: fragmentModel
  , update :: fragmentAction -> GEffect parent parentModel parentAction ()
  , view :: parentModel -> view
  , subscribe :: [M.Sub parentAction]
  }

embedFragment
  :: Lens' parentModel fragmentModel
  -> (fragmentAction -> parentAction)
  -> FragmentDef parent fragmentModel fragmentAction view
  -> EmbeddedFragment parent parentModel parentAction fragmentModel fragmentAction view
embedFragment lens lift frag = EmbeddedFragment
  { initialModel = frag.initialModel
  , update = \a -> liftEffect_ lens lift (frag.update a)
  , view = \m -> frag.view (m ^. lens)
  , subscribe = map (mapSub lift) frag.subs
  }

toComponent
  :: FragmentDef p model action ((action -> action) -> M.View model action)
  -> M.Component p model action
toComponent frag =
  (M.component frag.initialModel frag.update (\m -> frag.view m id))
    { M.subs = frag.subs
    }
