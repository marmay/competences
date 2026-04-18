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

data FragmentDef parent model action result view = FragmentDef
  { initialModel :: model
  , update :: action -> GEffect parent model action result
  , view :: model -> view
  , subs :: [M.Sub action]
  }

data EmbeddedFragment parent parentModel parentAction fragmentModel fragmentAction result view = EmbeddedFragment
  { initialModel :: fragmentModel
  , update :: fragmentAction -> GEffect parent parentModel parentAction result
  , view :: parentModel -> view
  , subscribe :: [M.Sub parentAction]
  }

embedFragment
  :: Lens' parentModel fragmentModel
  -> (fragmentAction -> parentAction)
  -> FragmentDef parent fragmentModel fragmentAction result view
  -> EmbeddedFragment parent parentModel parentAction fragmentModel fragmentAction result view
embedFragment lens lift frag = EmbeddedFragment
  { initialModel = frag.initialModel
  , update = \a -> liftEffect lens lift (frag.update a)
  , view = \m -> frag.view (m ^. lens)
  , subscribe = map (mapSub lift) frag.subs
  }

toComponent
  :: FragmentDef p model action result ((action -> action) -> M.View model action)
  -> M.Component p model action
toComponent frag =
  (M.component frag.initialModel (\a -> () <$ frag.update a) (\m -> frag.view m id))
    { M.subs = frag.subs
    }
