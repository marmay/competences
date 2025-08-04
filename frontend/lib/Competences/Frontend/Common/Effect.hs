module Competences.Frontend.Common.Effect
  ( liftEffect
  , liftEffect'
  , liftSub
  )
where

import Miso qualified as M
import Optics.Core (Lens', (&), (.~), (^.))

liftEffect :: forall m m' a a' p. Lens' m m' -> (a' -> a) -> M.Effect p m' a' -> M.Effect p m a
liftEffect lModel liftAction e = do
  domRef <- M.ask
  m <- M.get
  let (m'', a') = M.runEffect e domRef $ m ^. lModel
  M.put $ m & lModel .~ m''
  M.tell $ map (liftSub liftAction) a'
  pure ()

liftEffect' :: forall m m' a a' p. Lens' m (Maybe m') -> (a' -> a) -> M.Effect p m' a' -> M.Effect p m a
liftEffect' lModel liftAction e = do
  domRef <- M.ask
  m <- M.get
  case m ^. lModel of
    Nothing -> pure ()
    Just m' -> do
      let (m'', a') = M.runEffect e domRef m'
      M.put $ m & lModel .~ Just m''
      M.tell $ map (liftSub liftAction) a'
      pure ()

liftSub :: forall a a'. (a' -> a) -> M.Sub a' -> M.Sub a
liftSub l f g = f $ g . l
