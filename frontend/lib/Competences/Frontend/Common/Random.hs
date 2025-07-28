module Competences.Frontend.Common.Random
  ( random'
  , split'
  )
where

import Control.Monad.State.Class (MonadState (..))
import Optics.Core (A_Lens, (&), (.~), (^.))
import Optics.Label (LabelOptic')
import System.Random (Random (..), RandomGen (..), SplitGen (..))

-- | If you need something random in an update method and the model
-- has a random generator, you can use this function to simultanously
-- generate a random value and update the generator in the model.
random'
  :: forall m s g a
   . (RandomGen g, SplitGen g, Random a, LabelOptic' "random" A_Lens s g, MonadState s m) => m a
random' = state $ \s ->
  let (a, g) = random $ s ^. #random
   in (a, s & (#random .~ g))

split'
  :: forall m s g. (RandomGen g, SplitGen g, LabelOptic' "random" A_Lens s g, MonadState s m) => m g
split' = state $ \s ->
  let (g1, g2) = splitGen $ s ^. #random
   in (g2, s & (#random .~ g1))
