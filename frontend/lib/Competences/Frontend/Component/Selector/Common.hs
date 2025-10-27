module Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , mkSelectorBinding
  , selectorTransformedLens
  , selectorLens
  )
where

import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core qualified as O

data SelectorTransformedLens p f a f' a' = SelectorTransformedLens
  { lens :: O.Lens' p (f' a')
  , transform :: a -> a'
  , embed :: f a' -> f' a'
  } deriving (Generic)

selectorTransformedLens
  :: forall p f a f' a'
   . (a -> a') -> (f a' -> f' a') -> O.Lens' p (f' a') -> SelectorTransformedLens p f a f' a'
selectorTransformedLens t e s = SelectorTransformedLens s t e

selectorLens :: forall p f a. O.Lens' p (f a) -> SelectorTransformedLens p f a f a
selectorLens = selectorTransformedLens id id

mkSelectorBinding
  :: forall p m f a f' a'
   . (Functor f) => SelectorTransformedLens p f a f' a' -> O.Lens' m (f a) -> M.Binding p m
mkSelectorBinding SelectorTransformedLens {lens, transform, embed} g =
  O.toLensVL lens
    M.<--- O.toLensVL (O.lens (\m -> embed $ fmap transform (m O.^. g)) undefined)
