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

data SelectorTransformedLens p a a' = SelectorTransformedLens
  { lens :: O.Lens' p a'
  , transformer :: a -> a'
  }
  deriving (Generic)

selectorTransformedLens
  :: forall p a a'. (a -> a') -> O.Lens' p a' -> SelectorTransformedLens p a a'
selectorTransformedLens t s = SelectorTransformedLens s t

selectorLens :: forall p a. O.Lens' p a -> SelectorTransformedLens p a a
selectorLens = selectorTransformedLens id

mkSelectorBinding
  :: forall p m a a'. SelectorTransformedLens p a a' -> O.Lens' m a -> M.Binding p m
mkSelectorBinding SelectorTransformedLens {lens, transformer} g =
  O.toLensVL lens
    M.<--- O.toLensVL (O.lens (\m -> transformer (m O.^. g)) undefined)
