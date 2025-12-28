module Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , EntityPatchTransformedLens (..)
  , mkSelectorBinding
  , selectorTransformedLens
  , selectorLens
  , entityPatchTransformedLens
  , entityPatchLens
  )
where

import Competences.Command.Common (Change)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core qualified as O

data SelectorTransformedLens p f a f' a' = SelectorTransformedLens
  { lens :: O.Lens' p (f' a')
  , transform :: a -> a'
  , embed :: f a' -> f' a'
  } deriving (Generic)

-- | Represents a selector that operates on both entity and patch levels
--   Used as input to selectorEditorField before being transformed to operate on Model
data EntityPatchTransformedLens entity patch f b f' b' = EntityPatchTransformedLens
  { viewLens :: O.Lens' entity (f' b')
  , patchLens :: O.Lens' patch (Change (f' b'))
  , transform :: b -> b'
  , embed :: f b' -> f' b'
  } deriving (Generic)

selectorTransformedLens
  :: forall p f a f' a'
   . (a -> a') -> (f a' -> f' a') -> O.Lens' p (f' a') -> SelectorTransformedLens p f a f' a'
selectorTransformedLens t e s = SelectorTransformedLens s t e

selectorLens :: forall p f a. O.Lens' p (f a) -> SelectorTransformedLens p f a f a
selectorLens = selectorTransformedLens id id

-- | Construct an EntityPatchTransformedLens with custom transform and embed
entityPatchTransformedLens
  :: O.Lens' entity (f' b')
  -> O.Lens' patch (Change (f' b'))
  -> (b -> b')
  -> (f b' -> f' b')
  -> EntityPatchTransformedLens entity patch f b f' b'
entityPatchTransformedLens vl pl t e =
  EntityPatchTransformedLens
    { viewLens = vl
    , patchLens = pl
    , transform = t
    , embed = e
    }

-- | Construct an EntityPatchTransformedLens with identity transform and embed
entityPatchLens
  :: O.Lens' entity (f b')
  -> O.Lens' patch (Change (f b'))
  -> EntityPatchTransformedLens entity patch f b' f b'
entityPatchLens vl pl = entityPatchTransformedLens vl pl id id

mkSelectorBinding
  :: forall p m f a f' a'
   . (Functor f) => SelectorTransformedLens p f a f' a' -> O.Lens' m (f a) -> M.Binding p m
mkSelectorBinding SelectorTransformedLens {lens, transform, embed} g =
  O.toLensVL lens
    M.<--- O.toLensVL (O.lens (\m -> embed $ fmap transform (m O.^. g)) undefined)
