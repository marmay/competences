{-# LANGUAGE DefaultSignatures #-}

module Competences.Document.Order
  ( Order
  , OrderPosition
  , Orderable (..)
  , Reorder (..)
  , ReorderError (..)
  , OrderableSet
  , explainReorderError
  , orderPos
  , orderAt
  , orderMax
  , orderMin
  , orderPosition
  , ordered
  , orderedInsert
  , orderedDelete
  , reorder
  , reordered
  , reordered'
  , formatOrderNumber
  )
where

import Competences.Document.Id (Id)
import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Either.Extra (maybeToEither)
import Data.IxSet.Typed qualified as Ix
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Optics.Core (A_Lens, LabelOptic', Lens', (%~), (&), (.~), (^.))

-- | Defines the ordering within a collection; opaque as it is implemented
-- using wonky integer arithmetics.
newtype Order = Order Int
  deriving (Eq, Generic, Ord, Show)

class (Eq a, Ord a) => Orderable a where
  idL :: Lens' a (Id a)
  orderL :: Lens' a Order

  default idL :: (LabelOptic' "id" A_Lens a (Id a)) => Lens' a (Id a)
  idL = #id

  default orderL :: (LabelOptic' "order" A_Lens a Order) => Lens' a Order
  orderL = #order

-- | Collection of common constraints:
--   * The collection is an IxSet.
--   * The collection is indexable by Order.
--   * The collection is indexable by the Id of its elements.
--   * The elements of the collection have an id property.
--   * The elements of the collection have an order property.
type OrderableSet ixs a =
  (Ix.Indexable ixs a, Ix.IsIndexOf Order ixs, Ix.IsIndexOf (Id a) ixs, Orderable a)

-- | Describes the position of an item within a collection. Used to determine
-- whether a reordering operation can be applied to a collection.
data OrderPosition a = OrderPosition
  { before :: !(Maybe (Id a))
  -- ^ The item before the given item; Nothing if the item is the first one in the collection.
  , this :: !(Id a)
  -- ^ The id of the item of which the position is described.
  , after :: !(Maybe (Id a))
  -- ^ The item after the given item; Nothing if the item is the last one in the collection.
  }
  deriving (Eq, Generic, Ord, Show)

-- | Describes where to reorder a given item to.
data Reorder a
  = -- | Moves the item to the front of the collection.
    Front
  | -- | Moves the item to the back of the collection.
    Back
  | -- | Moves the item one place forward relative to its current position.
    Forward
  | -- | Moves the item one place backward relative to its current position.
    Backward
  | -- | Moves the item before the item with the given id.
    Before !(Id a)
  | -- | Moves the item after the item with the given id.
    After !(Id a)
  deriving (Eq, Generic, Ord, Show)

-- | Reordering can run into errors.
data ReorderError a
  = -- | The orderPositioon of the item is invalid within this collection.
    InvalidOrderPosition !(OrderPosition a)
  | -- | One of the referenced elements in either Before or After was not found within the collection.
    ReferencedElementNotFound !(Id a)
  deriving (Eq, Generic, Ord, Show)

explainReorderError :: ReorderError a -> Text
explainReorderError (InvalidOrderPosition p) = "Invalid order position: " <> pack (show p)
explainReorderError (ReferencedElementNotFound i) = "Referenced element not found: " <> pack (show i)

-- | Specify a given order.
orderPos :: Int -> Order
orderPos i = Order $ 2 * i

-- | Use this to insert an item at a given position in the collection.
orderAt :: Int -> Order
orderAt i = Order $ 2 * i - 1

-- | Use this to insert an item at the end of the collection.
orderMax :: Order
orderMax = Order maxBound

-- | Use this to insert an item at the beginning of the collection.
orderMin :: Order
orderMin = Order minBound

-- | Gives an index in ordered sequence.
ordered :: (OrderableSet ixs a) => Ix.IxSet ixs a -> [a]
ordered = Ix.toAscList (Proxy @Order)

-- | Inserts and reorders an element in a collection.
orderedInsert
  :: (OrderableSet ixs a)
  => a -> Ix.IxSet ixs a -> Either Text (Ix.IxSet ixs a)
orderedInsert a s = do
  unless (Ix.null $ s Ix.@= (a ^. idL)) $
    Left "An item with the given id already exists!"
  pure $ reordered' $ Ix.insert a s

-- | Deletes and reorders an element from a collection.
orderedDelete
  :: (OrderableSet ixs a)
  => Id a -> Ix.IxSet ixs a -> Either Text (Ix.IxSet ixs a)
orderedDelete idA s =
  pure $ reordered' $ Ix.deleteIx idA s

-- | Generates the orderPosition of an element with a given id.
orderPosition
  :: (OrderableSet ixs a) => Ix.IxSet ixs a -> Id a -> Maybe (OrderPosition a)
orderPosition s this = do
  a <- Ix.getOne $ s Ix.@= this
  let order = a ^. orderL
  let before = fmap (^. idL) $ listToMaybe $ Ix.toDescList (Proxy @Order) $ s Ix.@< order
  let after = fmap (^. idL) $ listToMaybe $ Ix.toAscList (Proxy @Order) $ s Ix.@> order
  pure OrderPosition {before, this, after}

-- | Reorders an item in a collection.
reorder
  :: (OrderableSet ixs a, Ix.IsIndexOf ix ixs)
  => OrderPosition a -> Reorder a -> Ix.IxSet ixs a -> (a -> ix) -> Either (ReorderError a) (Ix.IxSet ixs a)
reorder p Front s ix = withValidatedOrderPosition s ix p (orderL .~ orderMin)
reorder p Back s ix = withValidatedOrderPosition s ix p (orderL .~ orderMax)
reorder p Forward s ix = withValidatedOrderPosition s ix p (orderL %~ up)
  where
    up (Order i) = Order $ i - 3
reorder p Backward s ix = withValidatedOrderPosition s ix p (orderL %~ down)
  where
    down (Order i) = Order $ i + 3
reorder p (Before i) s ix = do
  o <- orderOf s i
  withValidatedOrderPosition s ix p (orderL .~ before o)
  where
    before (Order o) = Order $ o - 1
reorder p (After i) s ix = do
  o <- orderOf s i
  withValidatedOrderPosition s ix p (orderL .~ after o)
  where
    after (Order o) = Order $ o + 1

-- | Helper function that gets the order of an element, if it is within the collection.
orderOf
  :: (OrderableSet ixs a)
  => Ix.IxSet ixs a -> Id a -> Either (ReorderError a) Order
orderOf s i = maybeToEither (ReferencedElementNotFound i) $ fmap (^. orderL) $ Ix.getOne $ s Ix.@= i

-- | Ensures that a given orderPosition is valid for a given collection.
validateOrderPosition
  :: (OrderableSet ixs a)
  => Ix.IxSet ixs a -> OrderPosition a -> Bool
validateOrderPosition s p@OrderPosition {this} =
  orderPosition s this == Just p

-- | Helper function that conditionally applies a function to an item, if
-- that item is at the position described by its OrderPosition.
withValidatedOrderPosition
  :: (OrderableSet ixs a, Ix.IsIndexOf ix ixs)
  => Ix.IxSet ixs a -> (a -> ix) -> OrderPosition a -> (a -> a) -> Either (ReorderError a) (Ix.IxSet ixs a)
withValidatedOrderPosition s ixOf p@OrderPosition {this} f
  | validateOrderPosition s p = case Ix.getOne $ s Ix.@= this of
      Just a -> Right $ reordered (ixOf a) $ Ix.insert (f a) $ Ix.deleteIx (a ^. idL) s
      Nothing -> Left $ ReferencedElementNotFound this
  | otherwise = Left $ InvalidOrderPosition p

-- | Updates all order properties within the collection to prepare it for
-- new insertions.
reordered
  :: (OrderableSet ixs a, Ix.IsIndexOf ix ixs)
  => ix -> Ix.IxSet ixs a -> Ix.IxSet ixs a
reordered ix as =
  let as' = reordered' (as Ix.@= ix)
  in as Ix.@< ix Ix.||| as' Ix.||| as Ix.@> ix

reordered'
  :: (OrderableSet ixs a)
  => Ix.IxSet ixs a -> Ix.IxSet ixs a
reordered' as = Ix.fromList $ zipWith (\a i -> a & orderL .~ Order i) (ordered as) [0, 2 ..]

formatOrderNumber :: Order -> String
formatOrderNumber (Order i)
  | i == minBound = "min"
  | i == maxBound = "max"
  | otherwise = show (i `div` 2)

instance FromJSON Order

instance ToJSON Order

instance Binary Order

instance FromJSON (OrderPosition a)

instance ToJSON (OrderPosition a)

instance Binary (OrderPosition a)

instance FromJSON (Reorder a)

instance ToJSON (Reorder a)

instance Binary (Reorder a)
