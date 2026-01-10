module Competences.Command.Interpret
  ( EntityCommandContext (..)
  , mkEntityCommandContext
  , mkOrderedEntityCommandContext
  , mkGroupOrderedEntityCommandContext
  , doLock
  , doRelease
  , interpretEntityCommand
  )
where

import Competences.Command.Common (AffectedUsers, EntityCommand (..), ModifyCommand (..), UpdateResult)
import Competences.Document (Document (..), Lock)
import Competences.Document.Id (Id)
import Competences.Document.Order (OrderableSet, reordered, reordered')
import Competences.Document.User (UserId)
import Control.Monad (unless, when)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (Lens', (%), (%~), (&), (^.))
import Optics.Core qualified as O

-- | Context for interpreting entity commands - provides CRUD operations
data EntityCommandContext a patch = EntityCommandContext
  { create :: a -> Document -> Either Text Document
  , delete :: Id a -> Document -> Either Text (Document, a)
  , fetch :: Id a -> Document -> Either Text a
  , applyPatch :: a -> patch -> Either Text a
    -- ^ Apply a patch to an entity, checking for conflicts
  , affectedUsers :: a -> Document -> AffectedUsers
  , lock :: Id a -> Lock
  , getId :: a -> Id a
    -- ^ Extract the ID from an entity (for CreateAndLock)
  , update :: a -> Document -> Document
    -- ^ Update an entity in-place without reordering (for modify operations)
  }
  deriving (Generic)

-- | Create a context for entities that belong to ordered groups
mkGroupOrderedEntityCommandContext
  :: (Ord a, OrderableSet ixs a, Ix.IsIndexOf ix ixs, Ix.IsIndexOf (Id a) ixs)
  => Lens' Document (Ix.IxSet ixs a)
  -> Lens' a (Id a)
  -> (Id a -> Lock)
  -> (a -> ix)
  -> (a -> patch -> Either Text a)
  -> (a -> Document -> AffectedUsers)
  -> EntityCommandContext a patch
mkGroupOrderedEntityCommandContext l idOf lock orderGroup applyPatch affectedUsers =
  let ctx = mkEntityCommandContext l idOf lock applyPatch affectedUsers
   in ctx
        { create = \a d -> O.over l (reordered $ orderGroup a) <$> ctx.create a d
        , delete = \i d -> do
            a <- ctx.fetch i d
            O.over (O._1 % l) (reordered $ orderGroup a) <$> ctx.delete i d
        }

-- | Create a context for ordered entities
mkOrderedEntityCommandContext
  :: (Ord a, OrderableSet ixs a, Ix.IsIndexOf (Id a) ixs)
  => Lens' Document (Ix.IxSet ixs a)
  -> Lens' a (Id a)
  -> (Id a -> Lock)
  -> (a -> patch -> Either Text a)
  -> (a -> Document -> AffectedUsers)
  -> EntityCommandContext a patch
mkOrderedEntityCommandContext l idOf lock applyPatch affectedUsers =
  let ctx = mkEntityCommandContext l idOf lock applyPatch affectedUsers
   in ctx
        { create = \a d -> O.over l reordered' <$> ctx.create a d
        , delete = \i d -> do
            O.over (O._1 % l) reordered' <$> ctx.delete i d
        }

-- | Create a basic entity command context
mkEntityCommandContext
  :: (Ord a, Ix.Indexable ixs a, Ix.IsIndexOf (Id a) ixs)
  => Lens' Document (Ix.IxSet ixs a)
  -> Lens' a (Id a)
  -> (Id a -> Lock)
  -> (a -> patch -> Either Text a)
  -> (a -> Document -> AffectedUsers)
  -> EntityCommandContext a patch
mkEntityCommandContext l idOf lock applyPatch affectedUsers =
  let create a d = do
        unless (Ix.null $ (d ^. l) Ix.@= (a ^. idOf)) $
          Left "entity with that id already exists."
        pure $ d & l %~ Ix.insert a
      fetch i d = do
        case Ix.getOne $ (d ^. l) Ix.@= i of
          Nothing -> Left "entity with that index does not exist!"
          Just a -> pure a
      delete i d = do
        a <- fetch i d
        pure (d & l %~ Ix.delete a, a)
      update a d = d & l %~ Ix.insert a . Ix.deleteIx (a ^. idOf)
   in EntityCommandContext
        { create = create
        , delete = delete
        , fetch = fetch
        , applyPatch = applyPatch
        , affectedUsers = affectedUsers
        , lock = lock
        , getId = (^. idOf)
        , update = update
        }

-- | Lock an entity
doLock :: UserId -> Lock -> Document -> Either Text Document
doLock uid l d =
  case (d ^. #locks) Map.!? l of
    (Just _) -> Left "entity is already locked!"
    Nothing -> pure (d & (#locks %~ Map.insert l uid))

-- | Release a lock on an entity
doRelease :: UserId -> Lock -> Document -> Either Text Document
doRelease uid l d =
  case (d ^. #locks) Map.!? l of
    (Just uid') -> do
      when (uid /= uid') $
        Left "entity is locked by another user!"
      pure (d & (#locks %~ Map.delete l))
    Nothing -> Left "entity is not locked!"

-- | Interpret an entity command using the provided context
interpretEntityCommand
  :: (Eq a) => EntityCommandContext a patch -> UserId -> EntityCommand a patch -> Document -> UpdateResult
interpretEntityCommand ctx _ (Create a) d =
  (,ctx.affectedUsers a d) <$> ctx.create a d
interpretEntityCommand ctx uid (CreateAndLock a) d = do
  d' <- ctx.create a d
  d'' <- doLock uid (ctx.lock (ctx.getId a)) d'
  pure (d'', ctx.affectedUsers a d)
interpretEntityCommand ctx _ (Delete i) d = do
  (d', a) <- ctx.delete i d
  pure (d', ctx.affectedUsers a d)
interpretEntityCommand ctx uid (Modify i Lock) d = do
  d' <- doLock uid (ctx.lock i) d
  a <- ctx.fetch i d'
  pure (d', ctx.affectedUsers a d)
interpretEntityCommand ctx uid (Modify i (Release patch)) d = do
  d' <- doRelease uid (ctx.lock i) d
  aCurrent <- ctx.fetch i d'
  aModified <- ctx.applyPatch aCurrent patch
  -- Use in-place update to preserve ordering (avoid delete+create reorder effects)
  let d'' = ctx.update aModified d'
  pure (d'', ctx.affectedUsers aModified d <> ctx.affectedUsers aCurrent d)
