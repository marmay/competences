module Competences.Command
  ( Command (..)
  , EntityCommand (..)
  , ModifyCommand (..)
  , CommandId
  , handleCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), UpdateResult)
import Competences.Document
  ( CompetenceGrid (..)
  , Document (..)
  , Lock (..)
  , User (..)
  , UserRole (..)
  )
import Competences.Document.Competence (Competence (..))
import Competences.Document.Evidence (Evidence (..))
import Competences.Document.Id (Id)
import Competences.Document.Order
  ( OrderPosition
  , OrderableSet
  , Reorder
  , explainReorderError
  , reorder
  , reordered
  , reordered'
  )
import Competences.Document.User (UserId)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (Lens', (%), (%~), (&), (.~), (^.))
import Optics.Core qualified as O

data ModifyCommand a
  = Lock
  | Release !(Maybe (a, a))
  deriving (Eq, Generic, Show)

data EntityCommand a
  = Create !a
  | Delete !(Id a)
  | Modify !(Id a) !(ModifyCommand a)
  deriving (Eq, Generic, Show)

data Command
  = SetDocument !Document
  | OnCompetenceGrids !(EntityCommand CompetenceGrid)
  | OnCompetences !(EntityCommand Competence)
  | ReorderCompetence !(OrderPosition Competence) !(Reorder Competence)
  | OnUsers !(EntityCommand User)
  | OnEvidences !(EntityCommand Evidence)
  deriving (Eq, Generic, Show)

type CommandId = Id Command

instance (FromJSON a) => FromJSON (ModifyCommand a)

instance (ToJSON a) => ToJSON (ModifyCommand a)

instance (FromJSON a) => FromJSON (EntityCommand a)

instance (ToJSON a) => ToJSON (EntityCommand a)

instance FromJSON Command

instance ToJSON Command

data EntityCommandContext a = EntityCommandContext
  { create :: a -> Document -> Either Text Document
  , delete :: Id a -> Document -> Either Text (Document, a)
  , fetch :: Id a -> Document -> Either Text a
  , affectedUsers :: a -> Document -> AffectedUsers
  , lock :: Id a -> Lock
  }
  deriving (Generic)

mkGroupOrderedEntityCommandContext
  :: (Ord a, OrderableSet ixs a, Ix.IsIndexOf ix ixs, Ix.IsIndexOf (Id a) ixs)
  => Lens' Document (Ix.IxSet ixs a)
  -> Lens' a (Id a)
  -> (Id a -> Lock)
  -> (a -> ix)
  -> (a -> Document -> AffectedUsers)
  -> EntityCommandContext a
mkGroupOrderedEntityCommandContext l idOf lock orderGroup affectedUsers =
  let ctx = mkEntityCommandContext l idOf lock affectedUsers
   in ctx
        { create = \a d -> O.over l (reordered $ orderGroup a) <$> ctx.create a d
        , delete = \i d -> do
            a <- ctx.fetch i d
            O.over (O._1 % l) (reordered $ orderGroup a) <$> ctx.delete i d
        }

mkOrderedEntityCommandContext
  :: (Ord a, OrderableSet ixs a, Ix.IsIndexOf (Id a) ixs)
  => Lens' Document (Ix.IxSet ixs a)
  -> Lens' a (Id a)
  -> (Id a -> Lock)
  -> (a -> Document -> AffectedUsers)
  -> EntityCommandContext a
mkOrderedEntityCommandContext l idOf lock affectedUsers =
  let ctx = mkEntityCommandContext l idOf lock affectedUsers
   in ctx
        { create = \a d -> O.over l reordered' <$> ctx.create a d
        , delete = \i d -> do
            O.over (O._1 % l) reordered' <$> ctx.delete i d
        }

mkEntityCommandContext
  :: (Ord a, Ix.Indexable ixs a, Ix.IsIndexOf (Id a) ixs)
  => Lens' Document (Ix.IxSet ixs a)
  -> Lens' a (Id a)
  -> (Id a -> Lock)
  -> (a -> Document -> AffectedUsers)
  -> EntityCommandContext a
mkEntityCommandContext l idOf lock affectedUsers =
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
   in EntityCommandContext
        { create = create
        , delete = delete
        , fetch = fetch
        , affectedUsers = affectedUsers
        , lock = lock
        }

doLock :: UserId -> Lock -> Document -> Either Text Document
doLock uid l d =
  case (d ^. #locks) Map.!? l of
    (Just _) -> Left "entity is already locked!"
    Nothing -> pure (d & (#locks %~ Map.insert l uid))

doRelease :: UserId -> Lock -> Document -> Either Text Document
doRelease uid l d =
  case (d ^. #locks) Map.!? l of
    (Just uid') -> do
      when (uid /= uid') $
        Left "entity is locked by another user!"
      pure (d & (#locks %~ Map.delete l))
    Nothing -> Left "entity is not locked!"

interpretEntityCommand
  :: (Eq a) => EntityCommandContext a -> UserId -> EntityCommand a -> Document -> UpdateResult
interpretEntityCommand ctx _ (Create a) d =
  (,ctx.affectedUsers a d) <$> ctx.create a d
interpretEntityCommand ctx _ (Delete i) d = do
  (d', a) <- ctx.delete i d
  pure (d', ctx.affectedUsers a d)
interpretEntityCommand ctx uid (Modify i Lock) d = do
  d' <- doLock uid (ctx.lock i) d
  a <- ctx.fetch i d'
  pure (d', ctx.affectedUsers a d)
interpretEntityCommand ctx uid (Modify i (Release Nothing)) d = do
  d' <- doRelease uid (ctx.lock i) d
  a <- ctx.fetch i d'
  pure (d', ctx.affectedUsers a d)
interpretEntityCommand ctx uid (Modify i (Release (Just (a, a')))) d = do
  d' <- doRelease uid (ctx.lock i) d
  a'' <- ctx.fetch i d'
  when (a'' /= a) $
    Left "entity has been modified in the meantime!"
  d'' <- ctx.create a' . fst =<< ctx.delete i d'
  pure (d'', ctx.affectedUsers a' d <> ctx.affectedUsers a d)

handleCommand :: UserId -> Command -> Document -> UpdateResult
handleCommand userId cmd d = case cmd of
  SetDocument newDoc ->
    -- Replace entire document, all users affected
    let allUserIds = map (.id) $ Ix.toList $ newDoc ^. #users
     in Right (newDoc, AffectedUsers allUserIds)
  OnCompetenceGrids c -> interpretEntityCommand competenceGridContext userId c d
  OnCompetences c -> interpretEntityCommand competenceContext userId c d
  ReorderCompetence p t -> do
    case reorder p t d.competences (.competenceGridId) of
      Left err -> Left $ explainReorderError err
      Right c' -> Right (d & (#competences .~ c'), allUsers d)
  OnUsers c -> interpretEntityCommand userContext userId c d
  OnEvidences c -> interpretEntityCommand evidenceContext userId c d
  where
    competenceGridContext =
      mkOrderedEntityCommandContext
        #competenceGrids
        #id
        CompetenceGridLock
        (\_ d' -> allUsers d')
    competenceContext =
      mkGroupOrderedEntityCommandContext
        #competences
        #id
        CompetenceLock
        (^. #competenceGridId)
        (\_ d' -> allUsers d')
    userContext =
      mkEntityCommandContext
        #users
        #id
        UserLock
        (\_ d' -> allUsers d')
    evidenceContext =
      mkEntityCommandContext
        #evidences
        #id
        EvidenceLock
        (\e d' -> allTeachersAnd d' (Set.toList e.userIds))
    allUsers d' = AffectedUsers $ map (.id) $ Ix.toList $ d' ^. #users
    allTeachersAnd d' us =
      AffectedUsers $
        map (.id) $
          Ix.toList (d' ^. #users) & filter (\u -> u.id `elem` us || u.role == Teacher)
