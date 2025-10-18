module Competences.Command
  ( Command (..)
  , EntityCommand (..)
  , ModifyCommand (..)
  , CommandId
  , handleCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), UpdateResult)
import Competences.Document (CompetenceGrid (..), Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.Competence (Competence (..))
import Competences.Document.Evidence (Evidence (..))
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (Lens', (%), (%~), (&), (.~), (^.))

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
  = ModifyCompetenceGridTitle !(ModifyCommand Text)
  | ModifyCompetenceGridDescription !(ModifyCommand Text)
  | OnCompetences !(EntityCommand Competence)
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
  ModifyCompetenceGridTitle Lock -> do
    (,allUsers d) <$> doLock userId CompetenceGridTitleLock d
  ModifyCompetenceGridTitle (Release Nothing) -> do
    (,allUsers d) <$> doRelease userId CompetenceGridTitleLock d
  ModifyCompetenceGridTitle (Release (Just (t, t'))) -> do
    d' <- doRelease userId CompetenceGridTitleLock d
    when (d'.competenceGrid.title /= t) $
      Left "competence grid title has been modified in the meantime!"
    pure (d' & (#competenceGrid % #title .~ t'), allUsers d')
  ModifyCompetenceGridDescription Lock -> do
    (,allUsers d) <$> doLock userId CompetenceGridDescriptionLock d
  ModifyCompetenceGridDescription (Release Nothing) -> do
    (,allUsers d) <$> doRelease userId CompetenceGridDescriptionLock d
  ModifyCompetenceGridDescription (Release (Just (t, t'))) -> do
    d' <- doRelease userId CompetenceGridDescriptionLock d
    when (d'.competenceGrid.description /= t) $
      Left "competence grid description has been modified in the meantime!"
    pure (d' & (#competenceGrid % #description .~ t'), allUsers d')
  OnCompetences c -> interpretEntityCommand competenceContext userId c d
  OnUsers c -> interpretEntityCommand userContext userId c d
  OnEvidences c -> interpretEntityCommand evidenceContext userId c d
  where
    competenceContext =
      mkEntityCommandContext
        #competences
        #id
        CompetenceLock
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
