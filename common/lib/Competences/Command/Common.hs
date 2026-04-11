{-# LANGUAGE CPP #-}

module Competences.Command.Common
  ( AffectedUsers (..)
  , UpdateResult
  , Change
  , CommandContext (..)
  , ModifyCommand (..)
  , EntityCommand (..)
  , patchField
  , patchField'
  , inContext
  , requireTeacher
#ifdef WITH_AESON
  , injectLockHolder
  , migrateSnapshotLocks
#endif
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserRole (..))
import Competences.Document.Id (Id (..), nilId)
import Competences.Document.Session (SessionId, legacySessionId)
import Competences.Document.User (UserId)
import Control.Monad (when)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), fromJSON, object, toJSON, withObject, (.:), (.:?), (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.UUID.Types qualified as UUID
#endif
import Data.Bifunctor (first)
import Data.Binary (Binary)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Optics.Core (A_Lens, LabelOptic (labelOptic), Lens', (&), (.~), (^.))

newtype AffectedUsers = AffectedUsers [UserId]
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

type UpdateResult = Either Text (Document, AffectedUsers)

-- | Context for command execution, carrying the authenticated user and session.
data CommandContext = CommandContext
  { userId :: !UserId
  , sessionId :: !SessionId
  }
  deriving (Eq, Show)

-- | Represents a change from one value to another (for conflict detection in patches)
type Change a = Maybe (a, a)

-- | Generic modify command - can only lock or release with a patch.
-- Lock carries the requesting user and session (validated by server, used
-- by all clients for consistent document state).
data ModifyCommand patch
  = Lock !UserId !SessionId
  | Release !patch
  deriving (Eq, Generic, Show)

-- | Generic entity command - create, delete, or modify.
-- CreateAndLock carries the requesting user and session for the implicit lock.
data EntityCommand a patch
  = Create !a
  | CreateAndLock !a !UserId !SessionId
  | Delete !(Id a)
  | Modify !(Id a) !(ModifyCommand patch)
  deriving (Eq, Generic, Show)

-- Binary instances
instance (Binary patch) => Binary (ModifyCommand patch)
instance (Binary a, Binary patch) => Binary (EntityCommand a patch)

-- JSON instances
#ifdef WITH_AESON
-- | Custom FromJSON for backward compat: old Lock was nullary, new Lock carries UserId + SessionId.
instance (FromJSON patch) => FromJSON (ModifyCommand patch) where
  parseJSON = withObject "ModifyCommand" $ \v -> do
    tag <- v .: "tag" :: Parser Text
    case tag of
      "Lock" -> do
        mContents <- v .:? "contents"
        case mContents of
          Just (uid, sid) -> pure $ Lock uid sid
          Nothing -> pure $ Lock (nilId) legacySessionId
      "Release" -> Release <$> v .: "contents"
      _ -> fail $ "Unknown ModifyCommand tag: " <> show tag

instance (ToJSON patch) => ToJSON (ModifyCommand patch)

-- | Custom FromJSON for backward compat: old CreateAndLock had one field, new has three.
instance (FromJSON a, FromJSON patch) => FromJSON (EntityCommand a patch) where
  parseJSON = withObject "EntityCommand" $ \v -> do
    tag <- v .: "tag" :: Parser Text
    case tag of
      "Create" -> Create <$> v .: "contents"
      "CreateAndLock" -> do
        contents <- v .: "contents"
        -- Try new format [entity, userId, sessionId], fall back to old format (entity only)
        case fromJSON @(a, UserId, SessionId) contents of
          Success (entity, uid, sid) -> pure $ CreateAndLock entity uid sid
          Error _ -> case fromJSON @a contents of
            Success entity -> pure $ CreateAndLock entity nilId legacySessionId
            Error err -> fail $ "Failed to parse CreateAndLock contents: " <> err
      "Delete" -> Delete <$> v .: "contents"
      "Modify" -> do
        (eid, mcmd) <- v .: "contents"
        pure $ Modify eid mcmd
      _ -> fail $ "Unknown EntityCommand tag: " <> show tag

instance (ToJSON a, ToJSON patch) => ToJSON (EntityCommand a patch)
#endif

-- | Apply a change to a single field, checking for conflicts
-- Plain version with explicit field name and lenses
patchField
  :: (Eq a)
  => Text -- Field name for error messages
  -> Lens' e a -- Lens into entity field
  -> Lens' p (Change a) -- Lens into patch field
  -> p
  -> e
  -> Either Text e
patchField fieldName entityLens patchLens p e =
  case p ^. patchLens of
    Nothing -> Right e
    Just (before, after) -> do
      let current = e ^. entityLens
      when (current /= before) $
        Left $ "field '" <> fieldName <> "': conflict detected (current value differs from expected)"
      Right $ e & entityLens .~ after

-- | Syntactic sugar version using type applications with a single label
patchField'
  :: forall name e p a.
     ( Eq a
     , KnownSymbol name
     , LabelOptic name A_Lens e e a a
     , LabelOptic name A_Lens p p (Change a) (Change a)
     )
  => p
  -> e
  -> Either Text e
patchField' = patchField (pack $ symbolVal (Proxy @name)) (labelOptic @name) (labelOptic @name)

-- | Add entity type context to error messages
inContext :: Text -> a -> (a -> Either Text a) -> Either Text a
inContext entityType entity cmd = first (\err -> entityType <> ": " <> err) $ cmd entity

-- | Require that the acting user is a teacher.
-- Returns Left if the user is not found or is not a teacher.
requireTeacher :: UserId -> Document -> Either Text ()
requireTeacher userId doc =
  case Ix.getOne (doc.users Ix.@= userId) of
    Nothing -> Left "User not found"
    Just u -> when (u.role /= Teacher) $ Left "Only teachers can perform this action"

#ifdef WITH_AESON
-- | Inject userId + legacySessionId into v1 Lock and CreateAndLock commands.
-- V1 Lock was nullary, V1 CreateAndLock had only the entity.
-- Recursively walks the JSON tree to handle commands at any nesting depth.
injectLockHolder :: UserId -> Value -> Value
injectLockHolder uid = go
  where
    uidJson = toJSON (UUID.toText uid.unId)
    sidJson = toJSON (UUID.toText legacySessionId.unId)
    go (Object obj) = Object $ case KM.lookup "tag" obj of
      -- V1 Lock: {"tag":"Lock"} → {"tag":"Lock","contents":[uid,sid]}
      Just (String "Lock")
        | Nothing <- KM.lookup "contents" obj ->
            KM.insert "contents" (toJSON (uidJson, sidJson)) obj
      -- V1 CreateAndLock: {"tag":"CreateAndLock","contents":entity}
      -- → {"tag":"CreateAndLock","contents":[entity,uid,sid]}
      Just (String "CreateAndLock")
        | Just entityVal <- KM.lookup "contents" obj
        , not (isArray entityVal) ->
            KM.insert "contents" (toJSON (entityVal, uidJson, sidJson)) obj
      _ -> fmap go obj
    go (Array arr) = Array $ fmap go arr
    go other = other

    isArray (Array _) = True
    isArray _ = False

-- | Migrate locks in a v2 snapshot from [(Lock, UserId)] to [(Lock, LockHolder)].
-- Transforms bare UUID strings into LockHolder objects with legacySessionId.
migrateSnapshotLocks :: Value -> Value
migrateSnapshotLocks (Object docObj) = case KM.lookup "locks" docObj of
  Just locksVal -> Object $ KM.insert "locks" (migrateLockList locksVal) docObj
  Nothing -> Object docObj
  where
    sidText = UUID.toText legacySessionId.unId
    migrateLockList (Array locks) = Array $ fmap migrateLockPair locks
    migrateLockList other = other
    migrateLockPair (Array pair) = Array $ fmap migrateValue pair
    migrateLockPair other = other
    migrateValue (String uidText) = object ["userId" .= uidText, "sessionId" .= sidText]
    migrateValue other = other
migrateSnapshotLocks other = other
#endif
