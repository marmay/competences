{-# LANGUAGE CPP #-}

module Competences.Command.Common
  ( AffectedUsers (..)
  , UpdateResult
  , Change
  , ModifyCommand (..)
  , EntityCommand (..)
  , patchField
  , patchField'
  , inContext
  , requireTeacher
#ifdef WITH_AESON
  , injectLockHolder
#endif
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserRole (..))
import Competences.Document.Id (Id)
import Competences.Document.Id qualified
import Competences.Document.Session (SessionId, legacySessionId)
import Competences.Document.User (UserId)
import Control.Monad (when)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), toJSON, withObject, (.:), (.:?))
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

-- | Represents a change from one value to another (for conflict detection in patches)
type Change a = Maybe (a, a)

-- | Generic modify command - can only lock or release with a patch.
-- Lock carries the requesting user and session (validated by server, used
-- by all clients for consistent document state).
data ModifyCommand patch
  = Lock !UserId !SessionId
  | Release !patch
  deriving (Eq, Generic, Show)

-- | Generic entity command - create, delete, or modify
data EntityCommand a patch
  = Create !a
  | CreateAndLock !a
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
          Nothing -> pure $ Lock (Competences.Document.Id.nilId) legacySessionId
      "Release" -> Release <$> v .: "contents"
      _ -> fail $ "Unknown ModifyCommand tag: " <> show tag

instance (ToJSON patch) => ToJSON (ModifyCommand patch)

instance (FromJSON a, FromJSON patch) => FromJSON (EntityCommand a patch)
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
-- | Inject userId + legacySessionId into v1 Lock commands.
-- V1 Lock was nullary: {"tag":"Lock"} — transforms to {"tag":"Lock","contents":[userId, sessionId]}.
-- Recursively walks the JSON tree to handle Lock at any nesting depth.
injectLockHolder :: UserId -> Value -> Value
injectLockHolder uid = go
  where
    uidText = UUID.toText uid.unId
    sidText = UUID.toText legacySessionId.unId
    go (Object obj)
      | KM.lookup "tag" obj == Just (String "Lock")
      , Nothing <- KM.lookup "contents" obj =
          Object $ KM.insert "contents" (toJSON (uidText, sidText)) obj
      | otherwise = Object $ fmap go obj
    go (Array arr) = Array $ fmap go arr
    go other = other
#endif
