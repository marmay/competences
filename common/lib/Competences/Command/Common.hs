{-# LANGUAGE CPP #-}

module Competences.Command.Common
  ( UpdateResult
  , Change
  , CommandContext (..)
  , ModifyCommand (..)
  , EntityCommand (..)
  , patchField
  , patchField'
  , inContext
  , requireTeacher
  )
where

import Competences.Command.Audience (CommandAudience)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserRole (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Session (SessionId)
import Competences.Document.User (UserId)
import Control.Monad (when)
#ifdef WITH_AESON
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), fromJSON, withObject, (.:))
import Data.Aeson.Types (Parser)
#endif
import Data.Bifunctor (first)
import Data.Binary (Binary)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Optics.Core (A_Lens, LabelOptic (labelOptic), Lens', (&), (.~), (^.))

type UpdateResult = Either Text (Document, CommandAudience)

-- | Context for command execution, carrying the authenticated user and session.
data CommandContext = CommandContext
  { userId :: !UserId
  , sessionId :: !SessionId
  }
  deriving (Eq, Show, Generic)

instance Binary CommandContext

-- | Represents a change from one value to another (for conflict detection in patches)
type Change a = Maybe (a, a)

-- | Generic modify command - lock or release with a patch.
-- The lock holder identity comes from the CommandContext (envelope),
-- not from the command payload.
data ModifyCommand patch
  = Lock
  | Release !patch
  deriving (Eq, Generic, Show)

-- | Generic entity command - create, delete, or modify.
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
instance FromJSON CommandContext
instance ToJSON CommandContext

-- | Custom FromJSON for backward compat: old Lock carried UserId + SessionId,
-- which are now ignored (identity comes from CommandContext).
instance (FromJSON patch) => FromJSON (ModifyCommand patch) where
  parseJSON = withObject "ModifyCommand" $ \v -> do
    tag <- v .: "tag" :: Parser Text
    case tag of
      "Lock" -> pure Lock
      "Release" -> Release <$> v .: "contents"
      _ -> fail $ "Unknown ModifyCommand tag: " <> show tag

instance (ToJSON patch) => ToJSON (ModifyCommand patch)

-- | Custom FromJSON for backward compat: old CreateAndLock carried UserId + SessionId,
-- which are now ignored (identity comes from CommandContext).
instance (FromJSON a, FromJSON patch) => FromJSON (EntityCommand a patch) where
  parseJSON = withObject "EntityCommand" $ \v -> do
    tag <- v .: "tag" :: Parser Text
    case tag of
      "Create" -> Create <$> v .: "contents"
      "CreateAndLock" -> do
        contents <- v .: "contents"
        -- Handle old format [entity, userId, sessionId] by extracting just the entity
        case fromJSON @(a, UserId, SessionId) contents of
          Success (entity, _, _) -> pure $ CreateAndLock entity
          Error _ -> CreateAndLock <$> parseJSON contents
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

