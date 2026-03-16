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
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserRole (..))
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
import Control.Monad (when)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
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

-- | Generic modify command - can only lock or release with a patch
data ModifyCommand patch
  = Lock
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
instance (FromJSON patch) => FromJSON (ModifyCommand patch)
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
