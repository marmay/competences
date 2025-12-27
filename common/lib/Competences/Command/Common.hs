module Competences.Command.Common
  ( AffectedUsers (..)
  , UpdateResult
  , Change
  , ModifyCommand (..)
  , EntityCommand (..)
  )
where

import Competences.Document (Document)
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

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
  | Delete !(Id a)
  | Modify !(Id a) !(ModifyCommand patch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance (FromJSON patch) => FromJSON (ModifyCommand patch)
instance (ToJSON patch) => ToJSON (ModifyCommand patch)
instance (Binary patch) => Binary (ModifyCommand patch)

instance (FromJSON a, FromJSON patch) => FromJSON (EntityCommand a patch)
instance (ToJSON a, ToJSON patch) => ToJSON (EntityCommand a patch)
instance (Binary a, Binary patch) => Binary (EntityCommand a patch)
