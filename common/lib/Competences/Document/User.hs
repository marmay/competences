{-# LANGUAGE CPP #-}

module Competences.Document.User
  ( User (..)
  , UserId
  , UserIxs
  , UserRole (..)
  , Office365Id (..)
  , EntraOid (..)
  , isStudent
  , isTeacher
  )
where

import Competences.Document.Id (Id)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import GHC.Generics (Generic)

type UserId = Id User

-- | Office365 sign-in address (the lowercased user principal name).
-- The human-readable provisioning matcher: teachers create users by
-- address, and the first login binds the account (see 'EntraOid').
newtype Office365Id = Office365Id Text
  deriving (Eq, Generic, Ord, Show)
#ifdef WITH_AESON
  deriving newtype (Binary, FromJSON, ToJSON)
#else
  deriving newtype (Binary)
#endif

-- | Immutable Entra directory object id — the durable identity key.
-- Bound lazily: 'Nothing' until the user's first login matches by
-- address and backfills it.
newtype EntraOid = EntraOid Text
  deriving (Eq, Generic, Ord, Show)
#ifdef WITH_AESON
  deriving newtype (Binary, FromJSON, ToJSON)
#else
  deriving newtype (Binary)
#endif

data UserRole
  = Teacher
  | Student
  deriving (Eq, Generic, Ord, Read, Show, Enum, Bounded)

-- | Information about a User (Teacher or Student).
data User = User
  { id :: !UserId
  -- ^ Unique identifier for the user.
  , name :: !Text
  -- ^ Display name of the user.
  , role :: !UserRole
  -- ^ User's role (Teacher or Student).
  , office365Id :: !Office365Id
  -- ^ Office365 sign-in address. Empty string for local/test users.
  , entraOid :: !(Maybe EntraOid)
  -- ^ Entra object id, bound on first login (identity key once set).
  }
  deriving (Eq, Generic, Ord, Show)

isStudent :: User -> Bool
isStudent = (== Student) . (.role)

isTeacher :: User -> Bool
isTeacher = (== Teacher) . (.role)

type UserIxs = '[UserId, Text, UserRole, Office365Id, EntraOid]

instance Ix.Indexable UserIxs User where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.name))
      (Ix.ixFun $ singleton . (.role))
      (Ix.ixFun $ singleton . (.office365Id))
      (Ix.ixFun $ maybeToList . (.entraOid))

#ifdef WITH_AESON
instance FromJSON UserRole

instance ToJSON UserRole
#endif

instance Binary UserRole

#ifdef WITH_AESON
instance FromJSON User

instance ToJSON User
#endif

instance Binary User
