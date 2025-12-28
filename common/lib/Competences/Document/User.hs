module Competences.Document.User
  ( User (..)
  , UserId
  , UserIxs
  , UserRole (..)
  , Office365Id (..)
  , isStudent
  , isTeacher
  )
where

import Competences.Document.Id (Id)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import GHC.Generics (Generic)

type UserId = Id User

-- | Office365 user identifier for authentication.
newtype Office365Id = Office365Id Text
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON, Binary)

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
  -- ^ Office365 user ID for authentication. Empty string for local/test users.
  }
  deriving (Eq, Generic, Ord, Show)

isStudent :: User -> Bool
isStudent = (== Student) . (.role)

isTeacher :: User -> Bool
isTeacher = (== Teacher) . (.role)

type UserIxs = '[UserId, Text, UserRole, Office365Id]

instance Ix.Indexable UserIxs User where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.name))
      (Ix.ixFun $ singleton . (.role))
      (Ix.ixFun $ singleton . (.office365Id))

instance FromJSON UserRole

instance ToJSON UserRole

instance Binary UserRole

instance FromJSON User

instance ToJSON User

instance Binary User
