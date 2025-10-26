module Competences.Document.User
  ( User (..)
  , UserId
  , UserIxs
  , UserRole (..)
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

data UserRole
  = Teacher
  | Student
  deriving (Eq, Generic, Ord, Read, Show, Enum, Bounded)

-- | Information about a Student.
data User = User
  { id :: !UserId
  -- ^ Unique identifier for the student.
  , name :: !Text
  -- ^ Last name of the student.
  , role :: !UserRole
  }
  deriving (Eq, Generic, Ord, Show)

isStudent :: User -> Bool
isStudent = (== Student) . (.role)

isTeacher :: User -> Bool
isTeacher = (== Teacher) . (.role)

type UserIxs = '[UserId, Text, UserRole]

instance Ix.Indexable UserIxs User where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.name))
      (Ix.ixFun $ singleton . (.role))

instance FromJSON UserRole

instance ToJSON UserRole

instance Binary UserRole

instance FromJSON User

instance ToJSON User

instance Binary User
