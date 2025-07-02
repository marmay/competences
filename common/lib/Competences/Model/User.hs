module Competences.Model.User
  ( User (..)
  , UserId (..)
  , UserIxs
  , UserRole (..)
  )
where

import Data.IxSet.Typed qualified as Ix
import Data.List (singleton)
import Data.Text (Text)
import Data.UUID (UUID)

-- | Unique identifier for a Student.
newtype UserId = UserId UUID
  deriving (Eq, Ord, Show)

data UserRole
  = Teacher
  | Student
  deriving (Eq, Ord, Show)

-- | Information about a Student.
data User = User
  { id :: !UserId
  -- ^ Unique identifier for the student.
  , firstName :: !Text
  -- ^ First name of the student.
  , lastName :: !Text
  -- ^ Last name of the student.
  , role :: !UserRole
  }
  deriving (Eq, Ord, Show)

type UserIxs = '[UserId, UserRole]

instance Ix.Indexable UserIxs User where
  indices =
    Ix.ixList
      (Ix.ixFun $ singleton . (.id))
      (Ix.ixFun $ singleton . (.role))
