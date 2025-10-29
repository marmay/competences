module Competences.Command.Common
  ( AffectedUsers (..)
  , UpdateResult
  )
where

import Competences.Document (Document)
import Competences.Document.User (UserId)
import Data.Text (Text)

newtype AffectedUsers = AffectedUsers [UserId]
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

type UpdateResult = Either Text (Document, AffectedUsers)
