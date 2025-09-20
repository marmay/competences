module Competences.Command.Common
  ( AffectedUsers (..)
  , UpdateResult
  )
where

import Competences.Document (Document)
import Competences.Document.User (UserId)
import Data.Text (Text)

data AffectedUsers
  = AllUsers
  | AllTeachers
  | AllTeachersAndSpecificStudents ![UserId]
  | Nobody
  deriving (Eq, Show)

type UpdateResult = Either Text (Document, AffectedUsers)
