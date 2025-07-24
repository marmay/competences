module Competences.Command.Common
  ( AffectedUsers (..)
  , UpdateResult
  )
where

import Competences.Model.User (UserId)
import Data.Text (Text)
import Competences.Model (Model)

data AffectedUsers
  = AllUsers
  | AllTeachers
  | AllTeachersAndSpecificStudents ![UserId]
  deriving (Eq, Show)

type UpdateResult = Either Text (Model, AffectedUsers)

