module Competences.Frontend.State.Edit
  ( Editability (..)
  -- , editability
  )
where

import Competences.Command
import Competences.Model (Model)
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.User (UserId, UserRole)
import Data.Map qualified as M
import Miso.String (MisoString)

data Editability
  = Editable
  | LockedBy !UserId
  | LockedByUs !MisoString
  | NotEditable

type EditsState = M.Map ChangableField MisoString

-- editability :: UserRole -> Model -> EditsState -> ChangableField -> Editability
-- editability r m s f
--   | r /= TeacherRole = NotEditable
--   | otherwise =
