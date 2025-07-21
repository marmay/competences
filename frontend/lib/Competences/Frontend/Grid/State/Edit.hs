module Competences.Frontend.Grid.State.Edit
  ( Editability (..)
  , editability
  )
where

import Competences.Command.ChangeField (canChangeField)
import Competences.Model (Model (..))
import Competences.Model.ChangableField (ChangableField)
import Competences.Model.User (User (..), UserId)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Miso.String (MisoString)

data Editability
  = Editable
  | LockedBy !UserId
  | LockedByUs !MisoString
  | NotEditable

type EditsState = M.Map ChangableField MisoString

editability :: Model -> EditsState -> ChangableField -> User -> Editability
editability m s f u =
  case M.lookup f m.lockedFields of
    Nothing ->
      if canChangeField f u.role
        then Editable
        else NotEditable
    Just lockedUid ->
      if lockedUid == u.id
        then LockedByUs $ fromMaybe "" $ M.lookup f s
        else LockedBy lockedUid
