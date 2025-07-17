module Competences.Frontend.State.Edit
  (
  )
  where

data Editability
  = Editable
  | LockedBy !UserId
  | LockedByUs !M.MisoString
  | NotEditable
