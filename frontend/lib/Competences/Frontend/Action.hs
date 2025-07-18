module Competences.Frontend.Action
  ( Action (..)
  )
where

import Competences.Command (Command, CommandId)

data Action
  = Trigger !Command
  | Process !(CommandId, Command)
  | Refresh
  | LoadEmptyModel
