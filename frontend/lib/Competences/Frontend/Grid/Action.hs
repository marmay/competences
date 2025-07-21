module Competences.Frontend.Grid.Action
  ( Action (..)
  )
where

import Competences.Command (Command, CommandId)
import Competences.Model.ChangableField (ChangableField)
import Miso.String (MisoString)
import Miso (SomeComponent)
import Competences.Frontend.Grid.State (State)

data Action
  = Trigger !Command
  | Process !(CommandId, Command)
  | EditField !ChangableField !MisoString
  | PushModal !(State -> SomeComponent)
  | Refresh
  | LoadEmptyModel
  | Init ![Action]
