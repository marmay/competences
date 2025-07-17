module Competences.Frontend.Action
  ( Action (..)
  )
where

import Competences.Event (Event, EventId)

data Action
  = Trigger !Event
  | Process !(EventId, Event)
  | Refresh
  | LoadEmptyModel
