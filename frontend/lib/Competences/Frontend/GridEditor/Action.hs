module Competences.Frontend.GridEditor.Action
  ( Action (..)
  )
where

import Competences.Command (Command)
import Competences.Document.ChangableField (ChangableField)
import Competences.Frontend.SyncDocument (DocumentChange)
import Miso.String (MisoString)

data Action where
  EditField :: !ChangableField -> !MisoString -> Action
  UpdateDocument :: !DocumentChange -> Action
  IssueCommand :: !Command -> Action
  NewCompetence :: Action
  CancelNewCompetence :: Action
  AddNewCompetence :: Action
  SetNewCompetenceDescription :: !MisoString -> Action
