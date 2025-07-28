module Competences.Frontend.Grid.Action
  ( Action (..)
  )
where

import Competences.Model.ChangableField (ChangableField)
import Miso.String (MisoString)
import Competences.Frontend.Document (ModelChange)
import Competences.Command (Command)

data Action where
  EditField :: !ChangableField -> !MisoString -> Action
  UpdateModel :: !ModelChange -> Action
  IssueCommand :: !Command -> Action
  NewCompetence :: Action
  CancelNewCompetence :: Action
  AddNewCompetence :: Action
  SetNewCompetenceDescription :: !MisoString -> Action
