module Competences.Frontend.Grid.Action
  ( Action (..)
  )
where

import Competences.Frontend.App.Action (ComponentAction)
import Competences.Frontend.App.ComponentRegistry (ChannelId, LoadModelAction (..))
import Competences.Model (Model)
import Competences.Model.ChangableField (ChangableField)
import Miso.String (MisoString)

data Action where
  EditField :: !ChangableField -> !MisoString -> Action
  ChangeApp :: !ComponentAction -> Action
  LoadModel :: !ChannelId -> Action
  UpdateModel :: !Model -> Action

instance LoadModelAction Action where
  loadModelAction = LoadModel
  updateModelAction = UpdateModel
