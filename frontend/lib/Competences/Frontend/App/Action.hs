module Competences.Frontend.App.Action
  ( Action(..)
  , UiAction(..)
  )
  where

import Competences.Frontend.App.ComponentRegistry (ModalComponent, GridComponent, SideBarComponent)
import Competences.Command (Command, CommandId)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Action
  = Trigger !Command
  | Process !(CommandId, Command)
  | ChangeUi !UiAction
  deriving (Eq, Show, Generic)

data UiAction
  = PushModal !ModalComponent
  | PopModal
  | SetGrid !GridComponent
  | SetSideBar !SideBarComponent
  deriving (Eq, Show, Generic)

instance ToJSON Action
instance FromJSON Action

instance ToJSON UiAction
instance FromJSON UiAction
