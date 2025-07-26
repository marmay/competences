module Competences.Frontend.App.Action
  ( Action (..)
  , ComponentAction (..)
  , UiAction (..)
  )
where

import Competences.Frontend.App.RegisteredComponent (RegisteredComponent)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Miso.String (MisoString)
import Competences.Command (CommandId, Command)

data Action
  = ComponentAction !ComponentAction
  | Process !(CommandId, Command)
  | LogError !MisoString
  | Initialize
  deriving (Eq, Show, Generic)

data UiAction
  = PushModal !RegisteredComponent
  | PopModal
  | SetMain !RegisteredComponent
  deriving (Eq, Show, Generic)

data ComponentAction
  = Trigger !Command
  | ChangeUi !UiAction
  deriving (Eq, Show, Generic)

instance ToJSON Action

instance FromJSON Action

instance ToJSON UiAction

instance FromJSON UiAction

instance ToJSON ComponentAction

instance FromJSON ComponentAction
