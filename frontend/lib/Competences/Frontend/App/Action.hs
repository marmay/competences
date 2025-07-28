module Competences.Frontend.App.Action
  ( Action (..)
  , UiAction (..)
  )
where

import Competences.Frontend.App.RegisteredComponent (RegisteredComponent)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Miso.String (MisoString)

data Action
  = ChangeUi !UiAction
  | LogError !MisoString
  | Initialize
  deriving (Eq, Show, Generic)

data UiAction
  = PushModal !RegisteredComponent
  | PopModal
  | SetMain !RegisteredComponent
  deriving (Eq, Show, Generic)

instance ToJSON Action

instance FromJSON Action

instance ToJSON UiAction

instance FromJSON UiAction
