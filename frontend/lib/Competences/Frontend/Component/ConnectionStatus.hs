module Competences.Frontend.Component.ConnectionStatus
  ( connectionStatusView
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.SyncContext (SyncContext, getCommandSender)
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Competences.Frontend.WebSocket.CommandSender
  ( ConnectionChange (..)
  , ConnectionState (..)
  , subscribeConnection
  )
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~))

-- | Connection status indicator for footer
connectionStatusView :: SyncContext -> M.View p a
connectionStatusView ir = component "connection-status" (connectionStatusComponent ir)

data Model = Model
  { connectionState :: !ConnectionState
  , pendingCount :: !Int
  }
  deriving (Eq, Generic, Show)

newtype Action = ConnectionChanged ConnectionChange
  deriving (Eq, Show)

connectionStatusComponent :: SyncContext -> M.Component p Model Action
connectionStatusComponent ir =
  (M.component model update view)
    { M.subs = [subscribeConnection (getCommandSender ir) ConnectionChanged]
    }
  where
    model = Model Disconnected 0

    update (ConnectionChanged change) =
      M.modify $ \m ->
        m & #connectionState .~ change.state
          & #pendingCount .~ change.pendingCount

    view m =
      withTooltip (PlainTooltip (tooltipText m)) $
        connectionIcon m.connectionState m.pendingCount

-- | Icon based on connection state and pending count
connectionIcon :: ConnectionState -> Int -> M.View model action
connectionIcon Connected 0 = Icon.iconFull Icon.OnPrimary Icon.Large Icon.Static Icon.IcnCloudCheck
connectionIcon Connected _ = Icon.iconFull Icon.OnPrimary Icon.Large Icon.Pulse Icon.IcnCloudSync
connectionIcon Disconnected _ = Icon.iconFull Icon.OnPrimary Icon.Large Icon.Pulse Icon.IcnCloudOff

-- | Tooltip text based on state and pending count
tooltipText :: Model -> M.MisoString
tooltipText Model{connectionState, pendingCount} =
  stateText <> pendingText
  where
    stateText = case connectionState of
      Connected -> translate' LblConnected
      Disconnected -> translate' LblDisconnected

    pendingText
      | pendingCount == 0 = ""
      | otherwise = " - " <> changesText

    changesText = case connectionState of
      Connected -> translate' (LblPendingChanges pendingCount)
      _ -> translate' (LblUnsavedChanges pendingCount)
