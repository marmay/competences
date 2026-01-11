module Competences.Frontend.Component.ConnectionStatus
  ( connectionStatusView
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.SyncContext (SyncContext, getCommandSender)
import Competences.Frontend.WebSocket.CommandSender
  ( ConnectionChange (..)
  , ConnectionState (..)
  , subscribeConnection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (TooltipPosition (..), withTooltipPosition)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~))

-- | Connection status indicator for footer
connectionStatusView :: SyncContext -> M.View p a
connectionStatusView ir = V.component "connection-status" (connectionStatusComponent ir)

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
      withTooltipPosition TooltipLeft (tooltipText m) $
        M.div_
          [class_ "flex items-center gap-1.5 px-2 py-1 rounded-full cursor-default"]
          [ M.span_ [class_ $ "w-2.5 h-2.5 rounded-full " <> dotClasses m.connectionState] []
          , M.text $ if m.pendingCount > 0 then M.ms (show m.pendingCount) else ""
          ]

-- | Dot color and animation classes based on connection state
dotClasses :: ConnectionState -> Text
dotClasses Connected = "bg-green-500"
dotClasses Disconnected = "bg-red-500 animate-pulse"

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
