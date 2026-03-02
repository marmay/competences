module Competences.Frontend.Component.ConnectionStatus
  ( connectionStatusView
  )
where

import Competences.Frontend.Common.Translate (Label (..), translate')
import Competences.Frontend.SyncContext (SyncContext, getCommandSender)
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Color.Status (Status (..), statusPalette)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Notification (notificationBanner)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Competences.Frontend.WebSocket.CommandSender
  ( ConnectionChange (..)
  , ConnectionState (..)
  , subscribeConnection
  )
import Control.Concurrent (forkIO, threadDelay)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~))

-- | Connection status indicator for nav bar
connectionStatusView :: SyncContext -> M.View p a
connectionStatusView ir = inlineComponent "connection-status" (connectionStatusComponent ir)

data NotificationPhase
  = NpIdle
  | NpWaiting !Int -- 10s timer running; Int = generation to ignore stale timers
  | NpStuck -- Warning shown: changes stuck
  | NpSynced -- "Back in sync" shown (auto-dismisses)
  deriving (Eq, Generic, Show)

data Model = Model
  { connectionState :: !ConnectionState
  , pendingCount :: !Int
  , notificationPhase :: !NotificationPhase
  , timerGeneration :: !Int
  }
  deriving (Eq, Generic, Show)

data Action
  = ConnectionChanged !ConnectionChange
  | PendingTimeout !Int -- 10s timer fired (carries generation)
  | DismissNotification -- Auto-dismiss synced notification
  deriving (Eq, Show)

connectionStatusComponent :: SyncContext -> M.Component p Model Action
connectionStatusComponent ir =
  (M.component model update view)
    { M.subs = [subscribeConnection (getCommandSender ir) ConnectionChanged]
    }
  where
    model = Model Disconnected 0 NpIdle 0

    update (ConnectionChanged change) = do
      m <- M.get
      M.modify $ \m' ->
        m' & #connectionState .~ change.state
          & #pendingCount .~ change.pendingCount
      -- Run notification state machine transitions
      let newPending = change.pendingCount
      case m.notificationPhase of
        NpIdle
          | newPending > 0 -> do
              let gen = m.timerGeneration + 1
              M.modify $ \m' ->
                m' & #notificationPhase .~ NpWaiting gen
                  & #timerGeneration .~ gen
              M.withSink $ \sink -> do
                _ <- forkIO $ do
                  threadDelay 10_000_000 -- 10 seconds
                  sink (PendingTimeout gen)
                pure ()
          | otherwise -> pure ()
        NpWaiting _
          | newPending == 0 ->
              M.modify $ #notificationPhase .~ NpIdle
          | otherwise -> pure ()
        NpStuck
          | newPending == 0 -> do
              M.modify $ #notificationPhase .~ NpSynced
              M.withSink $ \sink -> do
                _ <- forkIO $ do
                  threadDelay 3_000_000 -- 3 seconds
                  sink DismissNotification
                pure ()
          | otherwise -> pure ()
        NpSynced -> pure ()
    update (PendingTimeout gen) =
      M.modify $ \m -> case m.notificationPhase of
        NpWaiting g | g == gen -> m & #notificationPhase .~ NpStuck
        _ -> m
    update DismissNotification =
      M.modify $ \m -> case m.notificationPhase of
        NpSynced -> m & #notificationPhase .~ NpIdle
        _ -> m

    view m =
      M.div_ []
        [ withTooltip (PlainTooltip (tooltipText m)) $
            connectionIcon m.connectionState m.pendingCount
        , notificationView m
        ]

-- | Icon based on connection state and pending count
connectionIcon :: ConnectionState -> Int -> M.View model action
connectionIcon Connected 0 = Icon.iconFull Icon.OnPrimary Icon.Large Icon.Static Icon.IcnCloudCheck
connectionIcon Connected _ = Icon.iconFull Icon.OnPrimary Icon.Large Icon.Pulse Icon.IcnCloudSync
connectionIcon Disconnected _ = Icon.iconFull Icon.OnPrimary Icon.Large Icon.Pulse Icon.IcnCloudOff

-- | Notification banner based on notification phase
notificationView :: Model -> M.View Model Action
notificationView m = case m.notificationPhase of
  NpStuck ->
    notificationBanner (statusPalette Pending)
      [ Icon.iconVS Icon.Primary Icon.Small Icon.IcnWarning
      , M.span_ [class_ "text-sm font-medium"] [M.text (translate' LblChangesPendingWarning)]
      ]
  NpSynced ->
    notificationBanner (statusPalette Ok)
      [ Icon.iconVS Icon.Primary Icon.Small Icon.IcnCloudCheck
      , M.span_ [class_ "text-sm font-medium"] [M.text (translate' LblChangesNowSynced)]
      ]
  _ -> M.text ""

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
