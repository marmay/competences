module Competences.Protocol
  ( ClientMessage (..)
  , ServerMessage (..)
  )
where

import Competences.Command (Command)
import Competences.Document (Document, User)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Messages sent from client to server over WebSocket.
data ClientMessage
  = -- | Send a command to be validated and applied by the server.
    SendCommand !Command
  | -- | Keep-alive ping to prevent connection timeout.
    KeepAlive
  deriving (Eq, Generic, Show)

instance FromJSON ClientMessage

instance ToJSON ClientMessage

-- | Messages sent from server to client over WebSocket.
data ServerMessage
  = -- | Initial document snapshot sent upon connection establishment.
    -- Includes the authenticated user making the connection.
    InitialSnapshot !Document !User
  | -- | Command successfully applied by server (echo or broadcast).
    -- Client should apply to remoteDocument and replay localChanges.
    ApplyCommand !Command
  | -- | Command rejected by server during validation.
    -- Contains the full rejected command for robust matching and cleanup.
    CommandRejected !Command !Text
  | -- | Response to KeepAlive ping.
    KeepAliveResponse
  deriving (Eq, Generic, Show)

instance FromJSON ServerMessage

instance ToJSON ServerMessage
