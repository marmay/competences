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
  = -- | Authenticate with JWT token (must be first message after connection).
    -- Removes token from URL to prevent logging in server logs, browser history, etc.
    Authenticate !Text
  | -- | Send a command to be validated and applied by the server.
    SendCommand !Command
  | -- | Keep-alive ping to prevent connection timeout.
    KeepAlive
  deriving (Eq, Generic, Show)

instance FromJSON ClientMessage

instance ToJSON ClientMessage

-- | Messages sent from server to client over WebSocket.
data ServerMessage
  = -- | Authentication failed - connection will be closed after this message.
    AuthenticationFailed !Text
  | -- | Initial document snapshot sent upon successful authentication.
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
