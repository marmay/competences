{-# LANGUAGE CPP #-}

module Competences.Protocol
  ( ClientMessage (..)
  , ServerMessage (..)
  , ClientInfo (..)
  , ServerInfo (..)
  )
where

import Competences.Command (Command)
import Competences.Document (Document, User)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Version information sent by the frontend during authentication.
data ClientInfo = ClientInfo
  { frontendVersion :: !Text
  }
  deriving (Eq, Generic, Show)

instance Binary ClientInfo

#ifdef WITH_AESON
instance FromJSON ClientInfo

instance ToJSON ClientInfo
#endif

-- | Version information sent by the backend with the initial snapshot.
data ServerInfo = ServerInfo
  { backendVersion :: !Text
  }
  deriving (Eq, Generic, Show)

instance Binary ServerInfo

#ifdef WITH_AESON
instance FromJSON ServerInfo

instance ToJSON ServerInfo
#endif

-- | Messages sent from client to server over WebSocket.
data ClientMessage
  = -- | Authenticate with JWT token (must be first message after connection).
    -- Removes token from URL to prevent logging in server logs, browser history, etc.
    -- Includes client version information for compatibility checking.
    Authenticate !Text !ClientInfo
  | -- | Send a command to be validated and applied by the server.
    SendCommand !Command
  | -- | Keep-alive ping to prevent connection timeout.
    KeepAlive
  deriving (Eq, Generic, Show)

instance Binary ClientMessage

#ifdef WITH_AESON
instance FromJSON ClientMessage

instance ToJSON ClientMessage
#endif

-- | Messages sent from server to client over WebSocket.
data ServerMessage
  = -- | Authentication failed - connection will be closed after this message.
    AuthenticationFailed !Text
  | -- | Initial document snapshot sent upon successful authentication.
    -- Includes the authenticated user and server version information.
    InitialSnapshot !Document !User !ServerInfo
  | -- | Command successfully applied by server (echo or broadcast).
    -- Client should apply to remoteDocument and replay localChanges.
    ApplyCommand !Command
  | -- | Command rejected by server during validation.
    -- Contains the full rejected command for robust matching and cleanup.
    CommandRejected !Command !Text
  | -- | Response to KeepAlive ping.
    KeepAliveResponse
  deriving (Eq, Generic, Show)

instance Binary ServerMessage

#ifdef WITH_AESON
instance FromJSON ServerMessage

instance ToJSON ServerMessage
#endif
