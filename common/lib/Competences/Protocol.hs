{-# LANGUAGE CPP #-}

module Competences.Protocol
  ( ClientMessage (..)
  , ServerMessage (..)
  , ClientInfo (..)
  , ServerInfo (..)
  , CommandVersion
  , CommandId
  )
where

import Competences.Command (Command, CommandContext (..))
import Competences.Document (Document, User, UserId)
import Competences.Document.FileRef (FileData, FileRef, SHA256Hash)
import Competences.Document.Id (Id)
import Competences.Document.Session (SessionId)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Phantom type for command version identifiers.
-- CommandId is the UUID assigned to each command when persisted to the database.
data CommandVersion

-- | Unique identifier for a persisted command (UUID-based).
-- This is the database's command_id, assigned on save.
type CommandId = Id CommandVersion

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
    -- The Maybe UserId is for teacher impersonation.
    Authenticate !Text !ClientInfo !SessionId !(Maybe UserId)
  | -- | Subscribe from a given command version.
    -- Nothing = fresh client (send full snapshot).
    -- Just commandId = incremental from this point.
    -- Also used as ACK after receiving a sync/update.
    SubscribeFrom !(Maybe CommandId)
  | -- | Send a command to be validated and applied by the server.
    SendCommand !Command
  | -- | Keep-alive ping to prevent connection timeout.
    KeepAlive
  | -- | Request a file from the CAS by its content hash.
    RequestFile !SHA256Hash
  | -- | Upload a file to the CAS.
    -- Fields: fileName, mimeType, file contents.
    UploadFile !Text !Text !FileData
  | -- | Request permission to upload a file.
    -- Fields: fileName, mimeType, fileSize.
    RequestUploadPermission !Text !Text !Int64
  | -- | Query whether a session is still active (has WebSocket connections).
    -- Used by clients to decide if a lock can be stolen.
    QuerySessionAlive !SessionId
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
  | -- | Authentication succeeded. Contains the authenticated user and server info.
    Authenticated !User !ServerInfo
  | -- | Full snapshot at a command version, with optional checksum for persistence.
    SnapshotUpdate !CommandId !Document !(Maybe Text)
  | -- | Batch of commands up to a version, with optional checksum.
    -- The CommandId is the ID of the last command in the batch.
    -- Each command is paired with the context that originally issued it.
    CommandUpdate !CommandId ![(CommandContext, Command)] !(Maybe Text)
  | -- | Command rejected by server during validation.
    -- Contains the full rejected command for robust matching and cleanup.
    CommandRejected !Command !Text
  | -- | Response to KeepAlive ping.
    KeepAliveResponse
  | -- | File contents from the CAS (response to RequestFile).
    FileContents !SHA256Hash !FileData
  | -- | Requested file was not found in the CAS.
    FileNotFound !SHA256Hash
  | -- | File upload succeeded, returning the FileRef with hash and metadata.
    FileUploaded !FileRef
  | -- | File upload failed with an error message.
    FileUploadFailed !Text
  | -- | Server grants permission to proceed with upload.
    UploadPermitted
  | -- | Server denies upload with reason.
    UploadDenied !Text
  | -- | Response to QuerySessionAlive: whether the session has active connections.
    SessionAliveResponse !SessionId !Bool
  deriving (Eq, Generic, Show)

instance Binary ServerMessage

#ifdef WITH_AESON
instance FromJSON ServerMessage

instance ToJSON ServerMessage
#endif
