{-# LANGUAGE CPP #-}

-- | Structured intermediate representation for round-trip export/import
-- of teaching content across instances.
--
-- The 'ExchangeDoc' value is the wire-side contract: it has a 'Binary'
-- instance used for WebSocket transport between frontend and backend,
-- and (when built with the @aeson@ flag) JSON instances that the
-- backend feeds into the @yaml@ library to produce the clipboard-side
-- YAML payload. The frontend never parses YAML — it builds/consumes
-- 'ExchangeDoc' values directly.
module Competences.Exchange.Types
  ( ExchangeDoc (..)
  , ExchangeAssignment (..)
  , ExchangeTask (..)
  , ExchangeSolution (..)
  , ExchangeAttachment (..)
  , ExchangeCompetenceRef (..)
  , exchangeFormatVersion
  )
where

import Competences.Document.ActivityType (ActivityType)
import Competences.Document.Competence (Level)
import Competences.Document.Solution (SolutionType)
import Competences.Document.Task (TaskPurpose)
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif

-- | Bump when the wire format changes in a way that breaks
-- backward-compatible parsing. Currently @1@.
exchangeFormatVersion :: Int
exchangeFormatVersion = 1

-- | Top-level exchange document, one per clipboard payload. The
-- variant discriminator survives the YAML round-trip as the
-- @type@ field at top level (e.g. @type: assignment@).
data ExchangeDoc
  = ExchangeAssignmentDoc !ExchangeAssignment
  deriving (Eq, Generic, Show)

instance Binary ExchangeDoc

#ifdef WITH_AESON
instance FromJSON ExchangeDoc
instance ToJSON ExchangeDoc
#endif

-- | Assignment payload, including nested tasks and solutions.
-- Competence references carry grid title + description + level so
-- the importing instance can resolve them against its own grid.
data ExchangeAssignment = ExchangeAssignment
  { name :: !Text
  , description :: !Text
  , assignmentDate :: !Day
  , activityType :: !ActivityType
  , isDraft :: !Bool
  , groupSubmissionAllowed :: !Bool
  , tasks :: ![ExchangeTask]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeAssignment

#ifdef WITH_AESON
instance FromJSON ExchangeAssignment
instance ToJSON ExchangeAssignment
#endif

-- | Task payload. Content, primary/secondary competences, solutions
-- and attachments all travel together.
data ExchangeTask = ExchangeTask
  { identifier :: !Text
  , title :: !Text
  , content :: !(Maybe Text)
  , purpose :: !TaskPurpose
  , primary :: ![ExchangeCompetenceRef]
  , secondary :: ![ExchangeCompetenceRef]
  , solutions :: ![ExchangeSolution]
  , attachments :: ![ExchangeAttachment]
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeTask

#ifdef WITH_AESON
instance FromJSON ExchangeTask
instance ToJSON ExchangeTask
#endif

data ExchangeSolution = ExchangeSolution
  { solutionType :: !SolutionType
  , content :: !Text
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeSolution

#ifdef WITH_AESON
instance FromJSON ExchangeSolution
instance ToJSON ExchangeSolution
#endif

-- | Attachment metadata. Metadata-only by default — same-server
-- imports resolve the blob via the shared CAS by 'sha256'. A later
-- revision adds an optional embedded content field for cross-server
-- sharing.
data ExchangeAttachment = ExchangeAttachment
  { fileName :: !Text
  , mimeType :: !Text
  , sha256 :: !Text
  , bytes :: !Int64
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeAttachment

#ifdef WITH_AESON
instance FromJSON ExchangeAttachment
instance ToJSON ExchangeAttachment
#endif

-- | Competence reference by grid title + competence description +
-- level. The importing instance matches these against its own
-- competence grids; unmatched references are surfaced in the preview.
data ExchangeCompetenceRef = ExchangeCompetenceRef
  { grid :: !Text
  , description :: !Text
  , level :: !Level
  }
  deriving (Eq, Generic, Show)

instance Binary ExchangeCompetenceRef

#ifdef WITH_AESON
instance FromJSON ExchangeCompetenceRef
instance ToJSON ExchangeCompetenceRef
#endif
