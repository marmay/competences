module Competences.Command.Evidences
  ( EvidencesCommand (..)
  , EvidencePatch (..)
  )
where

import Competences.Command.Common (Change, EntityCommand)
import Competences.Common.IxSet qualified as Ix
import Competences.Document.Evidence
  ( ActivityType
  , Evidence
  , Observation
  , ObservationIxs
  )
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)

-- | Patch for modifying an Evidence (only editable fields)
data EvidencePatch = EvidencePatch
  { userIds :: !(Change (Set UserId))
    -- ^ Change userIds from old to new value
  , activityType :: !(Change ActivityType)
    -- ^ Change activityType from old to new value
  , date :: !(Change Day)
    -- ^ Change date from old to new value
  , tasks :: !(Change [TaskId])
    -- ^ Change tasks from old to new value
  , oldTasks :: !(Change (Maybe Text))
    -- ^ Change oldTasks from old to new value (Maybe Maybe for setting to Nothing)
  , observations :: !(Change (Ix.IxSet ObservationIxs Observation))
    -- ^ Change observations from old to new value
  }
  deriving (Eq, Generic, Show)

-- | Commands for the Evidences context
data EvidencesCommand
  = OnEvidences !(EntityCommand Evidence EvidencePatch)
  deriving (Eq, Generic, Show)

-- JSON instances
instance FromJSON EvidencePatch
instance ToJSON EvidencePatch
instance Binary EvidencePatch

instance FromJSON EvidencesCommand
instance ToJSON EvidencesCommand
instance Binary EvidencesCommand
