module Competences.Command.Evidences
  ( EvidencesCommand (..)
  , EvidencePatch (..)
  , handleEvidencesCommand
  )
where

import Competences.Command.Common (AffectedUsers (..), Change, EntityCommand, UpdateResult, inContext, patchField')
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Common.IxSet qualified as Ix
import Data.Default (Default (..))
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.Evidence
  ( ActivityType
  , Evidence (..)
  , Observation
  , ObservationIxs
  )
import Competences.Document.Task (TaskId)
import Competences.Document.User (UserId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Optics.Core ((&), (^.))
import Control.Monad ((>=>))

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
  , oldTasks :: !(Change Text)
    -- ^ Change oldTasks from old to new value
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

-- Default instances
instance Default EvidencePatch where
  def =
    EvidencePatch
      { userIds = Nothing
      , activityType = Nothing
      , date = Nothing
      , tasks = Nothing
      , oldTasks = Nothing
      , observations = Nothing
      }

-- | Apply a patch to an Evidence, checking for conflicts
applyEvidencePatch :: Evidence -> EvidencePatch -> Either Text Evidence
applyEvidencePatch evidence patch =
  inContext "Evidence" evidence $
    patchField' @"userIds" patch
      >=> patchField' @"activityType" patch
      >=> patchField' @"date" patch
      >=> patchField' @"tasks" patch
      >=> patchField' @"oldTasks" patch
      >=> patchField' @"observations" patch

-- | Handle an Evidences context command
handleEvidencesCommand :: UserId -> EvidencesCommand -> Document -> UpdateResult
handleEvidencesCommand userId (OnEvidences c) = interpretEntityCommand evidenceContext userId c
  where
    evidenceContext =
      mkEntityCommandContext
        #evidences
        #id
        EvidenceLock
        applyEvidencePatch
        (\e d' -> allTeachersAnd d' (Set.toList e.userIds))
    allTeachersAnd d' us =
      AffectedUsers $
        map (.id) $
          IxSet.toList (d' ^. #users) & filter (\u -> u.id `elem` us || u.role == Teacher)
