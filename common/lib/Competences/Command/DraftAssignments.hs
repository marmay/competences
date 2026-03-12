{-# LANGUAGE CPP #-}

module Competences.Command.DraftAssignments
  ( DraftAssignmentsCommand (..)
  , handleDraftAssignmentsCommand
  )
where

import Competences.Command.Assignments (AssignmentPatch (..), applyAssignmentPatch)
import Competences.Command.Common (AffectedUsers (..), EntityCommand (..), UpdateResult)
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..), User (..))
import Competences.Document.Assignment (Assignment (..))
import Competences.Document.User (UserId, UserRole (..))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import Data.IxSet.Typed qualified as IxSet
import GHC.Generics (Generic)
import Optics.Core ((^.))

-- | Commands for draft assignments (teacher-only, targeting draft collection)
data DraftAssignmentsCommand
  = OnDraftAssignments !(EntityCommand Assignment AssignmentPatch)
  deriving (Eq, Generic, Show)

instance Binary DraftAssignmentsCommand
#ifdef WITH_AESON
instance FromJSON DraftAssignmentsCommand
instance ToJSON DraftAssignmentsCommand
#endif

-- | All teachers (draft entities only visible to teachers)
allTeachers :: Document -> AffectedUsers
allTeachers d = AffectedUsers $ map (.id) $ filter (\u -> u.role == Teacher) $ IxSet.toList $ d ^. #users

-- | Handle a DraftAssignments context command
handleDraftAssignmentsCommand :: UserId -> DraftAssignmentsCommand -> Document -> UpdateResult
handleDraftAssignmentsCommand userId (OnDraftAssignments c) d =
  -- No referential integrity checks for draft assignments (they can be freely deleted)
  interpretEntityCommand draftAssignmentContext userId c d
  where
    draftAssignmentContext =
      mkEntityCommandContext
        #draftAssignments
        #id
        AssignmentLock
        applyAssignmentPatch
        (\_ d' -> allTeachers d')
