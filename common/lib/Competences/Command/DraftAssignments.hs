{-# LANGUAGE CPP #-}

module Competences.Command.DraftAssignments
  ( DraftAssignmentsCommand (..)
  , handleDraftAssignmentsCommand
  )
where

import Competences.Command.Assignments (AssignmentPatch (..), applyAssignmentPatch)
import Competences.Command.Audience (CommandAudience (..))
import Competences.Command.Common (CommandContext (..), EntityCommand (..), UpdateResult)
import Competences.Command.Interpret (interpretEntityCommand, mkEntityCommandContext)
import Competences.Document (Document (..), Lock (..))
import Competences.Document.Assignment (Assignment (..))
#ifdef WITH_AESON
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Binary (Binary)
import GHC.Generics (Generic)

-- | Commands for draft assignments (teacher-only, targeting draft collection)
data DraftAssignmentsCommand
  = OnDraftAssignments !(EntityCommand Assignment AssignmentPatch)
  deriving (Eq, Generic, Show)

instance Binary DraftAssignmentsCommand
#ifdef WITH_AESON
instance FromJSON DraftAssignmentsCommand
instance ToJSON DraftAssignmentsCommand
#endif

-- | Handle a DraftAssignments context command
handleDraftAssignmentsCommand :: CommandContext -> DraftAssignmentsCommand -> Document -> UpdateResult
handleDraftAssignmentsCommand cmdCtx (OnDraftAssignments c) d =
  -- No referential integrity checks for draft assignments (they can be freely deleted)
  interpretEntityCommand draftAssignmentContext cmdCtx c d
  where
    draftAssignmentContext =
      mkEntityCommandContext
        #draftAssignments
        #id
        AssignmentLock
        applyAssignmentPatch
        (\_ _ -> AudienceTeachers)
