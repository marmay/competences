-- | Assignment status queries
-- Pure functions for determining assignment completion status
-- Designed for reuse across frontend and for property-based testing
module Competences.Query.Assignment
  ( AssignmentStatus (..)
  , assignmentStatus
  , accumulatedObservations
  , isAssignmentCompleted
  , statusLabel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.User (UserId)
import Competences.Query.Evidence qualified as QEvidence
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Assignment completion status for a user
data AssignmentStatus
  = NotGraded -- ^ No evidence exists linked to this assignment
  | NeedsWork -- ^ Has WithSupport or NotYet abilities
  | Completed -- ^ All SelfReliant or SelfReliantWithSillyMistakes
  deriving (Eq, Ord, Show)

-- | Determine assignment status for a user
-- Uses the direct assignmentId link on Evidence
-- Considers all linked evidences ordered by date, with later observations overriding earlier ones
-- for the same competence level. This allows re-assessment to "fix" earlier failures.
assignmentStatus :: Document -> UserId -> AssignmentId -> AssignmentStatus
assignmentStatus doc userId assignmentId =
  let accumulated = accumulatedObservations doc userId assignmentId
   in if Map.null accumulated
        then NotGraded
        else
          let hasNeedsWork = any (`elem` [WithSupport, NotYet]) (Map.elems accumulated)
           in if hasNeedsWork then NeedsWork else Completed

-- | Get accumulated observations for an assignment
-- Orders evidences by date and accumulates observations into a Map where later observations
-- override earlier ones for the same competence level.
accumulatedObservations :: Document -> UserId -> AssignmentId -> Map CompetenceLevelId Ability
accumulatedObservations doc userId assignmentId =
  let -- Get evidences sorted by date (ascending, so later dates come last and override)
      sortedEvidences = QEvidence.userEvidencesAsc doc userId
      linkedEvidences = filter (\e -> e.assignmentId == Just assignmentId) sortedEvidences
      -- Accumulate observations: later evidences override earlier for same competence level
      accumulateObs acc ev =
        foldl' (\m obs -> Map.insert obs.competenceLevelId obs.ability m) acc (Ix.toList ev.observations)
   in foldl' accumulateObs Map.empty linkedEvidences

-- | Convenience predicate for filtering
isAssignmentCompleted :: Document -> UserId -> AssignmentId -> Bool
isAssignmentCompleted doc userId aId = assignmentStatus doc userId aId == Completed

-- | Status label for display (German)
statusLabel :: AssignmentStatus -> Text
statusLabel NotGraded = "Nicht korrigiert"
statusLabel NeedsWork = "Zu verbessern"
statusLabel Completed = "Erledigt"
