-- | Assignment status queries
-- Pure functions for determining assignment completion status
-- Designed for reuse across frontend and for property-based testing
module Competences.Query.Assignment
  ( -- * Single-entity lookup
    getAssignment
    -- * User-scoped queries
  , userAssignments
    -- * Status queries
  , AssignmentStatus (..)
  , assignmentStatus
  , accumulatedObservations
  , isAssignmentCompleted
  , isAssignmentOpen
  , statusLabel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment, AssignmentId, AssignmentIxs, Document (..))
import Competences.Document.Submission (Submission (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.User (UserId)
import Competences.Query.Evidence qualified as QEvidence
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (utctDay)

-- | Lookup an assignment by primary key.
getAssignment :: Document -> AssignmentId -> Maybe Assignment
getAssignment doc assignmentId = Ix.getOne $ doc.assignments Ix.@= assignmentId

-- | All assignments for a user (as IxSet for further filtering).
userAssignments :: Document -> UserId -> Ix.IxSet AssignmentIxs Assignment
userAssignments doc userId = doc.assignments Ix.@= userId

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

-- | Is an assignment "open" for a user?
-- An assignment is open when the student still needs to act:
-- - Completed → False (nothing to do)
-- - NotGraded + no submissions → True (student hasn't submitted)
-- - NotGraded + has submissions → False (ball is with the teacher)
-- - NeedsWork + no submission after latest evidence → True (student needs to fix)
-- - NeedsWork + has submission on/after latest evidence → False (ball is with the teacher)
isAssignmentOpen :: Document -> UserId -> AssignmentId -> Bool
isAssignmentOpen doc userId assignmentId =
  case assignmentStatus doc userId assignmentId of
    Completed -> False
    NotGraded ->
      Ix.null (doc.submissions Ix.@= assignmentId Ix.@= userId)
    NeedsWork ->
      let submissions = Ix.toList (doc.submissions Ix.@= assignmentId Ix.@= userId)
          linkedEvidences = filter (\e -> e.assignmentId == Just assignmentId) (QEvidence.userEvidencesAsc doc userId)
          latestEvidenceDay = case linkedEvidences of
            [] -> Nothing
            es -> Just (last es).date
       in case latestEvidenceDay of
            Nothing -> True -- No evidence date to compare against
            Just d -> not $ any (\s -> utctDay s.submittedAt >= d) submissions

-- | Status label for display (German)
statusLabel :: AssignmentStatus -> Text
statusLabel NotGraded = "Nicht korrigiert"
statusLabel NeedsWork = "Zu verbessern"
statusLabel Completed = "Erledigt"
