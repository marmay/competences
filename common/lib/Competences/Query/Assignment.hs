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
    -- * Completion categories (for statistics)
  , AssignmentCompletionCategory (..)
  , assignmentCompletionCategory
  , userAssignmentCompletionStats
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), AssignmentId, AssignmentIxs, Document (..))
import Competences.Document.Submission (Submission (..), SubmissionKind (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.User (UserId)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (Day, diffDays, fromGregorian, utctDay)

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
  let linkedEvidences = Ix.toAscList (Proxy @Day) $ doc.evidences Ix.@= assignmentId Ix.@= userId
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
          linkedEvidences = Ix.toAscList (Proxy @Day) $ doc.evidences Ix.@= assignmentId Ix.@= userId
          latestEvidenceDay = case linkedEvidences of
            [] -> Nothing
            es -> Just (last es).date
       in case latestEvidenceDay of
            Nothing -> True -- No evidence date to compare against
            Just d
              | d < submissionTrackingCutoff -> False -- Legacy data: assume already corrected
              | otherwise -> not $ any (\s -> utctDay s.submittedAt >= d) submissions

-- | Cutoff date for submission tracking.
-- Before this date, submissions weren't tracked, so NeedsWork assignments
-- with evidence before this date are assumed to have been corrected offline.
-- TODO: Remove before next school year (2026/27)
submissionTrackingCutoff :: Day
submissionTrackingCutoff = fromGregorian 2026 3 1

-- | Status label for display (German)
statusLabel :: AssignmentStatus -> Text
statusLabel NotGraded = "Nicht korrigiert"
statusLabel NeedsWork = "Zu verbessern"
statusLabel Completed = "Erledigt"

-- | Assignment completion categories for statistics views.
-- These form a partition: every assignment falls into exactly one category.
data AssignmentCompletionCategory
  = -- | Has evidence, no NotYet in accumulated observations (includes WithSupport)
    AsgCompleted
  | -- | Has evidence, but NotYet remains in accumulated observations
    AsgCorrectedNotDone
  | -- | Has submission(s) but no evidence
    AsgSubmittedNotCorrected
  | -- | All submissions are VoidSubmission (student opted out)
    AsgVoid
  | -- | No submission and no evidence, assigned less than 2 weeks ago
    AsgNotSubmitted
  | -- | No submission and no evidence, assigned more than 2 weeks ago
    AsgOverdue
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Classify an assignment for a user into a completion category.
-- Requires today's date to distinguish overdue (>2 weeks) from not-yet-submitted.
assignmentCompletionCategory :: Day -> Document -> UserId -> AssignmentId -> AssignmentCompletionCategory
assignmentCompletionCategory today doc userId assignmentId =
  let accumulated = accumulatedObservations doc userId assignmentId
      hasEvidence = not (Map.null accumulated)
      hasSubmissions = not $ Ix.null (doc.submissions Ix.@= assignmentId Ix.@= userId)
   in if hasEvidence
        then
          if any (== NotYet) (Map.elems accumulated)
            then AsgCorrectedNotDone
            else AsgCompleted
        else
          if hasSubmissions
            then
              let subs = Ix.toList (doc.submissions Ix.@= assignmentId Ix.@= userId)
                  latest = maximumBy (comparing (.submittedAt)) subs
               in if isVoidSubmission latest then AsgVoid else AsgSubmittedNotCorrected
            else
              let mAssignment = getAssignment doc assignmentId
                  isOverdue = case mAssignment of
                    Just a -> diffDays today a.assignmentDate > 14
                    Nothing -> False
               in if isOverdue then AsgOverdue else AsgNotSubmitted

-- | Check if a submission is a VoidSubmission.
isVoidSubmission :: Submission -> Bool
isVoidSubmission s = case s.kind of
  VoidSubmission _ -> True
  _ -> False

-- | Count assignments per completion category for a user.
userAssignmentCompletionStats :: Day -> Document -> UserId -> Map AssignmentCompletionCategory Int
userAssignmentCompletionStats today doc userId =
  let assignments = Ix.toList (userAssignments doc userId)
      categories = map (\a -> assignmentCompletionCategory today doc userId a.id) assignments
   in foldl' (\m c -> Map.insertWith (+) c 1 m) Map.empty categories
