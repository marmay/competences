-- | Assignment status queries
-- Pure functions for determining assignment completion status
-- Designed for reuse across frontend and for property-based testing
module Competences.Query.Assignment
  ( AssignmentStatus (..)
  , assignmentStatus
  , isAssignmentCompleted
  , statusLabel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Evidence (Ability (..), Evidence (..), Observation (..))
import Competences.Document.User (UserId)
import Data.Text (Text)

-- | Assignment completion status for a user
data AssignmentStatus
  = NotGraded -- ^ No evidence exists linked to this assignment
  | NeedsWork -- ^ Has WithSupport or NotYet abilities
  | Completed -- ^ All SelfReliant or SelfReliantWithSillyMistakes
  deriving (Eq, Ord, Show)

-- | Determine assignment status for a user
-- Uses the direct assignmentId link on Evidence
assignmentStatus :: Document -> UserId -> AssignmentId -> AssignmentStatus
assignmentStatus doc userId assignmentId =
  let userEvidences = Ix.toList $ doc.evidences Ix.@= userId
      linkedEvidences = filter (\e -> e.assignmentId == Just assignmentId) userEvidences
   in case linkedEvidences of
        [] -> NotGraded
        evidences ->
          let allAbilities = concatMap (map (.ability) . Ix.toList . (.observations)) evidences
              hasNeedsWork = any (`elem` [WithSupport, NotYet]) allAbilities
           in if null allAbilities
                then NotGraded
                else if hasNeedsWork then NeedsWork else Completed

-- | Convenience predicate for filtering
isAssignmentCompleted :: Document -> UserId -> AssignmentId -> Bool
isAssignmentCompleted doc userId aId = assignmentStatus doc userId aId == Completed

-- | Status label for display (German)
statusLabel :: AssignmentStatus -> Text
statusLabel NotGraded = "Nicht korrigiert"
statusLabel NeedsWork = "Zu verbessern"
statusLabel Completed = "Erledigt"
