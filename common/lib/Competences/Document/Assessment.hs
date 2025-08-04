module Competences.Document.Assessment
  ( Assessment (..)
  , AssessmentType (..)
  , Verdict (..)
  , AssessmentId
  , fromEvidences
  )
where

import Competences.Document.Evidence (Evidence)
import Competences.Document.Id (Id)
import Competences.Document.User (UserId)

type AssessmentId = Id Assessment

-- | Given a list of evidences, produces a final result.
data Verdict
  = -- | When we consider all evidences in reverse chronological order up
    -- to before the first one where `Ability` is `NotYet`, that sequence
    -- has at least two entries with `Ability` being `SelfReliant` on
    -- two different days and at least one entry has the `SocialForm`
    -- `Individual`.
    Competent
  | -- | When we consider all evidences in reverse chronological order up
    -- to before the second one where `Ability` is `NotYet`, that sequence
    -- has at least two entries with `Ability` being `SelfReliant` on
    -- two different days and at least one entry has the `SocialForm`
    -- `Individual`.
    ProbablyCompetent
  | -- | When there are at least four evidences, but non of the criteria
    -- above are satisfied, we consider the student to be `NotYetCompetent`.
    NotYetCompetent
  | -- | When none of the criteria above are satisfied, we consider the
    -- assessment to be `Inconclusive`.
    Inconclusive
  deriving (Eq, Ord, Show)

data AssessmentType
  = AutomaticAssessment
  | ManualAssessment !AssessmentId
  deriving (Eq, Ord, Show)

data Assessment = Assessment
  { assessmentType :: !AssessmentType
  , userId :: !UserId
  , verdict :: !Verdict
  }
  deriving (Eq, Ord, Show)

fromEvidences :: [Evidence] -> Verdict
fromEvidences es = undefined
