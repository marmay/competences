-- | CompetenceAssessment queries.
-- Provides IxSet-level lookups for assessments, designed to work on
-- pre-filtered sets (e.g. already projected to a focused user).
--
-- Algebraic property: @activeAssessment xs cid == listToMaybe (assessmentHistory xs cid)@
module Competences.Query.CompetenceAssessment
  ( -- * Document-level queries
    userAssessments
    -- * IxSet-level queries (work on pre-filtered sets)
  , activeAssessment
  , assessmentHistory
  , findAssessmentForDay
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceAssessment
  , CompetenceAssessmentIxs
  , CompetenceId
  , Document (..)
  , UserId
  )
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Time (Day)

-- | All assessments for a user (as IxSet for further filtering).
userAssessments :: Document -> UserId -> Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
userAssessments doc userId = doc.competenceAssessments Ix.@= userId

-- | Most recent (active) assessment for a competence.
-- The input IxSet should be pre-filtered (e.g. to a single user).
activeAssessment
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> Maybe CompetenceAssessment
activeAssessment assessments competenceId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $ assessments Ix.@= competenceId

-- | Assessment history for a competence, sorted by date descending.
-- The input IxSet should be pre-filtered (e.g. to a single user).
assessmentHistory
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> [CompetenceAssessment]
assessmentHistory assessments competenceId =
  Ix.toDescList (Proxy @Day) $ assessments Ix.@= competenceId

-- | Find the assessment for a specific competence on a specific day.
-- The input IxSet should be pre-filtered (e.g. to a single user).
findAssessmentForDay
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> Day
  -> Maybe CompetenceAssessment
findAssessmentForDay assessments competenceId day =
  Ix.getOne $ assessments Ix.@= competenceId Ix.@= day
