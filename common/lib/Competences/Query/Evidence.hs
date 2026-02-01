-- | Evidence queries on the Document.
-- Provides reusable lookups for user evidences.
module Competences.Query.Evidence
  ( -- * Single-entity lookup
    getEvidence
    -- * User-scoped queries
  , userEvidences
  , userEvidencesDesc
  , userEvidencesAsc
    -- * Lesson-day grouping
  , groupByLessonDay
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Evidence (..), EvidenceId, EvidenceIxs, UserId)
import Competences.Document.ActivityType (activityReliability)
import Competences.Document.Lesson (LessonId)
import Data.List (groupBy, sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Time (Day)

-- | Lookup an evidence by primary key.
getEvidence :: Document -> EvidenceId -> Maybe Evidence
getEvidence doc evidenceId = Ix.getOne $ doc.evidences Ix.@= evidenceId

-- | All evidences for a user (as IxSet for further filtering).
userEvidences :: Document -> UserId -> Ix.IxSet EvidenceIxs Evidence
userEvidences doc userId = doc.evidences Ix.@= userId

-- | All evidences for a user, sorted newest-first.
userEvidencesDesc :: Document -> UserId -> [Evidence]
userEvidencesDesc doc userId =
  Ix.toDescList (Proxy @Day) $ doc.evidences Ix.@= userId

-- | All evidences for a user, sorted oldest-first.
userEvidencesAsc :: Document -> UserId -> [Evidence]
userEvidencesAsc doc userId =
  Ix.toAscList (Proxy @Day) $ doc.evidences Ix.@= userId

-- ============================================================================
-- Lesson-day grouping
-- ============================================================================

-- | Group evidences (assumed sorted newest-first by Day) by lesson day.
--
-- Evidences sharing the same @(Day, Just LessonId)@ form a group, sorted
-- internally by 'activityReliability' descending (most reliable first).
-- Evidences with @lessonId = Nothing@ remain singletons.
-- Returns groups in Day-descending order.
groupByLessonDay :: [Evidence] -> [[Evidence]]
groupByLessonDay = concatMap splitDayGroup . groupBy sameDay
  where
    sameDay a b = a.date == b.date

    splitDayGroup :: [Evidence] -> [[Evidence]]
    splitDayGroup dayEvs =
      let -- Partition into lesson-linked and standalone
          (withLesson, withoutLesson) = foldr partitionLesson (Map.empty, []) dayEvs
          -- Each lesson group sorted by reliability descending
          lessonGroups = map (sortOn (Down . activityReliability . (.activityType))) (Map.elems withLesson)
          -- Standalone evidences are singletons
          standaloneGroups = map (: []) withoutLesson
       in lessonGroups ++ standaloneGroups

    partitionLesson :: Evidence -> (Map.Map LessonId [Evidence], [Evidence]) -> (Map.Map LessonId [Evidence], [Evidence])
    partitionLesson ev (lessonMap, standalone) = case ev.lessonId of
      Just lid -> (Map.insertWith (++) lid [ev] lessonMap, standalone)
      Nothing -> (lessonMap, ev : standalone)
