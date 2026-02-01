-- | Lesson and MesoPlan queries on the Document.
-- Provides reusable lookups for lessons by meso plan and entity getters.
module Competences.Query.Lesson
  ( -- * Single-entity lookups
    getLesson
  , getMesoPlan
    -- * MesoPlan-scoped queries
  , mesoPlanLessons
    -- * Lesson-scoped queries
  , lessonAssignments
  , lessonEvidences
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment, Document (..), Evidence, Lesson, LessonId, MesoPlan, MesoPlanId, Order)
import Data.Proxy (Proxy (..))

-- | Lookup a lesson by primary key.
getLesson :: Document -> LessonId -> Maybe Lesson
getLesson doc lessonId = Ix.getOne $ doc.lessons Ix.@= lessonId

-- | Lookup a meso plan by primary key.
getMesoPlan :: Document -> MesoPlanId -> Maybe MesoPlan
getMesoPlan doc planId = Ix.getOne $ doc.mesoPlans Ix.@= planId

-- | All lessons for a meso plan, sorted by Order.
mesoPlanLessons :: Document -> MesoPlanId -> [Lesson]
mesoPlanLessons doc planId =
  Ix.toAscList (Proxy @Order) $ doc.lessons Ix.@= planId

-- | All assignments linked to a lesson.
lessonAssignments :: Document -> LessonId -> [Assignment]
lessonAssignments doc lessonId =
  Ix.toList $ doc.assignments Ix.@= lessonId

-- | All evidences collected during a lesson.
lessonEvidences :: Document -> LessonId -> [Evidence]
lessonEvidences doc lessonId =
  Ix.toList $ doc.evidences Ix.@= lessonId
