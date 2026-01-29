-- | Lesson and MesoPlan queries on the Document.
-- Provides reusable lookups for lessons by meso plan and entity getters.
module Competences.Query.Lesson
  ( -- * Single-entity lookups
    getLesson
  , getMesoPlan
    -- * MesoPlan-scoped queries
  , mesoPlanLessons
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lesson, LessonId, MesoPlan, MesoPlanId, Order)
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
