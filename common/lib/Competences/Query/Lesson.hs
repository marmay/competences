-- | Lesson queries on the Document.
-- Provides reusable lookups for lessons by meso plan.
module Competences.Query.Lesson
  ( mesoPlanLessons
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lesson, MesoPlanId, Order)
import Data.Proxy (Proxy (..))

-- | All lessons for a meso plan, sorted by Order.
mesoPlanLessons :: Document -> MesoPlanId -> [Lesson]
mesoPlanLessons doc planId =
  Ix.toAscList (Proxy @Order) $ doc.lessons Ix.@= planId
