module Competences.Query.DefaultSelection
  ( defaultAssignment
  , defaultCompetenceGrid
  , defaultLessonNotes
  , defaultMesoPlan
  , defaultTask
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Assignment (Assignment (..), AssignmentIxs)
import Competences.Document.CompetenceGrid (CompetenceGrid (..), CompetenceGridIxs)
import Competences.Document.Task (Task, TaskIdentifier, TaskIxs)
import Competences.Document.LessonNotes (LessonNotes (..), LessonNotesIxs)
import Competences.Document.MesoPlan (MesoPlan (..), MesoPlanIxs)
import Competences.Document.Order (Order)
import Data.List (find, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Time (Day)

-- | Last HomeExercise by date; fallback: last assignment by date.
defaultAssignment :: Day -> Ix.IxSet AssignmentIxs Assignment -> Maybe Assignment
defaultAssignment _today assignments =
  let allDesc = Ix.toDescList (Proxy @Day) assignments
   in case find (\a -> a.activityType == HomeExercise) allDesc of
        Just a -> Just a
        Nothing -> listToMaybe allDesc

-- | Last competence grid by Order.
defaultCompetenceGrid :: Ix.IxSet CompetenceGridIxs CompetenceGrid -> Maybe CompetenceGrid
defaultCompetenceGrid grids =
  listToMaybe $ Ix.toDescList (Proxy @Order) grids

-- | First lesson notes entry on or after today.
defaultLessonNotes :: Day -> Ix.IxSet LessonNotesIxs LessonNotes -> Maybe LessonNotes
defaultLessonNotes today notes =
  listToMaybe $ Ix.toAscList (Proxy @Day) $ notes Ix.@>= today

-- | First task by identifier (alphabetical order).
defaultTask :: Ix.IxSet TaskIxs Task -> Maybe Task
defaultTask tasks =
  listToMaybe $ Ix.toAscList (Proxy @TaskIdentifier) tasks

-- | Last meso plan whose dateFrom <= today.
defaultMesoPlan :: Day -> Ix.IxSet MesoPlanIxs MesoPlan -> Maybe MesoPlan
defaultMesoPlan today plans =
  listToMaybe
    $ sortOn (Down . (.dateFrom))
    $ filter (\p -> maybe False (<= today) p.dateFrom)
    $ Ix.toList plans
