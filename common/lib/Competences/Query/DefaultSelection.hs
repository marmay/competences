module Competences.Query.DefaultSelection
  ( defaultAssignment
  , defaultCompetenceGrid
  , defaultLessonNotes
  , defaultMesoPlan
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Assignment (Assignment (..), AssignmentIxs)
import Competences.Document.CompetenceGrid (CompetenceGrid (..), CompetenceGridIxs)
import Competences.Document.LessonNotes (LessonNotes (..), LessonNotesIxs)
import Competences.Document.MesoPlan (MesoPlan (..), MesoPlanIxs)
import Competences.Document.Order (Order)
import Data.List (find, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Time (Day)

-- | First HomeExercise on or after today; fallback: first assignment on or after today.
defaultAssignment :: Day -> Ix.IxSet AssignmentIxs Assignment -> Maybe Assignment
defaultAssignment today assignments =
  let future = Ix.toAscList (Proxy @Day) $ assignments Ix.@>= today
   in case find (\a -> a.activityType == HomeExercise) future of
        Just a -> Just a
        Nothing -> listToMaybe future

-- | Last competence grid by Order.
defaultCompetenceGrid :: Ix.IxSet CompetenceGridIxs CompetenceGrid -> Maybe CompetenceGrid
defaultCompetenceGrid grids =
  listToMaybe $ Ix.toDescList (Proxy @Order) grids

-- | First lesson notes entry on or after today.
defaultLessonNotes :: Day -> Ix.IxSet LessonNotesIxs LessonNotes -> Maybe LessonNotes
defaultLessonNotes today notes =
  listToMaybe $ Ix.toAscList (Proxy @Day) $ notes Ix.@>= today

-- | Last meso plan whose dateFrom <= today.
defaultMesoPlan :: Day -> Ix.IxSet MesoPlanIxs MesoPlan -> Maybe MesoPlan
defaultMesoPlan today plans =
  listToMaybe
    $ sortOn (Down . (.dateFrom))
    $ filter (\p -> maybe False (<= today) p.dateFrom)
    $ Ix.toList plans
