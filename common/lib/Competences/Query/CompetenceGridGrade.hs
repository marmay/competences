-- | CompetenceGridGrade queries.
-- Provides IxSet-level lookups for grid grades, designed to work on
-- pre-filtered sets (e.g. already projected to a focused user).
--
-- Algebraic property: @activeGridGrade xs gid == listToMaybe (gridGradeHistory xs gid)@
module Competences.Query.CompetenceGridGrade
  ( -- * Document-level queries
    userGridGrades
    -- * IxSet-level queries (work on pre-filtered sets)
  , activeGridGrade
  , gridGradeHistory
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceGridGrade
  , CompetenceGridGradeIxs
  , CompetenceGridId
  , Document (..)
  , UserId
  )
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Time (Day)

-- | All grid grades for a user (as IxSet for further filtering).
userGridGrades :: Document -> UserId -> Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
userGridGrades doc userId = doc.competenceGridGrades Ix.@= userId

-- | Most recent (active) grid grade for a competence grid.
-- The input IxSet should be pre-filtered (e.g. to a single user).
activeGridGrade
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> CompetenceGridId
  -> Maybe CompetenceGridGrade
activeGridGrade gridGrades gridId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $ gridGrades Ix.@= gridId

-- | Grade history for a competence grid, sorted by date descending.
-- The input IxSet should be pre-filtered (e.g. to a single user).
gridGradeHistory
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> CompetenceGridId
  -> [CompetenceGridGrade]
gridGradeHistory gridGrades gridId =
  Ix.toDescList (Proxy @Day) $ gridGrades Ix.@= gridId
