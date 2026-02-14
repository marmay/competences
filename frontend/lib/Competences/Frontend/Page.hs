module Competences.Frontend.Page
  ( Page (..)
  )
where

import Data.Functor (($>))
import Miso qualified as M
import Miso.Router qualified as M

data Page
  = CompetenceGrid
  | Planning
  | Evidences
  | ManageTasks
  | ManageResources
  | ManageLessonNotes
  | ViewAssignments
  | ManageAssignments
  | StatisticsOverview
  | ManageUsers
  deriving (Eq, Show)

instance M.Router Page where
  routeParser =
    M.routes
      [ M.path "grid" $> CompetenceGrid
      , M.path "planning" $> Planning
      , M.path "evidences" $> Evidences
      , M.path "tasks" $> ManageTasks
      , M.path "resources" $> ManageResources
      , M.path "lesson-notes" $> ManageLessonNotes
      , M.path "assignments" $> ViewAssignments
      , M.path "manage-assignments" $> ManageAssignments
      , M.path "statistics-overview" $> StatisticsOverview
      , M.path "users" $> ManageUsers
      ]
  fromRoute CompetenceGrid = [M.toPath "grid"]
  fromRoute Planning = [M.toPath "planning"]
  fromRoute Evidences = [M.toPath "evidences"]
  fromRoute ManageTasks = [M.toPath "tasks"]
  fromRoute ManageResources = [M.toPath "resources"]
  fromRoute ManageLessonNotes = [M.toPath "lesson-notes"]
  fromRoute ViewAssignments = [M.toPath "assignments"]
  fromRoute ManageAssignments = [M.toPath "manage-assignments"]
  fromRoute StatisticsOverview = [M.toPath "statistics-overview"]
  fromRoute ManageUsers = [M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show
