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
    M.path "app"
      *> M.routes
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
  fromRoute CompetenceGrid = [M.toPath "app", M.toPath "grid"]
  fromRoute Planning = [M.toPath "app", M.toPath "planning"]
  fromRoute Evidences = [M.toPath "app", M.toPath "evidences"]
  fromRoute ManageTasks = [M.toPath "app", M.toPath "tasks"]
  fromRoute ManageResources = [M.toPath "app", M.toPath "resources"]
  fromRoute ManageLessonNotes = [M.toPath "app", M.toPath "lesson-notes"]
  fromRoute ViewAssignments = [M.toPath "app", M.toPath "assignments"]
  fromRoute ManageAssignments = [M.toPath "app", M.toPath "manage-assignments"]
  fromRoute StatisticsOverview = [M.toPath "app", M.toPath "statistics-overview"]
  fromRoute ManageUsers = [M.toPath "app", M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show
