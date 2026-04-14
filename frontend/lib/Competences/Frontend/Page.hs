module Competences.Frontend.Page
  ( Page (..)
  )
where

import Control.Applicative ((<|>))
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common.MisoId ()
import Data.Functor (($>))
import Miso qualified as M
import Miso.Router qualified as M

data Page
  = CompetenceGrid
  | Planning
  | Evidences
  | ManageTasks !(Maybe TaskId)
  | ManageResources
  | ManageLessonNotes
  | ViewAssignments
  | ManageAssignments
  | StatisticsOverview
  | ParticipationTimeline
  | ManageUsers
  deriving (Eq, Show)

instance M.Router Page where
  routeParser =
    M.path "app"
      *> M.routes
        [ M.path "grid" $> CompetenceGrid
        , M.path "planning" $> Planning
        , M.path "evidences" $> Evidences
        , M.path "tasks" *> (ManageTasks . Just <$> M.capture <|> pure (ManageTasks Nothing))
        , M.path "resources" $> ManageResources
        , M.path "lesson-notes" $> ManageLessonNotes
        , M.path "assignments" $> ViewAssignments
        , M.path "manage-assignments" $> ManageAssignments
        , M.path "statistics-overview" $> StatisticsOverview
        , M.path "participation-timeline" $> ParticipationTimeline
        , M.path "users" $> ManageUsers
        ]
  fromRoute CompetenceGrid = [M.toPath "app", M.toPath "grid"]
  fromRoute Planning = [M.toPath "app", M.toPath "planning"]
  fromRoute Evidences = [M.toPath "app", M.toPath "evidences"]
  fromRoute (ManageTasks Nothing) = [M.toPath "app", M.toPath "tasks"]
  fromRoute (ManageTasks (Just tid)) = [M.toPath "app", M.toPath "tasks", M.toCapture tid]
  fromRoute ManageResources = [M.toPath "app", M.toPath "resources"]
  fromRoute ManageLessonNotes = [M.toPath "app", M.toPath "lesson-notes"]
  fromRoute ViewAssignments = [M.toPath "app", M.toPath "assignments"]
  fromRoute ManageAssignments = [M.toPath "app", M.toPath "manage-assignments"]
  fromRoute StatisticsOverview = [M.toPath "app", M.toPath "statistics-overview"]
  fromRoute ParticipationTimeline = [M.toPath "app", M.toPath "participation-timeline"]
  fromRoute ManageUsers = [M.toPath "app", M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show
