module Competences.Frontend.Page
  ( Page (..)
  )
where

import Control.Applicative ((<|>))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.CompetenceGrid (CompetenceGridId)
import Competences.Document.Evidence (EvidenceId)
import Competences.Document.Lesson (LessonId)
import Competences.Document.MesoPlan (MesoPlanId)
import Competences.Document.Resource (ResourceId)
import Competences.Document.Task (TaskId)
import Competences.Frontend.Common.MisoId ()
import Data.Functor (($>))
import Miso qualified as M
import Miso.Router qualified as M

data Page
  = CompetenceGrid !(Maybe CompetenceGridId)
  | Planning !(Maybe MesoPlanId)
  | Evidences !(Maybe EvidenceId)
  | ManageTasks !(Maybe TaskId)
  | ManageResources !(Maybe ResourceId)
  | LessonRecords !(Maybe LessonId)
  | ManageAssignments !(Maybe AssignmentId)
  | StatisticsOverview
  | ParticipationTimeline
  | ManageUsers
  | Import
  deriving (Eq, Show)

instance M.Router Page where
  routeParser =
    M.path "app"
      *> M.routes
        [ M.path "grid" *> (CompetenceGrid . Just <$> M.capture <|> pure (CompetenceGrid Nothing))
        , M.path "planning" *> (Planning . Just <$> M.capture <|> pure (Planning Nothing))
        , M.path "evidences" *> (Evidences . Just <$> M.capture <|> pure (Evidences Nothing))
        , M.path "tasks" *> (ManageTasks . Just <$> M.capture <|> pure (ManageTasks Nothing))
        , M.path "resources" *> (ManageResources . Just <$> M.capture <|> pure (ManageResources Nothing))
        , M.path "lesson-records" *> (LessonRecords . Just <$> M.capture <|> pure (LessonRecords Nothing))
        , M.path "assignments" *> (ManageAssignments . Just <$> M.capture <|> pure (ManageAssignments Nothing))
        , M.path "statistics-overview" $> StatisticsOverview
        , M.path "participation-timeline" $> ParticipationTimeline
        , M.path "users" $> ManageUsers
        , M.path "import" $> Import
        ]
  fromRoute (CompetenceGrid Nothing) = [M.toPath "app", M.toPath "grid"]
  fromRoute (CompetenceGrid (Just gid)) = [M.toPath "app", M.toPath "grid", M.toCapture gid]
  fromRoute (Planning Nothing) = [M.toPath "app", M.toPath "planning"]
  fromRoute (Planning (Just pid)) = [M.toPath "app", M.toPath "planning", M.toCapture pid]
  fromRoute (Evidences Nothing) = [M.toPath "app", M.toPath "evidences"]
  fromRoute (Evidences (Just eid)) = [M.toPath "app", M.toPath "evidences", M.toCapture eid]
  fromRoute (ManageTasks Nothing) = [M.toPath "app", M.toPath "tasks"]
  fromRoute (ManageTasks (Just tid)) = [M.toPath "app", M.toPath "tasks", M.toCapture tid]
  fromRoute (ManageResources Nothing) = [M.toPath "app", M.toPath "resources"]
  fromRoute (ManageResources (Just rid)) = [M.toPath "app", M.toPath "resources", M.toCapture rid]
  fromRoute (LessonRecords Nothing) = [M.toPath "app", M.toPath "lesson-records"]
  fromRoute (LessonRecords (Just lid)) = [M.toPath "app", M.toPath "lesson-records", M.toCapture lid]
  fromRoute (ManageAssignments Nothing) = [M.toPath "app", M.toPath "assignments"]
  fromRoute (ManageAssignments (Just aid)) = [M.toPath "app", M.toPath "assignments", M.toCapture aid]
  fromRoute StatisticsOverview = [M.toPath "app", M.toPath "statistics-overview"]
  fromRoute ParticipationTimeline = [M.toPath "app", M.toPath "participation-timeline"]
  fromRoute ManageUsers = [M.toPath "app", M.toPath "users"]
  fromRoute Import = [M.toPath "app", M.toPath "import"]

instance M.ToKey Page where
  toKey = M.toKey . show
