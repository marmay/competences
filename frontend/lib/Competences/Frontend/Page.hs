module Competences.Frontend.Page
  ( Page (..)
  )
where

import Control.Applicative ((<|>))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Lesson (LessonId)
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Document.Resource (ResourceId)
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
  | ManageResources !(Maybe ResourceId)
  | ManageLessonNotes !(Maybe LessonNotesId)
  | LessonRecords !(Maybe LessonId)
  | ManageAssignments !(Maybe AssignmentId)
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
        , M.path "resources" *> (ManageResources . Just <$> M.capture <|> pure (ManageResources Nothing))
        , M.path "lesson-notes" *> (ManageLessonNotes . Just <$> M.capture <|> pure (ManageLessonNotes Nothing))
        , M.path "lesson-records" *> (LessonRecords . Just <$> M.capture <|> pure (LessonRecords Nothing))
        , M.path "assignments" *> (ManageAssignments . Just <$> M.capture <|> pure (ManageAssignments Nothing))
        , M.path "statistics-overview" $> StatisticsOverview
        , M.path "participation-timeline" $> ParticipationTimeline
        , M.path "users" $> ManageUsers
        ]
  fromRoute CompetenceGrid = [M.toPath "app", M.toPath "grid"]
  fromRoute Planning = [M.toPath "app", M.toPath "planning"]
  fromRoute Evidences = [M.toPath "app", M.toPath "evidences"]
  fromRoute (ManageTasks Nothing) = [M.toPath "app", M.toPath "tasks"]
  fromRoute (ManageTasks (Just tid)) = [M.toPath "app", M.toPath "tasks", M.toCapture tid]
  fromRoute (ManageResources Nothing) = [M.toPath "app", M.toPath "resources"]
  fromRoute (ManageResources (Just rid)) = [M.toPath "app", M.toPath "resources", M.toCapture rid]
  fromRoute (ManageLessonNotes Nothing) = [M.toPath "app", M.toPath "lesson-notes"]
  fromRoute (ManageLessonNotes (Just lnid)) = [M.toPath "app", M.toPath "lesson-notes", M.toCapture lnid]
  fromRoute (LessonRecords Nothing) = [M.toPath "app", M.toPath "lesson-records"]
  fromRoute (LessonRecords (Just lid)) = [M.toPath "app", M.toPath "lesson-records", M.toCapture lid]
  fromRoute (ManageAssignments Nothing) = [M.toPath "app", M.toPath "assignments"]
  fromRoute (ManageAssignments (Just aid)) = [M.toPath "app", M.toPath "assignments", M.toCapture aid]
  fromRoute StatisticsOverview = [M.toPath "app", M.toPath "statistics-overview"]
  fromRoute ParticipationTimeline = [M.toPath "app", M.toPath "participation-timeline"]
  fromRoute ManageUsers = [M.toPath "app", M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show
