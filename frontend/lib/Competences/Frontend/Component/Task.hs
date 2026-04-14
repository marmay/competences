-- | Task component and rendering primitives.
--
-- Re-exports from submodules for convenient access:
--
-- * 'Task.Component' — Miso component with SyncContext subscription
-- * 'Task.ListView' — shared task list rendering for parent components
-- * 'Task.EditButton' — standalone edit buttons (polymorphic)
module Competences.Frontend.Component.Task
  ( -- * Component
    TaskConfig (..)
  , TaskDisplayMode (..)
  , taskComponent
    -- * Task list rendering
  , taskListView
    -- * Edit buttons
  , taskEditButton
  , solutionEditButton
  )
where

import Competences.Frontend.Component.Task.Component
import Competences.Frontend.Component.Task.EditButton
import Competences.Frontend.Component.Task.ListView (taskListView)
