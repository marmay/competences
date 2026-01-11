module Competences.Frontend.Component.Assignment
  ( assignmentComponent
  , AssignmentMode (..)
  )
where

import Competences.Document (Assignment (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.EditorDetail (editorDetailView)
import Competences.Frontend.Component.Assignment.EvaluatorDetail (evaluatorDetailView)
import Competences.Frontend.Component.Selector.AssignmentSelector (assignmentSelectorComponent)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Miso qualified as M

-- | Mode for the assignment component
data AssignmentMode = AssignmentEdit | AssignmentEvaluate
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Assignment component using SelectorDetail pattern
-- Provides Edit and Evaluate modes for teachers
assignmentComponent
  :: SyncContext
  -> M.Component p (SD.Model Assignment AssignmentMode) (SD.Action AssignmentMode)
assignmentComponent r =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "assignment"
      , SD.selectorComponent = assignmentSelectorComponent r
      , SD.detailView = \mode assignment -> case mode of
          AssignmentEdit -> editorDetailView r assignment
          AssignmentEvaluate -> evaluatorDetailView r assignment
      , SD.modeLabel = \case
          AssignmentEdit -> C.translate' C.LblEdit
          AssignmentEvaluate -> C.translate' C.LblEvaluate
      , SD.modeIcon = \case
          AssignmentEdit -> Just IcnEdit
          AssignmentEvaluate -> Just IcnApply
      , SD.availableModes = AssignmentEdit :| [AssignmentEvaluate]
      , SD.defaultMode = AssignmentEdit
      , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
      }
