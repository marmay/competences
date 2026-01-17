module Competences.Frontend.Component.Assignment
  ( assignmentComponent
  , AssignmentMode (..)
  )
where

import Competences.Document (Assignment (..), User (..))
import Competences.Document.User (isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.EditorDetail (editorDetailView)
import Competences.Frontend.Component.Assignment.EvaluatorDetail (evaluatorDetailView)
import Competences.Frontend.Component.Assignment.ViewerDetail (viewerDetailView)
import Competences.Frontend.Component.Selector.AssignmentSelector (assignmentSelectorComponent)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Miso qualified as M

-- | Mode for the assignment component
-- Teachers have Edit, Evaluate, and View modes
-- Students only have View mode
data AssignmentMode = AssignmentEdit | AssignmentEvaluate | AssignmentView
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Assignment component using SelectorDetail pattern
-- Teachers: Edit (default), Evaluate, and View modes
-- Students: View mode only
assignmentComponent
  :: SyncContext
  -> User
  -> M.Component p (SD.Model Assignment AssignmentMode) (SD.Action AssignmentMode)
assignmentComponent r user =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "assignment"
      , SD.selectorComponent = assignmentSelectorComponent r
      , SD.detailView = \mode assignment -> case mode of
          AssignmentEdit -> editorDetailView r assignment
          AssignmentEvaluate -> evaluatorDetailView r assignment
          AssignmentView -> viewerDetailView r user assignment
      , SD.modeLabel = \case
          AssignmentEdit -> C.translate' C.LblEdit
          AssignmentEvaluate -> C.translate' C.LblEvaluate
          AssignmentView -> C.translate' C.LblView
      , SD.modeIcon = \case
          AssignmentEdit -> Just IcnEdit
          AssignmentEvaluate -> Just IcnApply
          AssignmentView -> Just IcnView
      , SD.availableModes = availableModes
      , SD.defaultMode = defaultMode
      , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
      }
  where
    -- Teachers get all modes, students only get View mode
    availableModes =
      if isTeacher user
        then AssignmentEdit :| [AssignmentEvaluate, AssignmentView]
        else AssignmentView :| []
    -- Teachers default to Edit mode, students default to View mode
    defaultMode =
      if isTeacher user
        then AssignmentEdit
        else AssignmentView
