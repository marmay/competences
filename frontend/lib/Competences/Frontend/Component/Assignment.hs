module Competences.Frontend.Component.Assignment
  ( assignmentComponent
  , AssignmentMode (..)
  )
where

import Competences.Document (Assignment (..), User (..))
import Competences.Document.User (isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.EditorDetail (editorDetailView)
import Competences.Frontend.Component.Assignment.ViewerDetail (viewerDetailView)
import Competences.Frontend.Component.Selector.AssignmentSelector (assignmentSelectorComponent)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Data.List.NonEmpty (NonEmpty (..))
import Miso qualified as M

-- | Mode for the assignment component
-- Teachers have Edit and View modes; evaluation is available via pinned dialog
-- Students only have View mode
data AssignmentMode = AssignmentEdit | AssignmentView
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Assignment component using SelectorDetail pattern
-- Teachers: Edit (default) and View modes
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
          AssignmentView -> viewerDetailView r user assignment
      , SD.modeLabel = \case
          AssignmentEdit -> C.translate' C.LblEdit
          AssignmentView -> C.translate' C.LblView
      , SD.modeIcon = \case
          AssignmentEdit -> Just Icon.IcnEdit
          AssignmentView -> Just Icon.IcnView
      , SD.availableModes = availableModes
      , SD.defaultMode = defaultMode
      , SD.emptyView = Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
      }
  where
    -- Teachers get Edit + View modes, students only get View mode
    availableModes =
      if isTeacher user
        then AssignmentEdit :| [AssignmentView]
        else AssignmentView :| []
    -- Teachers default to Edit mode, students default to View mode
    defaultMode =
      if isTeacher user
        then AssignmentEdit
        else AssignmentView
