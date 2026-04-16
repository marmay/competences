-- | Embedding helpers for the detailed assignment view.
--
-- Handles effectful operations for 'AssignmentDetailedAction' from the
-- Fragment layer. Safe to import from any entity's component module.
module Competences.Frontend.Component.Assignment.Detailed.Embed
  ( updateAssignmentDetailed
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..), ModifyCommand (..))
import Competences.Frontend.Component.Assignment.EvaluatorDetail (pinAssignmentEvaluator)
import Competences.Frontend.Fragment.Assignment.Detailed qualified as V
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (PinViewerRequest (..), SyncContext, modifySyncDocument, requestViewerPin)
import Miso qualified as M
import Miso.Router qualified as M

-- | Embeddable update: handles effectful operations for assignment menu actions.
updateAssignmentDetailed
  :: SyncContext
  -> (V.AssignmentDetailedAction -> action)
  -> V.AssignmentDetailedAction
  -> M.Effect parent model action
updateAssignmentDetailed r _lift = go
  where
    go (V.MenuEdit aid) =
      M.io_ $ modifySyncDocument r $ Assignments (OnAssignments (Modify aid Lock))
    go (V.MenuPin assignment) =
      M.io_ $ requestViewerPin r (PinAssignmentViewer assignment)
    go (V.MenuGoTo _aid) =
      M.io_ $ M.pushURI (M.toURI ManageAssignments)
    go (V.MenuEvaluate assignment) =
      M.io_ $ pinAssignmentEvaluator r assignment
