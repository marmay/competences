-- | Embedding helpers for the detailed assignment view.
--
-- Handles effectful operations for 'AssignmentDetailedAction' from the
-- Fragment layer. Safe to import from any entity's component module.
module Competences.Frontend.Component.Assignment.Detailed.Embed
  ( updateAssignmentDetailed
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Frontend.Component.Assignment.EvaluatorDetail (pinAssignmentEvaluator)
import Competences.Frontend.Component.Draft (retargetForDraft)
import Competences.Frontend.Fragment.Assignment.Detailed qualified as V
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (PinViewerRequest (..), SyncContext, SyncDocument (..), modifySyncDocument, readSyncDocument, requestViewerPin)
import Miso qualified as M
import Miso.Router qualified as M
import Optics.Core (Lens', (%), (%~), (.~))

-- | Embeddable update: handles effectful operations for assignment menu actions.
updateAssignmentDetailed
  :: Lens' model V.AssignmentDetailedState
  -> SyncContext
  -> (V.AssignmentDetailedAction -> action)
  -> V.AssignmentDetailedAction
  -> M.Effect parent model action
updateAssignmentDetailed stateLens r _lift = go
  where
    go (V.MenuEdit aid) = do
      dismiss
      M.io_ $ do
        sd <- readSyncDocument r
        let isDraft = not $ Ix.null (sd.localDocument.draftAssignments Ix.@= aid)
            wrap = if isDraft then retargetForDraft else id
        modifySyncDocument r $ wrap $ Assignments (OnAssignments (Modify aid Lock))
    go (V.MenuPin assignment) = do
      dismiss
      M.io_ $ requestViewerPin r (PinAssignmentViewer assignment)
    go (V.MenuGoTo _aid) = do
      dismiss
      M.io_ $ M.pushURI (M.toURI ManageAssignments)
    go (V.MenuEvaluate assignment) = do
      dismiss
      M.io_ $ pinAssignmentEvaluator r assignment
    go action = M.modify (stateLens %~ V.updateAssignmentDetailedPure action)

    dismiss = M.modify (stateLens % #menuDismissed .~ True)
