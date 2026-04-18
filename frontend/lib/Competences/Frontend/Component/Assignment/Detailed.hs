-- | Detailed assignment view: state machine, pure views, and effectful update.
module Competences.Frontend.Component.Assignment.Detailed
  ( AssignmentDetailedState (..)
  , AssignmentDetailedAction (..)
  , initialAssignmentDetailedState
  , updateAssignmentDetailedPure
  , assignmentEntityMenu
  , assignmentHeaderView
  , assignmentCardView
  , updateAssignmentDetailed
  )
where

import Competences.Command (AssignmentsCommand (..), Command (..), EntityCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.EvaluatorDetail (pinAssignmentEvaluator)
import Competences.Frontend.Component.Draft (retargetForDraft)
import Competences.Frontend.Fragment.EvidenceIcon qualified as EvidenceIcon
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (PinViewerRequest (..), SyncContext, SyncDocument (..), modifySyncDocument, readSyncDocument, requestViewerPin)
import Competences.Frontend.View.EntityMenu (menuCustom, menuEdit, menuGoTo, menuPin)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Router qualified as M
import Optics.Core (Lens', (%), (%~), (.~))

-- ============================================================================
-- State machine
-- ============================================================================

newtype AssignmentDetailedState = AssignmentDetailedState
  { menuOpen :: Bool
  }
  deriving (Eq, Generic, Show)

initialAssignmentDetailedState :: AssignmentDetailedState
initialAssignmentDetailedState = AssignmentDetailedState {menuOpen = False}

data AssignmentDetailedAction
  = MenuEdit !AssignmentId
  | MenuPin !Assignment
  | MenuGoTo !AssignmentId
  | MenuEvaluate !Assignment
  | MenuToggle
  | MenuClose
  deriving (Eq, Show)

updateAssignmentDetailedPure :: AssignmentDetailedAction -> AssignmentDetailedState -> AssignmentDetailedState
updateAssignmentDetailedPure MenuToggle = #menuOpen %~ not
updateAssignmentDetailedPure MenuClose = #menuOpen .~ False
updateAssignmentDetailedPure _ = id

-- ============================================================================
-- Entity menu
-- ============================================================================

assignmentEntityMenu :: Bool -> Assignment -> [M.View m AssignmentDetailedAction]
assignmentEntityMenu isTeacher a =
  [ menuEdit (MenuEdit a.id)
  , menuPin (MenuPin a)
  , menuGoTo (MenuGoTo a.id)
  ]
    ++ [ menuCustom Icon.IcnApply (C.translate' C.LblEvaluateAssignment) (MenuEvaluate a)
       | isTeacher
       ]

-- ============================================================================
-- Views
-- ============================================================================

-- | Assignment header: activity type icon + name + date, with an annotations slot.
assignmentHeaderView
  :: Assignment
  -> [M.View m a]
  -> M.View m a
assignmentHeaderView a annotations =
  let AssignmentName nameText = a.name
   in Layout.hFlow (Layout.gapS <> Layout.crossCenter)
        ( [ Icon.iconS Icon.Small (EvidenceIcon.activityTypeIcon a.activityType)
          , MH.span_ [class_ "font-medium"] [M.text $ M.ms nameText]
          , MH.span_ [class_ "text-sm text-muted-foreground"] [M.text $ C.formatDay a.assignmentDate]
          ]
            ++ annotations
        )

-- | Card view wrapping the header, for use in cross-reference lists.
assignmentCardView
  :: Assignment
  -> [M.View m a]
  -> M.View m a
assignmentCardView a annotations =
  MH.div_
    [class_ "border rounded-lg px-3 py-2"]
    [assignmentHeaderView a annotations]

-- ============================================================================
-- Effectful update
-- ============================================================================

-- | Embeddable update: handles effectful operations for assignment menu actions.
updateAssignmentDetailed
  :: Lens' model AssignmentDetailedState
  -> SyncContext
  -> (AssignmentDetailedAction -> action)
  -> AssignmentDetailedAction
  -> M.Effect parent model action
updateAssignmentDetailed stateLens r _lift = go
  where
    go (MenuEdit aid) = do
      dismiss
      M.io_ $ do
        sd <- readSyncDocument r
        let isDraft = not $ Ix.null (sd.localDocument.draftAssignments Ix.@= aid)
            wrap = if isDraft then retargetForDraft else id
        modifySyncDocument r $ wrap $ Assignments (OnAssignments (Modify aid Lock))
    go (MenuPin assignment) = do
      dismiss
      M.io_ $ requestViewerPin r (PinAssignmentViewer assignment)
    go (MenuGoTo _aid) = do
      dismiss
      M.io_ $ M.pushURI (M.toURI ManageAssignments)
    go (MenuEvaluate assignment) = do
      dismiss
      M.io_ $ pinAssignmentEvaluator r assignment
    go action = M.modify (stateLens %~ updateAssignmentDetailedPure action)

    dismiss = M.modify (stateLens % #menuOpen .~ False)
