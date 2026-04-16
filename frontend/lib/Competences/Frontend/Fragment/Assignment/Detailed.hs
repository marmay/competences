-- | Detailed assignment view: pure view primitives and action types.
--
-- Effects for these actions live in 'Component.Assignment.Detailed.Embed'.
module Competences.Frontend.Fragment.Assignment.Detailed
  ( AssignmentDetailedState (..)
  , AssignmentDetailedAction (..)
  , initialAssignmentDetailedState
  , updateAssignmentDetailedPure
  , assignmentEntityMenu
  , assignmentHeaderView
  , assignmentCardView
  )
where

import Competences.Document (Assignment (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Fragment.EvidenceIcon qualified as EvidenceIcon
import Competences.Frontend.View.EntityMenu (menuCustom, menuEdit, menuGoTo, menuPin)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((%~), (.~))

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
