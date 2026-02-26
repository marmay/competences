module Competences.Frontend.Component.Assignment.EditorDetail
  ( editorDetailView
  , pinAssignmentEvaluator
  )
where

import Competences.Command (AssignmentPatch (..), AssignmentsCommand (..), Command (..), EntityCommand (..))
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Document (..)
  , Lock (..)
  , User (..)
  )
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Id (idToText)
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.EvaluatorDetail (evaluatorComponent)
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.ExportButton (exportButtonComponent)
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.MultiTaskSelector (multiTaskEditorField)
import Competences.Frontend.Component.Selector.UserSelector (searchableMultiUserEditorField)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager (PinCategory (..), PinMeta (..), SortAtom (..), SortKey (..), WindowChrome (..), inlineComponent, pinDialog)
import Competences.Frontend.View.EvidenceIcon qualified as EvidenceIcon
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Import.Export (exportAssignment)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core (Iso', Lens', iso, (%), (&), (?~), (^.), (.~))

-- ============================================================================
-- Wrapper Model and Actions
-- ============================================================================

newtype EditorModel = EditorModel
  { document :: Document
  }
  deriving (Eq, Generic, Show)

data EditorAction
  = DocumentUpdated !DocumentChange
  | PinEvaluation
  deriving (Eq, Show)

-- | Pin the assignment evaluator as a persistent dialog.
pinAssignmentEvaluator :: SyncContext -> Assignment -> IO ()
pinAssignmentEvaluator r assignment =
  let AssignmentName nameText = assignment.name
      pinTitle = C.translate' C.LblEvaluateAssignment
        <> ": " <> M.ms nameText
      meta = PinMeta
        { key = "assignment-evaluation-" <> idToText assignment.id
        , category = PinCatAssignment
        , sortKey = SortKey [SortAtom assignment.assignmentDate, SortAtom assignment.activityType, SortAtom nameText, SortAtom assignment.id]
        , context = Just (C.formatDayShort assignment.assignmentDate)
        }
   in pinDialog r.windowManager
        meta
        (WindowChrome pinTitle (EvidenceIcon.activityTypeIcon assignment.activityType))
        (evaluatorComponent r assignment)

-- | Detail view for editing an assignment
-- The mode type parameter allows this to work with any mode type
editorDetailView
  :: SyncContext
  -> Assignment
  -> M.View (SD.Model Assignment mode) (SD.Action mode)
editorDetailView r assignment =
  inlineComponent
    ("assignment-editor-wrapper-" <> M.ms (show assignment.id))
    (editorWrapperComponent r assignment)

editorWrapperComponent :: SyncContext -> Assignment -> M.Component p EditorModel EditorAction
editorWrapperComponent r assignment =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel = EditorModel {document = emptyDocument}

    emptyDocument =
      Document
        { competenceGrids = Ix.empty
        , competences = Ix.empty
        , users = Ix.empty
        , evidences = Ix.empty
        , locks = mempty
        , tasks = Ix.empty
        , taskGroups = Ix.empty
        , solutions = Ix.empty
        , resources = Ix.empty
        , assignments = Ix.empty
        , competenceAssessments = Ix.empty
        , competenceGridGrades = Ix.empty
        , mesoPlans = Ix.empty
        , lessons = Ix.empty
        , lessonNotes = Ix.empty
        , participationRecords = Ix.empty
        , absences = Ix.empty
        }

    update (DocumentUpdated dc) = M.modify $ #document .~ dc.document

    update PinEvaluation = M.io_ $ pinAssignmentEvaluator r assignment

    view _m =
      Layout.vFlow Layout.gapM
        [ inlineComponent
            ("assignment-editor-" <> M.ms (show assignment.id))
            (TE.editorComponent assignmentEditor r)
        , MH.div_
            [class_ "flex justify-end gap-2"]
            [ Button.outline $ Button.button (Icon.IcnApply, C.LblEvaluateAssignment) PinEvaluation
            , inlineComponent
                ("export-btn-" <> M.ms (show assignment.id))
                (exportButtonComponent (\m' -> exportAssignment m'.document assignment))
            ]
        ]

    assignmentEditorId = "assignment-editor-" <> M.ms (show assignment.id)

    assignmentEditable =
      TE.editable
        ( \d ->
            fmap
              (\c -> (c, (d ^. #locks) Map.!? AssignmentLock c.id))
              (Ix.getOne $ d.assignments Ix.@= assignment.id)
        )
        & (#modify ?~ (\a modify -> Assignments $ OnAssignments (Modify a.id modify)))
        & (#delete ?~ (\a -> Assignments $ OnAssignments (Delete a.id)))

    assignmentEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblEditAssignment)
            id
        )
        assignmentEditable
        `TE.addNamedField` ( C.translate' C.LblAssignmentName
                           , TE.textEditorField nameViewLens namePatchLens
                           )
        `TE.addNamedField` ( C.translate' C.LblAssignmentDescription
                           , TE.richTextEditorField r.formulaCache "description" #description #description
                           )
        `TE.addNamedField` ( C.translate' C.LblAssignmentDate
                           , TE.dayEditorField #assignmentDate #assignmentDate
                           )
        `TE.addNamedField` ( C.translate' C.LblActivityType
                           , TE.enumEditorField
                               (C.translate' . C.LblActivityTypeDescription)
                               #activityType
                               #activityType
                           )
        `TE.addNamedField` ( C.translate' C.LblStudents
                           , searchableMultiUserEditorField
                               r
                               (assignmentEditorId <> "-users")
                               isStudent
                               (entityPatchTransformedLens #studentIds #studentIds (\u -> u.id) Set.fromList)
                           )
        `TE.addNamedField` ( C.translate' C.LblAssignmentTasks
                           , multiTaskEditorField
                               r
                               (assignmentEditorId <> "-tasks")
                               (entityPatchTransformedLens #tasks #tasks id id)
                           )

-- | Iso for converting between AssignmentName and Text
assignmentNameTextIso :: Iso' AssignmentName T.Text
assignmentNameTextIso = iso getter setter
  where
    getter (AssignmentName t) = t
    setter t = AssignmentName t

-- | Iso for converting Change AssignmentName to Change Text
changeAssignmentNameTextIso :: Iso' (Change AssignmentName) (Change T.Text)
changeAssignmentNameTextIso = iso (fmap convertChange) (fmap convertChange')
  where
    convertChange (old, new) = (getter old, getter new)
    convertChange' (old, new) = (setter old, setter new)
    getter (AssignmentName t) = t
    setter t = AssignmentName t

-- | Lens for viewing assignment name as Text
nameViewLens :: Lens' Assignment T.Text
nameViewLens = #name % assignmentNameTextIso

-- | Lens for patching assignment name as Text
namePatchLens :: Lens' AssignmentPatch (Change T.Text)
namePatchLens = #name % changeAssignmentNameTextIso
