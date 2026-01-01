module Competences.Frontend.Component.AssignmentEditor
  ( assignmentEditorComponent
  )
where

import Competences.Command (AssignmentPatch (..), AssignmentsCommand (..), Command (..), EntityCommand (..))
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Assignment (..)
  , Document (..)
  , Lock (..)
  )
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.User (User (..), isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Selector.AssignmentSelector (assignmentSelectorComponent)
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.MultiTaskSelector (multiTaskEditorField)
import Competences.Frontend.Component.Selector.UserSelector (multiUserEditorField)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind (class_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Iso', Lens', iso, (%),  (&), (?~), (^.))

data Model = Model
  { assignment :: !(Maybe Assignment)
  }
  deriving (Eq, Generic, Show)

data Action

assignmentEditorComponent :: SyncDocumentRef -> M.Component p Model Action
assignmentEditorComponent r =
  M.component model update view
  where
    model = Model Nothing
    update _ = pure ()

    view m =
      V.sideMenu
        (V.componentA "assignment-editor-selection" [class_ "h-full"] (assignmentSelectorComponent r #assignment))
        (V.componentA assignmentEditorId [class_ "h-full"] (TE.editorComponent assignmentEditor r))
      where
        assignmentEditorId = "assignment-editor-editor-" <> maybe "empty" (M.ms . show . (.id)) m.assignment

        assignmentEditable =
          TE.editable
            ( \d -> do
                a <- m.assignment
                fmap
                  (\c -> (c, (d ^. #locks) Map.!? AssignmentLock c.id))
                  (Ix.getOne $ d.assignments Ix.@= a.id)
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
                               , multiUserEditorField
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
