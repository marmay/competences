-- | Self-contained assignment editor for pinned dialogs.
module Competences.Frontend.Component.Assignment.PinEditor
  ( assignmentPinEditor
  , nameViewLens
  , namePatchLens
  , userSearchConfig
  )
where

import Competences.Command (AssignmentPatch (..), AssignmentsCommand (..), Command (..), EntityCommand (..), TasksCommand (..))
import Competences.Document.Task (defaultTask)
import Competences.Frontend.SyncContext (nextId)
import Competences.Command.Common (Change)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), Lock (..), User (..), lockOwner, TaskIxs)
import Competences.Document.Assignment (AssignmentId, AssignmentName (..))
import Competences.Document.Task (Task (..), TaskId, TaskIdentifier (..), taskDisplayName)
import Competences.Document.User (UserId, isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (EntityOrigin (..), wrapForOrigin)
import Competences.Frontend.Component.Editor (Editable (..), addNamedField, boolEditorField, dayEditorField, editable, editor, editorComponent, enumEditorField, richTextEditorField, textEditorField)
import Competences.Frontend.Component.Editor.FormView (editorFormView')
import Competences.Frontend.Component.Editor.Types (Action, Model)
import Competences.Frontend.Component.Selector.Common (entityPatchTransformedLens)
import Competences.Frontend.Component.Selector.SearchSelect (SearchSelectConfig (..), SelectionOrder (..), TagLayout (..))
import Competences.Frontend.Component.Selector.SearchSelectEditorField (AddAction (..), addableSearchSelectEditorField, searchSelectEditorField)
import Competences.Frontend.SyncContext (SyncContext (..))
import Competences.Frontend.SyncContext.WindowManager (PinId, WindowMode, justLens, pinSaveStateLens)
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Competences.Frontend.View.Icon qualified as Icon
import Data.Default (Default (..))
import Data.List (sortOn)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Iso', Lens', iso, (%), (&), (?~))
import Optics.Core qualified as O

assignmentPinEditor
  :: SyncContext
  -> AssignmentId
  -> EntityOrigin
  -> PinId
  -> WindowMode
  -> Maybe (Model Assignment AssignmentPatch Maybe)
  -> M.Component WM.Model (Model Assignment AssignmentPatch Maybe) (Action Assignment AssignmentPatch)
assignmentPinEditor r assignmentId origin pid _mode mSaved =
  (editorComponent assignmentEditor r mSaved def)
    { M.bindings =
        [O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL justLens]
    }
  where
    wrap = wrapForOrigin origin
    editorId = "assignment-pin-editor-" <> ms (show assignmentId)

    assignmentEditable =
      editable
        ( \d ->
            let mAssignment = case origin of
                  Published -> Ix.getOne (d.assignments Ix.@= assignmentId)
                  Draft -> Ix.getOne (d.draftAssignments Ix.@= assignmentId)
             in fmap (\a -> (a, lockOwner (AssignmentLock a.id) d)) mAssignment
        )
        & (#modify ?~ (\a modify -> wrap $ Assignments $ OnAssignments (Modify a.id modify)))

    assignmentEditor =
      editor
        ( editorFormView'
            (C.translate' C.LblEditAssignment)
            id
        )
        assignmentEditable
        `addNamedField` ( C.translate' C.LblAssignmentName
                        , textEditorField nameViewLens namePatchLens
                        )
        `addNamedField` ( C.translate' C.LblAssignmentDescription
                        , richTextEditorField r.formulaCache "description" #description #description
                        )
        `addNamedField` ( C.translate' C.LblAssignmentDate
                        , dayEditorField #assignmentDate #assignmentDate
                        )
        `addNamedField` ( C.translate' C.LblActivityType
                        , enumEditorField
                            (C.translate' . C.LblActivityTypeDescription)
                            #activityType
                            #activityType
                        )
        `addNamedField` ( C.translate' C.LblStudents
                        , searchSelectEditorField
                            r
                            (editorId <> "-users")
                            userSearchConfig
                            (Set.toList . (.studentIds))
                            (entityPatchTransformedLens #studentIds #studentIds (.id) Set.fromList)
                        )
        `addNamedField` ( C.translate' C.LblAssignmentTasks
                        , addableSearchSelectEditorField
                            r
                            (editorId <> "-tasks")
                            (taskSearchConfig origin)
                            (.tasks)
                            (entityPatchTransformedLens #tasks #tasks (.id) id)
                            [ AddAction
                                { label = C.LblAddTask
                                , icon = Icon.IcnAdd
                                , mkSpec = \entity patch -> do
                                    newTid <- nextId r
                                    let original = entity.tasks
                                        currentNew = maybe original snd (patch.tasks)
                                    pure
                                      ( wrap $ Tasks (OnTasks (CreateAndLock (defaultTask newTid)))
                                      , \p -> p & #tasks ?~ (original, currentNew <> [newTid])
                                      )
                                }
                            ]
                        )
        `addNamedField` ( C.translate' C.LblGroupSubmissionAllowed
                        , boolEditorField #groupSubmissionAllowed #groupSubmissionAllowed
                        )

-- ============================================================================
-- Name lenses (shared with EditorDetail)
-- ============================================================================

assignmentNameTextIso :: Iso' AssignmentName T.Text
assignmentNameTextIso = iso (\(AssignmentName t) -> t) AssignmentName

changeAssignmentNameTextIso :: Iso' (Change AssignmentName) (Change T.Text)
changeAssignmentNameTextIso = iso (fmap go) (fmap back)
  where
    go (old, new) = ((\(AssignmentName t) -> t) old, (\(AssignmentName t) -> t) new)
    back (old, new) = (AssignmentName old, AssignmentName new)

nameViewLens :: Lens' Assignment T.Text
nameViewLens = #name % assignmentNameTextIso

namePatchLens :: Lens' AssignmentPatch (Change T.Text)
namePatchLens = #name % changeAssignmentNameTextIso

-- ============================================================================
-- SearchSelect configs
-- ============================================================================

taskSearchConfig :: EntityOrigin -> SearchSelectConfig Task TaskId
taskSearchConfig origin =
  SearchSelectConfig
    { projectItems = tasksFactoryFor origin
    , itemId = (.id)
    , itemLabel = taskDisplayName
    , metaFilters = []
    , viewTag = \t -> (Icon.IcnTask, ms $ taskDisplayName t)
    , placeholder = M.fromMisoString $ C.translate' C.LblSelectTasks
    , selectionOrder = ManualReorder
    , tagLayout = TagsInline
    , onCreate = Nothing
    }

tasksFactoryFor :: EntityOrigin -> Document -> [Task]
tasksFactoryFor Published doc = tasksList doc.tasks
tasksFactoryFor Draft doc = tasksList doc.tasks <> tasksList doc.draftTasks

tasksList :: Ix.IxSet TaskIxs Task -> [Task]
tasksList = Ix.toAscList (Proxy @TaskIdentifier)

userSearchConfig :: SearchSelectConfig User UserId
userSearchConfig =
  SearchSelectConfig
    { projectItems = \doc ->
        sortOn (.name) $ filter isStudent $ Ix.toList doc.users
    , itemId = (.id)
    , itemLabel = (.name)
    , metaFilters = []
    , viewTag = \u -> (Icon.IcnSocialFormIndividual, ms u.name)
    , placeholder = "Schüler auswählen..."
    , selectionOrder = AutoOrder id
    , tagLayout = TagsInline
    , onCreate = Nothing
    }
