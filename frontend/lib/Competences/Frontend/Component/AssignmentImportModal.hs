{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.AssignmentImportModal
-- Description : Modal component for importing assignments with embedded tasks
--
-- Provides a modal dialog for importing assignments and their tasks from
-- a markdown-like format. Shows a preview of changes before applying.
module Competences.Frontend.Component.AssignmentImportModal
  ( assignmentImportModalComponent
  , Action (..)
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Assignment (Assignment (..), AssignmentName (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Solution (Solution (..))
import Competences.Document.Task (Task (..), TaskIdentifier (..))
import Competences.Document.User (User (..), isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View.Badge (BadgeVariant (..), badge)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Modal (modalHost)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Import.AssignmentParser (parseAssignmentImport)
import Competences.Import.Matching (matchAssignmentImport)
import Competences.Import.Types
  ( AssignmentImportPreview (..)
  , CompetenceMatch (..)
  , ImportAction (..)
  , TaskImportPreview (..)
  , activityTypeToGerman
  , levelToGerman
  )
import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~))

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { inputText :: !Text
  , parseResult :: !(Either String [AssignmentImportPreview])
  , document :: !Document
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = DocumentUpdated !DocumentChange
  | SetInputText !Text
  | ParseInput
  | ApplyImport
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

assignmentImportModalComponent :: SyncContext -> M.Component p Model Action
assignmentImportModalComponent r =
  (M.component model update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    model =
      Model
        { inputText = ""
        , parseResult = Right []
        , document = emptyDocument
        }

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
        }

    update (DocumentUpdated dc) =
      M.modify $ #document .~ dc.document

    update (SetInputText t) =
      M.modify $ #inputText .~ t

    update ParseInput = do
      m <- M.get
      let result = case parseAssignmentImport m.inputText of
            Left err -> Left err
            Right parsed -> Right $ matchAssignmentImport m.document parsed
      M.modify $ #parseResult .~ result

    update ApplyImport = do
      m <- M.get
      case m.parseResult of
        Right previews -> M.io_ $ applyPreviews r m.document previews
        Left _ -> pure ()

    update CloseModal = pure () -- Parent handles this

    view :: Model -> M.View Model Action
    view m =
      modalHost
        []
        [ M.div_
            [class_ "bg-popover text-popover-foreground rounded-xl shadow-lg w-[80vw] h-[80vh] max-w-[80vw] flex flex-col"]
            [ -- Header
              M.div_
                [class_ "flex items-center justify-between p-4 border-b border-border"]
                [ Typography.h2 (C.translate' C.LblImportAssignments)
                , Button.buttonGhost ""
                    & Button.withIcon IcnCancel
                    & Button.withClick CloseModal
                    & Button.renderButton
                ]
            , -- Content
              M.div_
                [class_ "flex-1 min-h-0 flex gap-4 p-4 overflow-hidden"]
                [ -- Left: Input area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0 flex-1 w-1/2"]
                    [ Typography.h3 "Eingabe"
                    , M.textarea_
                        [ class_ "flex-1 min-h-0 w-full p-3 font-mono text-sm border border-input rounded-md bg-background resize-none"
                        , MP.placeholder_ placeholderText
                        , MP.value_ (M.ms m.inputText)
                        , M.onInput (SetInputText . M.fromMisoString)
                        ]
                        []
                    ]
                , -- Right: Preview area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0 flex-1 w-1/2"]
                    [ Typography.h3 "Vorschau"
                    , M.div_
                        [class_ "flex-1 min-h-0 overflow-y-auto border border-border rounded-md p-3 bg-muted/30"]
                        [previewView m]
                    ]
                ]
            , -- Footer
              M.div_
                [class_ "flex justify-end gap-2 p-4 border-t border-border"]
                [ Button.buttonSecondary (C.translate' C.LblCancel)
                    & Button.withClick CloseModal
                    & Button.renderButton
                , Button.buttonPrimary "Vorschau"
                    & Button.withClick ParseInput
                    & Button.renderButton
                , case m.parseResult of
                    Right previews
                      | not (null previews) && any hasChanges previews ->
                          Button.buttonPrimary (C.translate' C.LblApply)
                            & Button.withIcon IcnApply
                            & Button.withClick ApplyImport
                            & Button.renderButton
                    _ -> M.text ""
                ]
            ]
        ]

placeholderText :: M.MisoString
placeholderText =
  "# Aufgabenname\n\n\
  \## Beschreibung\n\
  \Beschreibungstext...\n\n\
  \## Angaben\n\
  \Date: 2026-01-25\n\
  \Type: Hausübung\n\n\
  \### Buch-1.2.3\n\n\
  \#### Angabe\n\
  \Aufgabentext...\n\n\
  \#### Kompetenzen\n\
  \- Rastername / Kompetenz / Wesentlich\n\n\
  \#### Hinweis\n\
  \Optionaler Hinweis..."

-- ============================================================================
-- Preview View
-- ============================================================================

previewView :: Model -> M.View Model Action
previewView m = case m.parseResult of
  Left err ->
    M.div_
      [class_ "text-destructive"]
      [M.text $ M.ms $ "Fehler: " <> err]
  Right [] ->
    M.div_
      [class_ "text-muted-foreground italic"]
      [M.text "Keine Eingabe. Geben Sie Text ein und klicken Sie auf 'Vorschau'."]
  Right previews ->
    M.div_
      [class_ "flex flex-col gap-4"]
      (map previewAssignmentView previews)

previewAssignmentView :: AssignmentImportPreview -> M.View Model Action
previewAssignmentView preview =
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ -- Assignment header
      M.div_
        [class_ "flex items-center gap-2 mb-2"]
        [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ assignmentName preview.assignmentAction]
        , actionBadge preview.assignmentAction
        ]
    , -- Assignment metadata
      M.div_
        [class_ "text-sm text-muted-foreground mb-2"]
        [ M.text $ M.ms $ formatMetadata preview.assignmentAction
        ]
    , -- Tasks (sorted by identifier)
      if null preview.taskPreviews
        then M.text ""
        else
          M.div_
            [class_ "pl-4 border-l-2 border-border space-y-2"]
            (map previewTaskView $ sortBy (comparing taskIdentifier) preview.taskPreviews)
    ]

-- | Extract task identifier from TaskImportPreview for sorting
taskIdentifier :: TaskImportPreview -> TaskIdentifier
taskIdentifier tp = case tp.taskAction of
  Create t -> t.identifier
  Update _ t -> t.identifier
  NoChange t -> t.identifier

assignmentName :: ImportAction Assignment -> Text
assignmentName (Create a) = let AssignmentName n = a.name in n
assignmentName (Update _ a) = let AssignmentName n = a.name in n
assignmentName (NoChange a) = let AssignmentName n = a.name in n

formatMetadata :: ImportAction Assignment -> Text
formatMetadata action =
  let a = case action of
        Create x -> x
        Update _ x -> x
        NoChange x -> x
   in formatDay a.assignmentDate <> " | " <> activityTypeToGerman a.activityType

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

previewTaskView :: TaskImportPreview -> M.View Model Action
previewTaskView preview =
  M.div_
    [class_ "py-1"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ M.span_ [class_ "font-medium text-sm"] [M.text $ M.ms $ taskTitle preview.taskAction]
        , actionBadge preview.taskAction
        ]
    , -- Solutions count
      if null preview.solutionActions
        then M.text ""
        else
          M.div_
            [class_ "text-xs text-muted-foreground mt-1"]
            [M.text $ M.ms $ "Lösungen: " <> T.pack (show (length preview.solutionActions))]
    , -- Competence matches
      if null preview.competenceMatches
        then M.text ""
        else
          M.div_
            [class_ "mt-1 space-y-1"]
            (map competenceMatchView preview.competenceMatches)
    ]

taskTitle :: ImportAction Task -> Text
taskTitle (Create t) = let TaskIdentifier ident = t.identifier in ident
taskTitle (Update _ t) = let TaskIdentifier ident = t.identifier in ident
taskTitle (NoChange t) = let TaskIdentifier ident = t.identifier in ident

competenceMatchView :: CompetenceMatch -> M.View Model Action
competenceMatchView cm =
  M.div_
    [class_ "flex items-center gap-1 text-xs"]
    [ M.span_ [class_ "text-muted-foreground"] [M.text $ M.ms cm.gridName]
    , M.span_ [] [M.text "/"]
    , M.span_ [] [M.text $ M.ms $ T.take 20 cm.description <> if T.length cm.description > 20 then "..." else ""]
    , badge BadgeOutline (M.ms $ levelToGerman cm.level)
    , case cm.matched of
        Just _ -> badge BadgePrimary "OK"
        Nothing -> badge BadgeDestructive "?"
    ]

actionBadge :: ImportAction a -> M.View Model Action
actionBadge (Create _) = badge BadgePrimary "Neu"
actionBadge (Update _ _) = badge BadgeSecondary "Aktualisiert"
actionBadge (NoChange _) = badge BadgeOutline "Unverändert"

-- ============================================================================
-- Apply Import
-- ============================================================================

hasChanges :: AssignmentImportPreview -> Bool
hasChanges preview =
  isChange preview.assignmentAction
    || any taskHasChanges preview.taskPreviews
  where
    isChange (Create _) = True
    isChange (Update _ _) = True
    isChange (NoChange _) = False

    taskHasChanges tp =
      isChange tp.taskAction
        || any isChange tp.solutionActions

-- | Apply all assignment import previews
applyPreviews :: SyncContext -> Document -> [AssignmentImportPreview] -> IO ()
applyPreviews r doc previews = mapM_ (applyAssignmentPreview r doc) previews

-- | Apply a single assignment import preview
applyAssignmentPreview :: SyncContext -> Document -> AssignmentImportPreview -> IO ()
applyAssignmentPreview r doc preview = do
  -- First, apply all tasks and collect their IDs
  taskIds <- mapM (applyTaskAndGetId r doc) preview.taskPreviews

  -- Then create/update the assignment with the task IDs
  case preview.assignmentAction of
    Create a -> do
      newId <- nextId r
      let newAssignment =
            Assignment
              { id = newId
              , name = a.name
              , description = a.description
              , assignmentDate = a.assignmentDate
              , activityType = a.activityType
              , studentIds = Set.empty -- Start with no students
              , tasks = taskIds
              }
      modifySyncDocument r (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Create newAssignment)
    Update _ _ -> do
      -- For updates, we'd need to modify the assignment
      -- Currently not implemented
      pure ()
    NoChange _ -> pure ()

-- | Apply a task preview and return its ID
applyTaskAndGetId :: SyncContext -> Document -> TaskImportPreview -> IO (Id Task)
applyTaskAndGetId r doc preview = do
  -- Find a teacher to use as solution author
  let teachers = filter isTeacher $ Ix.toList doc.users
      mTeacherId = (.id) <$> listToMaybe teachers

  taskId <- case preview.taskAction of
    Create t -> do
      newId <- nextId r
      let newTask =
            Task
              { id = newId
              , identifier = t.identifier
              , content = t.content
              , taskType = t.taskType
              }
      modifySyncDocument r (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Create newTask)
      pure newId
    Update _old new -> pure new.id
    NoChange t -> pure t.id

  -- Apply solutions
  mapM_ (applySolutionAction r taskId mTeacherId) preview.solutionActions

  pure taskId

-- | Apply a single solution import action
applySolutionAction :: SyncContext -> Id Task -> Maybe (Id User) -> ImportAction Solution -> IO ()
applySolutionAction r taskId mTeacherId action = case action of
  Create s -> case mTeacherId of
    Just teacherId -> do
      newId <- nextId r
      let newSolution =
            Solution
              { id = newId
              , taskId = taskId
              , userId = teacherId
              , solutionType = s.solutionType
              , content = s.content
              }
      modifySyncDocument r (Cmd.Solutions $ Cmd.OnSolutions $ Cmd.Create newSolution)
    Nothing -> pure ()
  Update _ _ -> pure ()
  NoChange _ -> pure ()
