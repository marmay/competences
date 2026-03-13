{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.Assignment.ImportModal
-- Description : Modal component for importing assignments with embedded tasks
--
-- Provides a modal dialog for importing assignments and their tasks from
-- a markdown-like format. Shows a preview of changes before applying.
module Competences.Frontend.Component.Assignment.ImportModal
  ( assignmentImportModalComponent
  , openAssignmentImportModal
  , Action
  )
where

import Competences.Command (AssignmentPatch (..), ModifyCommand (..), SolutionPatch (..), TaskPatch (..))
import Competences.Command qualified as Cmd
import Competences.Document (Document (..))
import Competences.Document.Assignment (Assignment (..), AssignmentName (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Id (Id (..))
import Competences.Document.Solution (Solution (..))
import Competences.Document.Task (Task (..), TaskAttributes (..), TaskIdentifier (..), TaskPurpose (..), TaskType (..), defaultTaskAttributes, taskDisplayName)
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ImportModal qualified as IM
import Competences.Frontend.SyncContext
  ( SyncContext (..)
  , modifySyncDocument
  , nextId
  )
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalId (..), ModalHeight (..), ModalWidth (..), WindowChrome (..), WindowMode, closeWindow, openFramedModalWith)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
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
import Competences.Query.User qualified as QUser
import Data.List (sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html qualified as MH

-- ============================================================================
-- Types (re-exports from generic module)
-- ============================================================================

type Action = IM.Action

-- | Open the assignment import modal as a framed modal.
openAssignmentImportModal :: SyncContext -> IO ()
openAssignmentImportModal r =
  let cfg = ModalConfig (WindowChrome (C.translate' C.LblImportAssignments) Icon.IcnImport) (ModalId "import-assignments") ModalWide ModalFull Nothing
   in openFramedModalWith r.windowManager cfg (assignmentImportModalComponent r)

-- ============================================================================
-- Component
-- ============================================================================

assignmentImportModalComponent :: SyncContext -> WindowMode -> M.Component p (IM.Model AssignmentImportPreview) Action
assignmentImportModalComponent = IM.importModalComponent assignmentImportConfig

assignmentImportConfig :: IM.ImportModalConfig AssignmentImportPreview
assignmentImportConfig =
  IM.ImportModalConfig
    { parse = \doc input -> case parseAssignmentImport input of
        Left err -> Left err
        Right parsed -> Right $ matchAssignmentImport doc parsed
    , renderItem = previewAssignmentView
    , hasChanges = assignmentHasChanges
    , apply = applyAssignmentImport
    , placeholder = placeholderText
    }

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

previewAssignmentView :: AssignmentImportPreview -> M.View (IM.Model AssignmentImportPreview) IM.Action
previewAssignmentView preview =
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ -- Assignment header
      MH.div_
        [class_ "mb-2"]
        [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
            [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ assignmentName preview.assignmentAction]
            , IM.actionBadge preview.assignmentAction
            ]
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

previewTaskView :: TaskImportPreview -> M.View (IM.Model AssignmentImportPreview) IM.Action
previewTaskView preview =
  M.div_
    [class_ "py-1"]
    [ Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
        [ M.span_ [class_ "font-medium text-sm"] [M.text $ M.ms $ taskTitle preview.taskAction]
        , IM.actionBadge preview.taskAction
        ]
    , -- Solutions count
      if null preview.solutionActions
        then M.text ""
        else
          M.div_
            [class_ "text-xs text-muted-foreground mt-1"]
            [M.text $ M.ms $ "Lösungen: " <> T.pack (show (length preview.solutionActions))]
    , -- Primary competence matches
      if null preview.competenceMatches
        then M.text ""
        else
          M.div_
            [class_ "mt-1 space-y-1"]
            (map competenceMatchView preview.competenceMatches)
    , -- Secondary competence matches
      if null preview.secondaryCompetenceMatches
        then M.text ""
        else
          M.div_
            [class_ "mt-1 space-y-1"]
            ( M.span_ [class_ "text-xs text-muted-foreground"] [M.text "Sekundär:"]
                : map competenceMatchView preview.secondaryCompetenceMatches
            )
    ]

taskTitle :: ImportAction Task -> Text
taskTitle (Create t) = taskDisplayName t
taskTitle (Update _ t) = taskDisplayName t
taskTitle (NoChange t) = taskDisplayName t

competenceMatchView :: CompetenceMatch -> M.View (IM.Model AssignmentImportPreview) IM.Action
competenceMatchView cm =
  M.div_
    [class_ "flex items-center gap-1 text-xs"]
    [ M.span_ [class_ "text-muted-foreground"] [M.text $ M.ms cm.gridName]
    , M.span_ [] [M.text "/"]
    , M.span_ [] [M.text $ M.ms $ T.take 20 cm.description <> if T.length cm.description > 20 then "..." else ""]
    , Badge.outline (Badge.badgeText $ M.ms $ levelToGerman cm.level)
    , case cm.matched of
        Just _ -> Badge.primary (Badge.badgeText "OK")
        Nothing -> Badge.destructive (Badge.badgeText "?")
    ]

-- ============================================================================
-- Change Detection
-- ============================================================================

assignmentHasChanges :: AssignmentImportPreview -> Bool
assignmentHasChanges preview =
  isChange preview.assignmentAction
    || any taskHasChanges preview.taskPreviews

isChange :: ImportAction a -> Bool
isChange (Create _) = True
isChange (Update _ _) = True
isChange (NoChange _) = False

taskHasChanges :: TaskImportPreview -> Bool
taskHasChanges tp =
  isChange tp.taskAction
    || any isChange tp.solutionActions

-- ============================================================================
-- Apply Import
-- ============================================================================

applyAssignmentImport :: SyncContext -> WindowMode -> Document -> [AssignmentImportPreview] -> IO ()
applyAssignmentImport r wm doc previews = do
  mapM_ (applyAssignmentPreview r doc) previews
  closeWindow wm

applyAssignmentPreview :: SyncContext -> Document -> AssignmentImportPreview -> IO ()
applyAssignmentPreview r doc preview = do
  taskIds <- mapM (applyTaskAndGetId r doc) preview.taskPreviews

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
              , studentIds = Set.empty
              , tasks = taskIds
              , groupSubmissionAllowed = False
              }
      modifySyncDocument r (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Create newAssignment)
    Update old new -> do
      modifySyncDocument r (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Modify old.id Lock)
      let patch = buildAssignmentPatch old new taskIds
      modifySyncDocument r (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Modify old.id (Release patch))
    NoChange _ -> pure ()

applyTaskAndGetId :: SyncContext -> Document -> TaskImportPreview -> IO (Id Task)
applyTaskAndGetId r doc preview = do
  let teachers = QUser.teachers doc
      mTeacherId = (.id) <$> listToMaybe teachers
      matchedPrimary = mapMaybe (.matched) preview.competenceMatches
      matchedSecondary = mapMaybe (.matched) preview.secondaryCompetenceMatches
      purpose = maybe defaultTaskAttributes.purpose id preview.parsedPurpose

  taskId <- case preview.taskAction of
    Create t -> do
      newId <- nextId r
      let taskAttrs =
            TaskAttributes
              { primary = matchedPrimary
              , secondary = matchedSecondary
              , purpose = purpose
              , displayInResources = defaultTaskAttributes.displayInResources
              }
          newTask =
            Task
              { id = newId
              , identifier = t.identifier
              , title = t.title
              , content = t.content
              , taskType = SelfContained taskAttrs
              , attachments = []
              }
      modifySyncDocument r (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Create newTask)
      pure newId
    Update old new -> do
      modifySyncDocument r (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Modify old.id Lock)
      let patch = buildTaskPatch old new matchedPrimary matchedSecondary preview.parsedPurpose
      modifySyncDocument r (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Modify old.id (Release patch))
      pure old.id
    NoChange t -> pure t.id

  mapM_ (applySolutionAction r taskId mTeacherId) preview.solutionActions
  pure taskId

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
  Update old new -> do
    modifySyncDocument r (Cmd.Solutions $ Cmd.OnSolutions $ Cmd.Modify old.id Lock)
    let patch = buildSolutionPatch old new
    modifySyncDocument r (Cmd.Solutions $ Cmd.OnSolutions $ Cmd.Modify old.id (Release patch))
  NoChange _ -> pure ()

-- ============================================================================
-- Patch Builders
-- ============================================================================

buildAssignmentPatch :: Assignment -> Assignment -> [Id Task] -> AssignmentPatch
buildAssignmentPatch old new taskIds =
  AssignmentPatch
    { name = if old.name == new.name then Nothing else Just (old.name, new.name)
    , description = if old.description == new.description then Nothing else Just (old.description, new.description)
    , assignmentDate = if old.assignmentDate == new.assignmentDate then Nothing else Just (old.assignmentDate, new.assignmentDate)
    , activityType = if old.activityType == new.activityType then Nothing else Just (old.activityType, new.activityType)
    , studentIds = Nothing
    , tasks = if old.tasks == taskIds then Nothing else Just (old.tasks, taskIds)
    , groupSubmissionAllowed = Nothing
    }

buildTaskPatch :: Task -> Task -> [CompetenceLevelId] -> [CompetenceLevelId] -> Maybe TaskPurpose -> TaskPatch
buildTaskPatch old new matchedPrimary matchedSecondary parsedPurpose =
  let oldPrimary = getTaskPrimary old
      oldSecondary = getTaskSecondary old
      oldPurpose = getTaskPurposeField old
   in TaskPatch
        { identifier = if old.identifier == new.identifier then Nothing else Just (old.identifier, new.identifier)
        , title = if old.title == new.title then Nothing else Just (old.title, new.title)
        , content = if old.content == new.content then Nothing else Just (old.content, new.content)
        , primary = if oldPrimary == matchedPrimary then Nothing else Just (oldPrimary, matchedPrimary)
        , secondary = if oldSecondary == matchedSecondary then Nothing else Just (oldSecondary, matchedSecondary)
        , purpose = case parsedPurpose of
            Just p | oldPurpose /= Just p -> Just (maybe Practice id oldPurpose, p)
            _ -> Nothing
        , displayInResources = Nothing
        , attachments = Nothing
        }

getTaskPrimary :: Task -> [CompetenceLevelId]
getTaskPrimary task = case task.taskType of
  SelfContained attrs -> attrs.primary
  SubTask _ _ -> []

getTaskSecondary :: Task -> [CompetenceLevelId]
getTaskSecondary task = case task.taskType of
  SelfContained attrs -> attrs.secondary
  SubTask _ _ -> []

getTaskPurposeField :: Task -> Maybe TaskPurpose
getTaskPurposeField task = case task.taskType of
  SelfContained attrs -> Just attrs.purpose
  SubTask _ _ -> Nothing

buildSolutionPatch :: Solution -> Solution -> SolutionPatch
buildSolutionPatch old new =
  SolutionPatch
    { solutionType = if old.solutionType == new.solutionType then Nothing else Just (old.solutionType, new.solutionType)
    , content = if old.content == new.content then Nothing else Just (old.content, new.content)
    }
