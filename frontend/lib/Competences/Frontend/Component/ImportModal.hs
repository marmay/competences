{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Universal import modal: paste YAML, decode via the backend codec
-- (@\/api\/exchange\/decode@), match the result against the local
-- 'Document', preview the changes, then issue commands on Apply.
--
-- The exchange document is a flat package (six top-level entity
-- lists) so a single paste can carry any combination of tasks,
-- assignments, resources, and lessons. Apply runs in topological
-- order: tasks → resources → assignments → lessons.
module Competences.Frontend.Component.ImportModal
  ( importModalComponent
  , openImportModal
  , Action
  )
where

import Competences.Command
  ( AssignmentPatch (..)
  , Command
  , CompetenceGridPatch (..)
  , CompetencePatch (..)
  , LessonPatch (..)
  , LevelInfoPatch (..)
  , ModifyCommand (..)
  , ResourcePatch (..)
  , SolutionPatch (..)
  , TaskPatch (..)
  )
import Competences.Command qualified as Cmd
import Competences.Document (Competence (..), CompetenceGrid (..), Document (..), Lesson (..), Resource (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId, AssignmentName (..))
import Competences.Document (CompetenceGridId)
import Competences.Document.Competence (CompetenceLevelId, Level, LevelInfo (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Lesson (LessonItem (..), LessonItemContent (..), LessonPhase (..))
import Competences.Document.Resource (ResourceId, ResourceIdentifier (..))
import Competences.Document.Solution (Solution (..))
import Competences.Document.Task (Task (..), TaskId, taskDisplayName)
import Competences.Document.User (User (..))
import Competences.Exchange.Match
  ( AssignmentImportPreview (..)
  , AssignmentPreview (..)
  , CompetenceMatch (..)
  , ExchangePreview (..)
  , GridPreview (..)
  , ImportAction (..)
  , LessonPreview (..)
  , ResourceImportPreview (..)
  , TaskImportPreview (..)
  , TaskPreview (..)
  , matchExchangeDoc
  , previewHasBlockingConflicts
  , previewHasChanges
  , previewHasWarnings
  )
import Competences.Exchange.Types
  ( ExchangeAssignment (..)
  , ExchangeCompetenceGrid (..)
  , ExchangeDoc
  , ExchangeLesson (..)
  , ExchangeLessonItem (..)
  , ExchangeLessonItemKind (..)
  , ExchangeLessonPhase (..)
  , ExchangeTask (..)
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Draft (retargetForDraft)
import Competences.Frontend.Exchange (decodeExchangeYaml)
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager
  ( ModalConfig (..)
  , ModalHeight (..)
  , ModalId (..)
  , ModalWidth (..)
  , WindowChrome (..)
  , WindowMode
  , closeWindow
  , openFramedModalWith
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.User qualified as QUser
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~))

-- ============================================================================
-- Public
-- ============================================================================

openImportModal :: SyncContext -> IO ()
openImportModal r =
  let cfg =
        ModalConfig
          (WindowChrome (C.translate' C.LblImportAssignments) Icon.IcnImport Nothing)
          (ModalId "import")
          ModalWide
          ModalFull
          Nothing
   in openFramedModalWith r.windowManager cfg (importModalComponent r)

-- ============================================================================
-- Model
-- ============================================================================

data DecodeState
  = Idle
  | Decoding
  | DecodeFailed !Text
  | Decoded !ExchangeDoc !ExchangePreview
  deriving (Eq, Show, Generic)

data Model = Model
  { inputText :: !Text
  , decodeState :: !DecodeState
  , document :: !Document
  , warningAcknowledged :: !Bool
  }
  deriving (Eq, Show, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = DocumentUpdated !DocumentChange
  | SetInputText !Text
  | RequestDecode
  | DecodeReceived !(Either Text ExchangeDoc)
  | AcknowledgeWarnings
  | ApplyImport
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

importModalComponent :: SyncContext -> WindowMode -> M.Component p Model Action
importModalComponent r wm =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel =
      Model
        { inputText = ""
        , decodeState = Idle
        , document = emptyDocument
        , warningAcknowledged = False
        }

    update (DocumentUpdated dc) =
      M.modify $ \m ->
        let doc = dc.document
            newState = case m.decodeState of
              Decoded xdoc _ -> Decoded xdoc (matchExchangeDoc doc xdoc)
              s -> s
         in m{document = doc, decodeState = newState}

    update (SetInputText t) =
      M.modify $ \m -> m{inputText = t, decodeState = Idle, warningAcknowledged = False}

    update RequestDecode = do
      m <- M.get
      M.modify $ \mm -> mm{decodeState = Decoding, warningAcknowledged = False}
      let yaml = m.inputText
      M.withSink $ \sink ->
        decodeExchangeYaml yaml $ \result ->
          sink (DecodeReceived result)

    update (DecodeReceived (Left err)) =
      M.modify $ \m -> m{decodeState = DecodeFailed err}

    update (DecodeReceived (Right xdoc)) =
      M.modify $ \m -> m{decodeState = Decoded xdoc (matchExchangeDoc m.document xdoc)}

    update AcknowledgeWarnings =
      M.modify $ \m -> m{warningAcknowledged = True}

    update ApplyImport = do
      m <- M.get
      case m.decodeState of
        Decoded _ preview ->
          M.io_ $ applyExchangePreview r wm m.document preview
        _ -> pure ()

    view m =
      Layout.vFlow Layout.hFull
        [ Layout.scrollContent $
            Layout.padM $
              Layout.hFlow (Layout.gapM <> Layout.hFull)
                [ MH.div_
                    [class_ "min-h-0 flex-1 w-1/2 h-full"]
                    [ Layout.vFlow (Layout.gapS <> Layout.hFull)
                        [ Typography.h3 "YAML"
                        , M.textarea_
                            [ class_ "flex-1 min-h-0 w-full p-3 font-mono text-sm border border-input rounded-md bg-background resize-none"
                            , MP.placeholder_ "Hier YAML einfügen…"
                            , MP.value_ (M.ms m.inputText)
                            , M.onInput (SetInputText . M.fromMisoString)
                            ]
                            []
                        ]
                    ]
                , MH.div_
                    [class_ "min-h-0 flex-1 w-1/2 h-full"]
                    [ Layout.vFlow (Layout.gapS <> Layout.hFull)
                        [ Typography.h3 "Vorschau"
                        , M.div_
                            [class_ "flex-1 min-h-0 overflow-y-auto border border-border rounded-md p-3 bg-muted/30"]
                            [previewView m]
                        ]
                    ]
                ]
        , footerView m
        ]

footerView :: Model -> M.View Model Action
footerView m =
  Layout.actionFooter $
    [ Button.primary (Button.button ("Vorschau" :: M.MisoString) RequestDecode)
    ]
      <> applyButtons m

applyButtons :: Model -> [M.View Model Action]
applyButtons m = case m.decodeState of
  Decoded _ preview
    | previewHasBlockingConflicts preview -> []
    | not (previewHasChanges preview) -> []
    | previewHasWarnings preview && not m.warningAcknowledged ->
        [Button.secondary (Button.button ("Warnungen akzeptieren" :: M.MisoString) AcknowledgeWarnings)]
    | otherwise -> [Button.applyButton ApplyImport]
  _ -> []

-- ============================================================================
-- Preview view
-- ============================================================================

previewView :: Model -> M.View Model Action
previewView m = case m.decodeState of
  Idle ->
    M.div_
      [class_ "text-muted-foreground italic"]
      [M.text "YAML einfügen und auf 'Vorschau' klicken."]
  Decoding ->
    M.div_
      [class_ "text-muted-foreground italic"]
      [M.text "YAML wird verarbeitet…"]
  DecodeFailed err ->
    M.div_
      [class_ "text-destructive whitespace-pre-wrap"]
      [M.text $ M.ms $ "Fehler: " <> err]
  Decoded _ preview ->
    renderPreview preview

renderPreview :: ExchangePreview -> M.View Model Action
renderPreview p =
  Layout.vFlow Layout.gapL
    [ conflictBanner p.conflicts
    , warningBanner p.warnings
    , section "Kompetenzraster" (map renderGridRow p.gridPreviews)
    , section "Aufgaben" (map (renderTaskRow False) p.taskPreviews)
    , section "Aufgaben (Entwurf)" (map (renderTaskRow True) p.draftTaskPreviews)
    , section "Schulübungen" (map renderAssignmentRow p.assignmentPreviews)
    , section "Schulübungen (Entwurf)" (map renderAssignmentRow p.draftAssignmentPreviews)
    , section "Ressourcen" (map renderResourceRow p.resourcePreviews)
    , section "Stundenbilder" (map renderLessonRow p.lessonPreviews)
    , if previewHasChanges p
        then M.text ""
        else
          M.div_
            [class_ "text-muted-foreground italic"]
            [M.text "Keine Änderungen."]
    ]

renderGridRow :: GridPreview -> M.View Model Action
renderGridRow gp =
  M.div_
    [class_ "border-l-2 border-muted pl-3 py-1"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ actionBadge gp.gridAction
        , M.text (M.ms gp.exchangeGrid.title)
        ]
    , M.div_
        [class_ "ml-3 mt-1 space-y-0.5"]
        (map renderCompetenceRow gp.competenceActions)
    ]

renderCompetenceRow :: ImportAction Competence -> M.View Model Action
renderCompetenceRow a =
  let label = case a of
        Create c -> c.description
        Update _ c -> c.description
        NoChange c -> c.description
        Delete c -> c.description
   in M.div_
        [class_ "flex items-center gap-2 text-xs"]
        [actionBadge a, M.text (M.ms label)]

section :: Text -> [M.View Model Action] -> M.View Model Action
section _ [] = M.text ""
section label rows =
  M.div_
    [class_ "space-y-2"]
    [ Typography.h4 (M.ms label)
    , M.div_ [class_ "ml-3 space-y-1"] rows
    ]

renderTaskRow :: Bool -> TaskPreview -> M.View Model Action
renderTaskRow isDraft tp =
  M.div_
    [class_ "border-l-2 border-muted pl-3 py-1"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ actionBadge tp.preview.taskAction
        , M.text (M.ms (taskLabel tp.preview.taskAction))
        , draftBadge isDraft
        ]
    , competenceLine "Primär" tp.preview.competenceMatches
    , competenceLine "Sekundär" tp.preview.secondaryCompetenceMatches
    , solutionsLine tp.preview.solutionActions
    ]

renderAssignmentRow :: AssignmentPreview -> M.View Model Action
renderAssignmentRow ap =
  let p = ap.preview
   in M.div_
        [class_ "border-l-2 border-muted pl-3 py-1"]
        [ M.div_
            [class_ "flex items-center gap-2"]
            [ actionBadge p.assignmentAction
            , M.text (assignmentLabel p)
            , draftBadge p.isDraft
            ]
        ]

renderResourceRow :: ResourceImportPreview -> M.View Model Action
renderResourceRow rp =
  M.div_
    [class_ "border-l-2 border-muted pl-3 py-1"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ actionBadge rp.resourceAction
        , M.text (M.ms (resourceLabel rp.resourceAction))
        ]
    , competenceLine "Kompetenzen" rp.competenceMatches
    ]

renderLessonRow :: LessonPreview -> M.View Model Action
renderLessonRow lp =
  M.div_
    [class_ "border-l-2 border-muted pl-3 py-1"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ actionBadge lp.lessonAction
        , M.text (M.ms lp.lesson.title)
        ]
    , competenceLine "Kompetenzen" lp.competenceMatches
    ]

competenceLine :: Text -> [CompetenceMatch] -> M.View Model Action
competenceLine _ [] = M.text ""
competenceLine label matches =
  M.div_
    [class_ "text-xs text-muted-foreground mt-1"]
    [ M.text (M.ms (label <> ": "))
    , M.text $ M.ms $ T.intercalate ", " (map describeMatch matches)
    ]
  where
    describeMatch cm = case cm.matched of
      Just _ -> cm.description <> " ✓"
      Nothing -> cm.description <> " ?"

solutionsLine :: [ImportAction Solution] -> M.View Model Action
solutionsLine [] = M.text ""
solutionsLine actions =
  M.div_
    [class_ "text-xs mt-1"]
    [M.text $ M.ms $ "Lösungen: " <> T.pack (show (length actions))]

assignmentLabel :: AssignmentImportPreview -> M.MisoString
assignmentLabel p =
  M.ms $ assignmentName $ case p.assignmentAction of
    Create a -> a
    Update _ a -> a
    NoChange a -> a
    Delete a -> a

assignmentName :: Assignment -> Text
assignmentName a = let AssignmentName n = a.name in n

taskLabel :: ImportAction Task -> Text
taskLabel = \case
  Create t -> taskDisplayName t
  Update _ t -> taskDisplayName t
  NoChange t -> taskDisplayName t
  Delete t -> taskDisplayName t

resourceLabel :: ImportAction Resource -> Text
resourceLabel a =
  let r = case a of
        Create r' -> r'
        Update _ r' -> r'
        NoChange r' -> r'
        Delete r' -> r'
      ResourceIdentifier ident = r.identifier
   in ident

draftBadge :: Bool -> M.View Model Action
draftBadge False = M.text ""
draftBadge True = Badge.outline (Badge.badgeText "Entwurf")

actionBadge :: ImportAction a -> M.View Model Action
actionBadge (Create _) = Badge.primary (Badge.badgeText "Neu")
actionBadge (Update _ _) = Badge.secondary (Badge.badgeText "Aktualisiert")
actionBadge (NoChange _) = Badge.outline (Badge.badgeText "Unverändert")
actionBadge (Delete _) = Badge.destructive (Badge.badgeText "Gelöscht")

conflictBanner :: [Text] -> M.View Model Action
conflictBanner [] = M.text ""
conflictBanner conflicts =
  M.div_
    [class_ "rounded-md border border-destructive bg-destructive/10 p-3 text-sm text-destructive"]
    [ Typography.h4 "Konflikte"
    , MH.ul_ [class_ "list-disc list-inside mt-1 space-y-1"] (map item conflicts)
    , M.div_
        [class_ "mt-2 text-xs"]
        [M.text "Konflikte zuerst auflösen, dann erneut einfügen."]
    ]
  where
    item c = MH.li_ [] [M.text (M.ms c)]

warningBanner :: [Text] -> M.View Model Action
warningBanner [] = M.text ""
warningBanner warnings =
  M.div_
    [class_ "rounded-md border border-amber-500 bg-amber-50 p-3 text-sm text-amber-800"]
    [ Typography.h4 "Warnungen"
    , MH.ul_ [class_ "list-disc list-inside mt-1 space-y-1"] (map item warnings)
    , M.div_
        [class_ "mt-2 text-xs"]
        [M.text "Vor dem Anwenden bestätigen."]
    ]
  where
    item w = MH.li_ [] [M.text (M.ms w)]

-- ============================================================================
-- Apply
-- ============================================================================

applyExchangePreview :: SyncContext -> WindowMode -> Document -> ExchangePreview -> IO ()
applyExchangePreview r wm doc p = do
  -- Phase 0: grids (foundational; create competences before anything
  -- below tries to reference them).
  mapM_ (applyGridPreview r) p.gridPreviews
  -- Phase 1: tasks (no deps).
  taskMap <- applyTaskList r doc False p.taskPreviews
  draftTaskMap <- applyTaskList r doc True p.draftTaskPreviews
  -- Phase 2: resources (no deps on the import side).
  resourceMap <- applyResourceList r p.resourcePreviews
  -- Phase 3: assignments (depend on tasks). We pass the task map so
  -- each assignment's task list resolves to ids.
  assignmentMap <-
    applyAssignmentList r False taskMap p.assignmentPreviews
  draftAssignmentMap <-
    applyAssignmentList r True draftTaskMap p.draftAssignmentPreviews
  -- Phase 4: lessons (reference everything above).
  let lookA name =
        lookup (normalizeKey name) assignmentMap
          <|> lookup (normalizeKey name) draftAssignmentMap
      lookT ident =
        lookup (normalizeKey ident) taskMap
          <|> lookup (normalizeKey ident) draftTaskMap
      lookR ident = lookup (normalizeKey ident) resourceMap
  mapM_ (applyLessonPreview r lookA lookT lookR) p.lessonPreviews
  closeWindow wm

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> y = y
x <|> _ = x

normalizeKey :: Text -> Text
normalizeKey = T.toLower . T.strip

-- | Apply a grid preview: issue the grid Create/Modify command,
-- then issue per-competence commands. New competences get the
-- resulting grid id wired up. Deletes hit the backend, which rejects
-- in-use competences.
applyGridPreview :: SyncContext -> GridPreview -> IO ()
applyGridPreview r gp = do
  gridId <- applyGridAction r gp.gridAction
  mapM_ (applyCompetenceAction r gridId) gp.competenceActions

applyGridAction :: SyncContext -> ImportAction CompetenceGrid -> IO CompetenceGridId
applyGridAction r = \case
  Create new -> do
    newId <- nextId r
    let withId = new & #id .~ newId
    modifySyncDocument r $ Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create withId
    pure newId
  Update old new -> do
    modifySyncDocument r $ Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Modify old.id Lock
    let patch = buildGridPatch old new
    modifySyncDocument r $ Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Modify old.id (Release patch)
    pure old.id
  NoChange existing -> pure existing.id
  Delete existing -> pure existing.id  -- unreachable for grids

applyCompetenceAction :: SyncContext -> CompetenceGridId -> ImportAction Competence -> IO ()
applyCompetenceAction r gridId = \case
  Create new -> do
    newId <- nextId r
    let withId = new & #id .~ newId & #competenceGridId .~ gridId
    modifySyncDocument r $ Cmd.Competences $ Cmd.OnCompetences $ Cmd.Create withId
  Update old new -> do
    modifySyncDocument r $ Cmd.Competences $ Cmd.OnCompetences $ Cmd.Modify old.id Lock
    let patch = buildCompetencePatch old new
    modifySyncDocument r $ Cmd.Competences $ Cmd.OnCompetences $ Cmd.Modify old.id (Release patch)
  NoChange _ -> pure ()
  Delete c ->
    modifySyncDocument r $ Cmd.Competences $ Cmd.OnCompetences $ Cmd.Delete c.id

buildGridPatch :: CompetenceGrid -> CompetenceGrid -> CompetenceGridPatch
buildGridPatch old new =
  CompetenceGridPatch
    { title = if old.title == new.title then Nothing else Just (old.title, new.title)
    , description = if old.description == new.description then Nothing else Just (old.description, new.description)
    }

buildCompetencePatch :: Competence -> Competence -> CompetencePatch
buildCompetencePatch old new =
  CompetencePatch
    { description = if old.description == new.description then Nothing else Just (old.description, new.description)
    , levels = buildLevelPatches old.levels new.levels
    }

buildLevelPatches :: Map Level LevelInfo -> Map Level LevelInfo -> Map Level LevelInfoPatch
buildLevelPatches old new =
  Map.mapMaybeWithKey
    (\lvl newInfo ->
        let oldInfo = Map.lookup lvl old
            descChange = case oldInfo of
              Just o | o.description == newInfo.description -> Nothing
              Just o -> Just (o.description, newInfo.description)
              Nothing -> Just (mempty, newInfo.description)
            lockedChange = case oldInfo of
              Just o | o.locked == newInfo.locked -> Nothing
              Just o -> Just (o.locked, newInfo.locked)
              Nothing -> Just (False, newInfo.locked)
         in case (descChange, lockedChange) of
              (Nothing, Nothing) -> Nothing
              _ -> Just (LevelInfoPatch{description = descChange, locked = lockedChange}))
    new

-- | Apply each task in the list, returning a map from identifier to
-- resulting 'TaskId' (used by assignment / lesson apply).
applyTaskList :: SyncContext -> Document -> Bool -> [TaskPreview] -> IO [(Text, TaskId)]
applyTaskList r doc isDraft = mapM go
  where
    go tp = do
      tid <- applyTaskAndGetId r doc (cmdWrap isDraft) tp.preview
      pure (normalizeKey tp.exchangeTask.identifier, tid)

applyResourceList :: SyncContext -> [ResourceImportPreview] -> IO [(Text, ResourceId)]
applyResourceList r = mapM go
  where
    go rp = do
      rid <- applyResourcePreviewAndGetId r rp
      let ident = case rp.resourceAction of
            Create new -> let ResourceIdentifier i = new.identifier in i
            Update _ new -> let ResourceIdentifier i = new.identifier in i
            NoChange existing -> let ResourceIdentifier i = existing.identifier in i
            Delete existing -> let ResourceIdentifier i = existing.identifier in i
      pure (normalizeKey ident, rid)

applyAssignmentList
  :: SyncContext
  -> Bool
  -> [(Text, TaskId)]
  -> [AssignmentPreview]
  -> IO [(Text, AssignmentId)]
applyAssignmentList r isDraft taskMap = mapM go
  where
    go ap = do
      let p = ap.preview
          -- Resolve the assignment's task list via the per-assignment
          -- ref list against the freshly-applied task map. References
          -- we can't resolve drop silently — they'd come from a
          -- malformed YAML rather than our own export.
          resolvedTaskIds =
            mapMaybe (\ref -> lookup (normalizeKey ref) taskMap) ap.exchangeAssignment.taskRefs
          name = case p.assignmentAction of
            Create a -> assignmentNameOf a
            Update _ a -> assignmentNameOf a
            NoChange a -> assignmentNameOf a
            Delete a -> assignmentNameOf a
      aid <- applyAssignmentPreviewAndGetId r (cmdWrap isDraft) resolvedTaskIds p
      pure (normalizeKey name, aid)

assignmentNameOf :: Assignment -> Text
assignmentNameOf a = let AssignmentName n = a.name in n

cmdWrap :: Bool -> Command -> Command
cmdWrap True = retargetForDraft
cmdWrap False = id

applyLessonPreview
  :: SyncContext
  -> (Text -> Maybe AssignmentId)
  -> (Text -> Maybe TaskId)
  -> (Text -> Maybe ResourceId)
  -> LessonPreview
  -> IO ()
applyLessonPreview r lookA lookT lookR lp = do
  let l = lp.lesson
      assignmentList = mapMaybe lookA l.assignmentRefs
      resourceList = mapMaybe lookR l.resourceRefs
      phases = mapMaybe (toDomainPhase lookA lookT lookR) l.phases
      suppItems = mapMaybe (toDomainItem lookA lookT lookR) l.supplementalItems
  case lp.lessonAction of
    Create new -> do
      newId <- nextId r
      let newLesson =
            new
              & #id .~ newId
              & #assignments .~ assignmentList
              & #resources .~ resourceList
              & #phases .~ phases
              & #supplementalItems .~ suppItems
      modifySyncDocument r $ Cmd.Lessons $ Cmd.OnLessons $ Cmd.Create newLesson
    Update old _ -> do
      modifySyncDocument r $ Cmd.Lessons $ Cmd.OnLessons $ Cmd.Modify old.id Lock
      let patch = buildLessonPatch old l assignmentList resourceList phases suppItems
      modifySyncDocument r $ Cmd.Lessons $ Cmd.OnLessons $ Cmd.Modify old.id (Release patch)
    NoChange _ -> pure ()
    Delete _ -> pure ()  -- unreachable for lessons

applyAssignmentPreviewAndGetId
  :: SyncContext
  -> (Command -> Command)
  -> [TaskId]
  -- ^ resolved task ids for this assignment, in payload order
  -> AssignmentImportPreview
  -> IO AssignmentId
applyAssignmentPreviewAndGetId r wrapCmd resolvedTaskIds p =
  case p.assignmentAction of
    Create new -> do
      newId <- nextId r
      let newAssignment = new & #id .~ newId & #tasks .~ resolvedTaskIds
      modifySyncDocument r $ wrapCmd (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Create newAssignment)
      pure newId
    Update old new -> do
      modifySyncDocument r $ wrapCmd (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Modify old.id Lock)
      let -- Update path: prefer the resolved id list when it's non-
          -- empty (came from the import), otherwise preserve the
          -- existing assignment's task list.
          tasksForPatch =
            if null resolvedTaskIds then old.tasks else resolvedTaskIds
          patch = buildAssignmentPatch old new tasksForPatch
      modifySyncDocument r $ wrapCmd (Cmd.Assignments $ Cmd.OnAssignments $ Cmd.Modify old.id (Release patch))
      pure old.id
    NoChange a -> pure a.id
    Delete a -> pure a.id  -- unreachable for assignments

applyResourcePreviewAndGetId :: SyncContext -> ResourceImportPreview -> IO ResourceId
applyResourcePreviewAndGetId r rp = do
  let matchedLevels = mapMaybe (.matched) rp.competenceMatches
  case rp.resourceAction of
    Create new -> do
      newId <- nextId r
      let withId = new & #id .~ newId & #competenceLevels .~ matchedLevels
      modifySyncDocument r $ Cmd.Resources $ Cmd.OnResources $ Cmd.Create withId
      pure newId
    Update old new -> do
      modifySyncDocument r $ Cmd.Resources $ Cmd.OnResources $ Cmd.Modify old.id Lock
      let patch = buildResourcePatch old new matchedLevels
      modifySyncDocument r $ Cmd.Resources $ Cmd.OnResources $ Cmd.Modify old.id (Release patch)
      pure old.id
    NoChange existing -> pure existing.id
    Delete existing -> pure existing.id  -- unreachable for resources

applyTaskAndGetId :: SyncContext -> Document -> (Command -> Command) -> TaskImportPreview -> IO TaskId
applyTaskAndGetId r doc cmd tp = do
  let teachers = QUser.teachers doc
      mTeacherId = (.id) <$> listToMaybe teachers
      matchedPrimary = mapMaybe (.matched) tp.competenceMatches
      matchedSecondary = mapMaybe (.matched) tp.secondaryCompetenceMatches
  taskId <- case tp.taskAction of
    Create t -> do
      newId <- nextId r
      let newTask =
            Task
              { id = newId
              , identifier = t.identifier
              , title = t.title
              , content = t.content
              , primary = matchedPrimary
              , secondary = matchedSecondary
              , purpose = t.purpose
              , displayInResources = True
              , attachments = t.attachments
              }
      modifySyncDocument r $ cmd (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Create newTask)
      pure newId
    Update old new -> do
      modifySyncDocument r $ cmd (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Modify old.id Lock)
      let patch = buildTaskPatch old new matchedPrimary matchedSecondary
      modifySyncDocument r $ cmd (Cmd.Tasks $ Cmd.OnTasks $ Cmd.Modify old.id (Release patch))
      pure old.id
    NoChange t -> pure t.id
    Delete t -> pure t.id  -- unreachable for tasks
  mapM_ (applySolutionAction r cmd taskId mTeacherId) tp.solutionActions
  pure taskId

applySolutionAction :: SyncContext -> (Command -> Command) -> TaskId -> Maybe (Id User) -> ImportAction Solution -> IO ()
applySolutionAction r cmd taskId mTeacherId = \case
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
      modifySyncDocument r $ cmd (Cmd.Solutions $ Cmd.OnSolutions $ Cmd.Create newSolution)
    Nothing -> pure ()
  Update old new -> do
    modifySyncDocument r $ cmd (Cmd.Solutions $ Cmd.OnSolutions $ Cmd.Modify old.id Lock)
    let patch = buildSolutionPatch old new
    modifySyncDocument r $ cmd (Cmd.Solutions $ Cmd.OnSolutions $ Cmd.Modify old.id (Release patch))
  NoChange _ -> pure ()
  Delete _ -> pure ()  -- unreachable for solutions

-- ============================================================================
-- Phase / item resolution for lesson apply
-- ============================================================================

toDomainPhase
  :: (Text -> Maybe AssignmentId)
  -> (Text -> Maybe TaskId)
  -> (Text -> Maybe ResourceId)
  -> ExchangeLessonPhase
  -> Maybe LessonPhase
toDomainPhase lookA lookT lookR p = Just LessonPhase
  { title = p.title
  , socialForm = p.socialForm
  , duration = p.duration
  , actionForm = p.actionForm
  , notes = mempty
  , items = mapMaybe (toDomainItem lookA lookT lookR) p.items
  , privateNoteRef = Nothing
  }

toDomainItem
  :: (Text -> Maybe AssignmentId)
  -> (Text -> Maybe TaskId)
  -> (Text -> Maybe ResourceId)
  -> ExchangeLessonItem
  -> Maybe LessonItem
toDomainItem lookA lookT lookR item = do
  content <- case item.kind of
    ItemAssignment -> PhaseAssignment <$> lookA item.ref
    ItemTask -> PhaseTask <$> lookT item.ref
    ItemResource -> PhaseResource <$> lookR item.ref
  pure LessonItem
    { content = content
    , publish = item.publish
    }

-- ============================================================================
-- Patch builders
-- ============================================================================

buildAssignmentPatch :: Assignment -> Assignment -> [TaskId] -> AssignmentPatch
buildAssignmentPatch old new taskIds =
  AssignmentPatch
    { name = if old.name == new.name then Nothing else Just (old.name, new.name)
    , description = if old.description == new.description then Nothing else Just (old.description, new.description)
    , assignmentDate = if old.assignmentDate == new.assignmentDate then Nothing else Just (old.assignmentDate, new.assignmentDate)
    , activityType = if old.activityType == new.activityType then Nothing else Just (old.activityType, new.activityType)
    , studentIds = Nothing
    , tasks = if old.tasks == taskIds then Nothing else Just (old.tasks, taskIds)
    , groupSubmissionAllowed = if old.groupSubmissionAllowed == new.groupSubmissionAllowed then Nothing else Just (old.groupSubmissionAllowed, new.groupSubmissionAllowed)
    }

buildTaskPatch :: Task -> Task -> [CompetenceLevelId] -> [CompetenceLevelId] -> TaskPatch
buildTaskPatch old new matchedPrimary matchedSecondary =
  TaskPatch
    { identifier = if old.identifier == new.identifier then Nothing else Just (old.identifier, new.identifier)
    , title = if old.title == new.title then Nothing else Just (old.title, new.title)
    , content = if old.content == new.content then Nothing else Just (old.content, new.content)
    , primary = if old.primary == matchedPrimary then Nothing else Just (old.primary, matchedPrimary)
    , secondary = if old.secondary == matchedSecondary then Nothing else Just (old.secondary, matchedSecondary)
    , purpose = if old.purpose == new.purpose then Nothing else Just (old.purpose, new.purpose)
    , displayInResources = Nothing
    , attachments = if old.attachments == new.attachments then Nothing else Just (old.attachments, new.attachments)
    }

buildResourcePatch :: Resource -> Resource -> [CompetenceLevelId] -> ResourcePatch
buildResourcePatch old new matchedLevels =
  ResourcePatch
    { identifier = if old.identifier == new.identifier then Nothing else Just (old.identifier, new.identifier)
    , competenceLevels = if old.competenceLevels == matchedLevels then Nothing else Just (old.competenceLevels, matchedLevels)
    , content = if old.content == new.content then Nothing else Just (old.content, new.content)
    , attachments = if old.attachments == new.attachments then Nothing else Just (old.attachments, new.attachments)
    }

buildSolutionPatch :: Solution -> Solution -> SolutionPatch
buildSolutionPatch old new =
  SolutionPatch
    { solutionType = if old.solutionType == new.solutionType then Nothing else Just (old.solutionType, new.solutionType)
    , content = if old.content == new.content then Nothing else Just (old.content, new.content)
    }

buildLessonPatch
  :: Lesson
  -> ExchangeLesson
  -> [AssignmentId]
  -> [ResourceId]
  -> [LessonPhase]
  -> [LessonItem]
  -> LessonPatch
buildLessonPatch old new aids rids phases items =
  LessonPatch
    { title = if old.title == new.title then Nothing else Just (old.title, new.title)
    , description = Nothing
    , competenceLevels = Nothing
    , date = if old.date == new.date then Nothing else Just (old.date, new.date)
    , assignments = if old.assignments == aids then Nothing else Just (old.assignments, aids)
    , resources = if old.resources == rids then Nothing else Just (old.resources, rids)
    , phases = if old.phases == phases then Nothing else Just (old.phases, phases)
    , notes = Nothing
    , supplementalItems = if old.supplementalItems == items then Nothing else Just (old.supplementalItems, items)
    , notesTitleOverride = if old.notesTitleOverride == new.notesTitleOverride then Nothing else Just (old.notesTitleOverride, new.notesTitleOverride)
    , privateNoteRef = Nothing
    }

emptyDocument :: Document
emptyDocument =
  Document
    { competenceGrids = mempty
    , competences = mempty
    , users = mempty
    , evidences = mempty
    , locks = mempty
    , tasks = mempty
    , solutions = mempty
    , resources = mempty
    , assignments = mempty
    , competenceAssessments = mempty
    , competenceGridGrades = mempty
    , mesoPlans = mempty
    , lessons = mempty
    , lessonNotes = mempty
    , participationRecords = mempty
    , absences = mempty
    , submissions = mempty
    , draftTasks = mempty
    , draftAssignments = mempty
    , competenceLevelExamples = mempty
    , layouts = mempty
    , teachingNotes = mempty
    , lessonNotesMigrated = False
    }
