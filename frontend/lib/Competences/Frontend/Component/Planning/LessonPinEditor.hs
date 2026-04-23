-- |
-- Module      : Competences.Frontend.Component.Planning.LessonPinEditor
-- Description : Lesson editor mounted as a pinned dialog.
--
-- Mounted by 'LockWatching.ensureLessonPin' once the user holds the
-- 'LessonLock'. Lock acquisition / release flows through the standard
-- 'EntityMenu' + 'LockWatching' machinery, so this editor never issues
-- its own 'Lock' command and only fires 'Release patch' on Save; the
-- 'WindowHost' onPinClosed callback handles 'Release def' on cancel.
module Competences.Frontend.Component.Planning.LessonPinEditor
  ( lessonPinEditor
  , Model
  )
where

import Competences.Command (Command (..), EntityCommand (..), LessonPatch (..), LessonsCommand (..), ModifyCommand (..), TeachingNotesCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document (Document (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId, AssignmentName (..))
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Lesson
  ( ActionForm (..)
  , Lesson (..)
  , LessonItem (..)
  , LessonItemContent (..)
  , LessonPhase (..)
  , TeachingSocialForm (..)
  )
import Competences.Document.Resource (Resource (..), ResourceIdentifier (..))
import Competences.Document.Task (Task (..), taskDisplayName)
import Competences.Document.TeachingNote (TeachingNoteId)
import Competences.Frontend.SyncContext (nextId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.ListReorder (ListReorderAction (..), ListReorderState (..), initialListReorderState, listReorderButtons, ListReorderButtons (..), moveElement)
import Competences.Frontend.Component.Selector.Common (selectorLens, selectorTransformedLens)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelSelectorComponent)
import Competences.Frontend.Component.Selector.SearchSelect (MetaFilter (..), SearchSelectConfig (..), SelectionOrder (..), TagLayout (..), keywordsFilter, searchSelectComponent)
import Competences.Frontend.Component.Selector.MultiStageSelector (MultiStageSelectorStyle (..))
import Competences.Frontend.Component.MarkdownEditor (ContentState (..), contentValue, isContentValid, richContentEditorComponent)
import Competences.TaskContent.RichContent (RichContent)
import Competences.Frontend.SyncContext (SyncContext (..), modifySyncDocument)
import Competences.Frontend.SyncContext.WindowManager (PinId, WindowMode, closeWindow, inlineComponent, justLens, pinSaveStateLens)
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortOn)
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core (Lens', lens, (%~), (&), (.~), (?~))
import Optics.Core qualified as O
import Text.Read (readMaybe)

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { lesson :: !Lesson
  , titleValue :: !Text
  , description :: !(ContentState RichContent)
  , competenceLevels :: ![CompetenceLevelId]
  , dateValue :: !(Maybe Day)
  , initialAssignments :: ![AssignmentId]
  , selectedAssignments :: ![AssignmentId]
  , phases :: ![LessonPhase]
  , notes :: !(ContentState RichContent)
  , phaseNoteStates :: !(Map.Map Int (ContentState RichContent))
  , editingPhaseIndex :: !(Maybe Int)
  , phaseReorderState :: !ListReorderState
  , supplementalItems :: ![LessonItem]
  , titleOverrideValue :: !(Maybe Text)
  }
  deriving (Eq, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

-- | Where an items-list mutation is applied.
data ItemsScope = InPhase !Int | InSupplemental
  deriving (Eq, Show)

-- | Operations on a LessonItem list (phase or supplemental).
data ItemOp
  = ItemAdd !LessonItemContent
  | ItemRemove !Int
  | ItemTogglePublish !Int
  deriving (Eq, Show)

data Action
  = SetTitle !Text
  | SetTitleOverride !(Maybe Text)
  | SetDate !(Maybe Day)
  | AddPhase
  | DeletePhase !Int
  | TogglePhaseEdit !Int
  | SetPhaseTitle !Int !Text
  | SetPhaseDuration !Int !Int
  | SetPhaseSocialForm !Int !TeachingSocialForm
  | SetPhaseActionForm !Int !ActionForm
  | PhaseReorder !ListReorderAction
  | PhaseItemsOp !ItemsScope !ItemOp
  | SaveAndClose
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Lesson pin editor factory. Caller resolves the 'Lesson' and the
-- linked 'LessonNotesId' list from the current document and passes them
-- in; this component then owns its edit-time state.
--
-- Edit-time state is persisted across minimize/restore through the
-- standard @pinSaveStates@ binding — identical pattern to the
-- Editor-framework pin editors (Task/Resource/Assignment/...). The
-- 'LessonLock' is held by this session for the pin's whole lifetime, so
-- the restored snapshot cannot diverge from server state.
lessonPinEditor
  :: SyncContext
  -> Lesson
  -> RichContent
  -- ^ Initial lesson-level teacher prose, resolved from
  -- 'Lesson.privateNoteRef' (or 'mempty').
  -> Map.Map Int RichContent
  -- ^ Initial per-phase teacher prose by index, resolved from
  -- 'LessonPhase.privateNoteRef'.
  -> PinId
  -> WindowMode
  -> Maybe Model
  -> M.Component WM.Model Model Action
lessonPinEditor r lesson' lessonNoteContent phaseNoteContents pid wm mSaved =
  (M.component model update (view r))
    { M.bindings =
        [ O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL justLens
        ]
    }
  where
    model = fromMaybe emptyModel mSaved
    emptyModel =
      Model
        { lesson = lesson'
        , titleValue = lesson'.title
        , description = Valid lesson'.description
        , competenceLevels = lesson'.competenceLevels
        , dateValue = lesson'.date
        , initialAssignments = lesson'.assignments
        , selectedAssignments = lesson'.assignments
        , phases = lesson'.phases
        , notes = Valid lessonNoteContent
        , phaseNoteStates = Map.map Valid phaseNoteContents
        , editingPhaseIndex = Nothing
        , phaseReorderState = initialListReorderState
        , supplementalItems = lesson'.supplementalItems
        , titleOverrideValue = lesson'.notesTitleOverride
        }

    update (SetTitle t) =
      M.modify $ \m -> m {titleValue = t}

    update (SetTitleOverride mT) =
      M.modify $ \m -> m {titleOverrideValue = mT}

    update (SetDate d) =
      M.modify $ \m -> m {dateValue = d}

    update AddPhase =
      M.modify $ \m ->
        let newPhase =
              LessonPhase
                { title = ""
                , socialForm = WholeClass
                , duration = 10
                , actionForm = Presenting
                , notes = mempty
                , items = []
                , privateNoteRef = Nothing
                }
            newPhases = m.phases <> [newPhase]
            newIdx = length m.phases
         in m & #phases .~ newPhases & #editingPhaseIndex .~ Just newIdx

    update (DeletePhase idx) =
      M.modify $ \m ->
        let newPhases = deleteAt idx m.phases
            newExpanded = case m.editingPhaseIndex of
              Just i
                | i == idx -> Nothing
                | i > idx -> Just (i - 1)
              other -> other
            newNoteStates = Map.fromList
              [ (if k > idx then k - 1 else k, v)
              | (k, v) <- Map.toList m.phaseNoteStates
              , k /= idx
              ]
         in m & #phases .~ newPhases & #editingPhaseIndex .~ newExpanded & #phaseNoteStates .~ newNoteStates

    update (TogglePhaseEdit idx) =
      M.modify $ \m ->
        if m.editingPhaseIndex == Just idx
          then m & #editingPhaseIndex .~ Nothing
          else m & #editingPhaseIndex .~ Just idx

    update (SetPhaseTitle idx t) =
      M.modify $ \m -> m & #phases .~ updateAt idx (\p -> p & #title .~ t) m.phases

    update (SetPhaseDuration idx d) =
      M.modify $ \m -> m & #phases .~ updateAt idx (\p -> p & #duration .~ d) m.phases

    update (SetPhaseSocialForm idx sf) =
      M.modify $ \m -> m & #phases .~ updateAt idx (\p -> p & #socialForm .~ sf) m.phases

    update (SetPhaseActionForm idx af) =
      M.modify $ \m -> m & #phases .~ updateAt idx (\p -> p & #actionForm .~ af) m.phases

    update (PhaseReorder (StartListReorder idx)) =
      M.modify $ \m -> m & #phaseReorderState .~ ListReorderState (Just idx)

    update (PhaseReorder CancelListReorder) =
      M.modify $ \m -> m & #phaseReorderState .~ initialListReorderState

    update (PhaseReorder (ListReorderTo src tgt)) =
      M.modify $ \m ->
        let newPhases = moveElement src tgt m.phases
            newExpanded = case m.editingPhaseIndex of
              Just i
                | i == src -> Just (if tgt > src then tgt - 1 else tgt)
                | i >= min src tgt && i <= max src tgt ->
                    Just (if src < tgt then i - 1 else i + 1)
              other -> other
            remapKey k
              | k == src = if tgt > src then tgt - 1 else tgt
              | k >= min src tgt && k <= max src tgt =
                  if src < tgt then k - 1 else k + 1
              | otherwise = k
            newNoteStates = Map.fromList
              [(remapKey k, v) | (k, v) <- Map.toList m.phaseNoteStates]
         in m & #phases .~ newPhases & #editingPhaseIndex .~ newExpanded & #phaseReorderState .~ initialListReorderState & #phaseNoteStates .~ newNoteStates

    update (PhaseItemsOp scope op) =
      M.modify $ \m -> case scope of
        InPhase idx ->
          let go phase = phase & #items .~ applyItemOp op phase.items
           in m & #phases .~ updateAt idx go m.phases
        InSupplemental ->
          m & #supplementalItems %~ applyItemOp op

    update SaveAndClose = do
      m <- M.get
      M.io_ $ do
        let old = m.lesson
            descriptionValue = contentValue old.description m.description
            notesValue = contentValue mempty m.notes
            phaseProse i = case Map.lookup i m.phaseNoteStates of
              Just (Valid rc) -> rc
              _ -> mempty

        -- 1. Decide TeachingNote operations for the lesson-level prose.
        (lessonRefAfter, lessonNoteOps) <-
          decideNoteOps r old.privateNoteRef notesValue

        -- 2. Decide TeachingNote operations for each phase. Allocate fresh
        --    IDs for newly-needed notes; build the new phase list with
        --    updated privateNoteRef values.
        phaseDecisions <- mapM
          (\(idx, phase) -> do
             (refAfter, ops) <- decideNoteOps r phase.privateNoteRef (phaseProse idx)
             pure (phase & #privateNoteRef .~ refAfter, ops))
          (zip [0 ..] m.phases)
        let resolvedPhases = map fst phaseDecisions
            phaseNoteOps = concatMap snd phaseDecisions

        -- 3. Apply TeachingNote operations BEFORE the Lesson patch so
        --    refs always point at existing data.
        mapM_ (modifySyncDocument r . TeachingNotes) (lessonNoteOps <> phaseNoteOps)

        let patch =
              def
                & (if old.title /= m.titleValue then #title ?~ (old.title, m.titleValue) else id)
                & (if old.description /= descriptionValue then #description ?~ (old.description, descriptionValue) else id)
                & (if old.competenceLevels /= m.competenceLevels then #competenceLevels ?~ (old.competenceLevels, m.competenceLevels) else id)
                & (if old.date /= m.dateValue then #date ?~ (old.date, m.dateValue) else id)
                & (if old.assignments /= m.selectedAssignments then #assignments ?~ (old.assignments, m.selectedAssignments) else id)
                & (if old.phases /= resolvedPhases then #phases ?~ (old.phases, resolvedPhases) else id)
                & (if old.supplementalItems /= m.supplementalItems then #supplementalItems ?~ (old.supplementalItems, m.supplementalItems) else id)
                & (if old.notesTitleOverride /= m.titleOverrideValue then #notesTitleOverride ?~ (old.notesTitleOverride, m.titleOverrideValue) else id)
                & (if old.privateNoteRef /= lessonRefAfter then #privateNoteRef ?~ (old.privateNoteRef, lessonRefAfter) else id)

        -- The lock is already held (acquired via EntityMenu). Saving
        -- releases it with the patch; closing without saving releases
        -- with an empty patch via onPinClosed in LockWatching.
        modifySyncDocument r (Lessons $ OnLessons $ Modify m.lesson.id (Release patch))

        closeWindow wm

    -- ========================================================================
    -- View
    -- ========================================================================

    view :: SyncContext -> Model -> M.View Model Action
    view syncCtx m =
      Layout.vFlow Layout.hFull
        [ Layout.scrollContent $ Layout.padL $ Layout.vFlow Layout.gapM
            [ titleSection m
            , titleOverrideSection m
            , descriptionSection m
            , competenceLevelSection syncCtx m
            , dateSection m
            , assignmentsSection syncCtx m
            , notesSection m
            , phasesSection syncCtx m
            , supplementalSection syncCtx m
            ]
        , Layout.actionFooter
            [ Button.primary (Button.button C.LblSave (allContentReady m, SaveAndClose))
            ]
        ]

    -- ========================================================================
    -- Sections
    -- ========================================================================

    titleSection m =
      Input.fieldWrapper (C.translate' C.LblLessonTitle) $
        Input.renderInput $
          (Input.defaultInput
            & Input.withValue (M.ms m.titleValue)
            & Input.withOnInput (SetTitle . M.fromMisoString))
            { Input.attrs = [MP.autofocus_ True]
            }

    descriptionSection m =
      Input.fieldWrapper (C.translate' C.LblLessonDescription) $
        inlineComponent "lesson-description" (richContentEditorComponent r.formulaCache (contentValue mempty m.description) #description)

    competenceLevelSection syncCtx m =
      Input.fieldWrapper (C.translate' C.LblLessonCompetences) $
        inlineComponent
          "lesson-editor-competence-selector"
          ( competenceLevelSelectorComponent
              syncCtx
              (\_ -> m.competenceLevels)
              MultiStageSelectorEnabled
              0
              (selectorLens #competenceLevels)
          )

    dateSection m =
      Input.fieldWrapper (C.translate' C.LblLessonDate) $
        MH.div_
          [class_ "w-64"]
          [ Input.dateInput
              (M.ms $ maybe "" (formatTime defaultTimeLocale "%Y-%m-%d") m.dateValue)
              (SetDate . parseDate . M.fromMisoString)
          ]

    assignmentsSection syncCtx m =
      Input.fieldWrapper (C.translate' C.LblLessonAssignments) $
        inlineComponent
          "lesson-editor-assignment-selector"
          ( searchSelectComponent
              syncCtx
              "lesson-editor-assignment-selector"
              assignmentSearchConfig
              m.selectedAssignments
              (selectorTransformedLens (.assignment.id) id #selectedAssignments)
          )

    titleOverrideSection m =
      Input.fieldWrapper (C.translate' C.LblTitleOverride) $
        Input.renderInput $
          Input.defaultInput
            & Input.withValue (M.ms $ fromMaybe "" m.titleOverrideValue)
            & Input.withOnInput
                ( \t ->
                    let t' = M.fromMisoString t
                     in SetTitleOverride (if Text.null t' then Nothing else Just t')
                )
            & Input.withPlaceholder (M.ms (autoTitle m))

    autoTitle :: Model -> Text
    autoTitle m = case m.dateValue of
      Just d ->
        M.fromMisoString (C.translate' C.LblLessonRecordFromPrefix) <> " "
          <> Text.pack (formatTime defaultTimeLocale "%d.%m.%Y" d)
      Nothing -> m.titleValue

    supplementalSection syncCtx m =
      MH.div_
        [class_ "border-t border-border pt-4"]
        [ Typography.h4 (C.translate' C.LblSupplemental)
        , itemsEditor syncCtx "lesson-supplemental" InSupplemental m.supplementalItems
        ]

    notesSection m =
      Input.fieldWrapper (C.translate' C.LblTeachingNotes) $
        inlineComponent "lesson-notes" (richContentEditorComponent r.formulaCache (contentValue mempty m.notes) #notes)

    phasesSection syncCtx m =
      MH.div_
        [class_ "border-t border-border pt-4"]
        [ MH.div_
            [class_ "mb-2"]
            [ Layout.hFlow
                (Layout.hFull <> Layout.crossCenter)
                [ Typography.h4 (C.translate' C.LblLessonPhases)
                , Layout.flowSpring
                , Button.secondarySm (Button.button (Icon.IcnAdd, C.LblAddPhase) AddPhase)
                ]
            ]
        , if null m.phases
            then
              MH.div_
                [class_ "text-center text-muted-foreground py-4"]
                [M.text $ C.translate' C.LblNoPhases]
            else
              Layout.vFlow Layout.gapS
                (zipWith (viewPhaseCard syncCtx m) [0 ..] m.phases)
        ]

    viewPhaseCard :: SyncContext -> Model -> Int -> LessonPhase -> M.View Model Action
    viewPhaseCard syncCtx m idx phase =
      let isExpanded = m.editingPhaseIndex == Just idx
          titleView = Disclosure.titleText $ M.ms $ if Text.null phase.title then "(Phase " <> Text.pack (show (idx + 1)) <> ")" else phase.title
          actions = case listReorderButtons m.phaseReorderState idx of
            ShowReorderStart ->
              [ Disclosure.action Icon.IcnReorder (PhaseReorder (StartListReorder idx))
              , Disclosure.destructiveAction Icon.IcnDelete (DeletePhase idx)
              ]
            ShowReorderCancel ->
              [Disclosure.destructiveAction Icon.IcnCancel (PhaseReorder CancelListReorder)]
            ShowReorderTargets fromIdx thisIdx ->
              [ Disclosure.action Icon.IcnArrowUp (PhaseReorder (ListReorderTo fromIdx thisIdx))
              , Disclosure.action Icon.IcnArrowDown (PhaseReorder (ListReorderTo fromIdx (thisIdx + 1)))
              ]
       in Disclosure.disclosure (TogglePhaseEdit idx) $
            Disclosure.contents titleView isExpanded (viewPhaseEditor syncCtx idx phase) actions

    viewPhaseEditor :: SyncContext -> Int -> LessonPhase -> M.View Model Action
    viewPhaseEditor syncCtx idx phase =
      MH.div_
        [class_ "p-4 border-t border-border space-y-3 bg-muted/30"]
        [ Input.fieldWrapper (C.translate' C.LblPhaseTitle) $
            Input.textInput (M.ms phase.title) (SetPhaseTitle idx . M.fromMisoString)
        , Layout.hFlow Layout.gapM
            [ MH.div_
                [class_ "w-24"]
                [ Typography.fieldLabel (C.translate' C.LblPhaseDuration)
                , Input.numberInput
                    (M.ms $ show phase.duration)
                    (SetPhaseDuration idx . maybe phase.duration id . readMaybe . M.fromMisoString)
                ]
            , MH.div_
                [class_ "flex-1"]
                [ Typography.fieldLabel (C.translate' C.LblPhaseSocialForm)
                , MH.select_
                    [ class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background text-sm"
                    , MH.onChange (SetPhaseSocialForm idx . toEnum . maybe 0 id . readMaybe . M.fromMisoString)
                    ]
                    (map (socialFormOption phase.socialForm) [minBound .. maxBound])
                ]
            , MH.div_
                [class_ "flex-1"]
                [ Typography.fieldLabel (C.translate' C.LblPhaseActionForm)
                , MH.select_
                    [ class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background text-sm"
                    , MH.onChange (SetPhaseActionForm idx . toEnum . maybe 0 id . readMaybe . M.fromMisoString)
                    ]
                    (map (actionFormOption phase.actionForm) [minBound .. maxBound])
                ]
            ]
        , Input.fieldWrapper (C.translate' C.LblPhaseNotes) $
            inlineComponent ("phase-notes-" <> M.ms (show idx))
              (richContentEditorComponent r.formulaCache phase.notes (phaseNoteStateLens idx))
        , itemsEditor syncCtx ("lesson-phase-items-" <> M.ms (show idx)) (InPhase idx) phase.items
        ]

-- ============================================================================
-- Assignment search config
-- ============================================================================

data TaggedAssignment = TaggedAssignment
  { assignment :: !Assignment
  , isDraft :: !Bool
  }
  deriving (Eq, Show)

assignmentSearchConfig :: SearchSelectConfig TaggedAssignment AssignmentId
assignmentSearchConfig =
  SearchSelectConfig
    { projectItems = \doc ->
        let published = map (\a -> TaggedAssignment a False) $ filter eligible $ Ix.toList doc.assignments
            drafts = map (\a -> TaggedAssignment a True) $ filter eligible $ Ix.toList doc.draftAssignments
         in sortOn (\ta -> ta.assignment.assignmentDate) (published <> drafts)
    , itemId = (.assignment.id)
    , itemLabel = \ta ->
        let a = ta.assignment
         in unName a.name
              <> " (" <> Text.pack (show $ C.formatDay a.assignmentDate) <> ")"
              <> if ta.isDraft then " (" <> M.fromMisoString (C.translate' C.LblDraft) <> ")" else ""
    , metaFilters =
        [ keywordsFilter ["hü", "hausübung"] (\ta -> ta.assignment.activityType == HomeExercise)
        , keywordsFilter ["sü", "schulübung"] (\ta -> ta.assignment.activityType == SchoolExercise)
        , keywordsFilter ["entwurf", "draft"] (.isDraft)
        , MetaFilter {hint = "@datum", parser = dateFilter}
        ]
    , viewTag = \ta ->
        let label = M.ms (unName ta.assignment.name)
                      <> if ta.isDraft then " (" <> C.translate' C.LblDraft <> ")" else ""
         in (Icon.IcnAssignment, label)
    , placeholder = M.fromMisoString $ C.translate' C.LblSelectAssignments
    , selectionOrder = AutoOrder id
    , tagLayout = TagsInline
    , onCreate = Nothing
    }
  where
    eligible a = a.activityType `elem` [SchoolExercise, HomeExercise, Exam]
    unName (AssignmentName t) = t

    dateFilter :: Text -> Maybe (TaggedAssignment -> Bool)
    dateFilter t
      | Text.any (\c -> c == '.' || (c >= '0' && c <= '9')) t && Text.length t >= 2 =
          Just (\ta -> Text.toLower t `Text.isInfixOf` Text.toLower (Text.pack $ show $ C.formatDay ta.assignment.assignmentDate))
      | otherwise = Nothing

-- ============================================================================
-- TeachingNote orchestration
-- ============================================================================

-- | Decide what 'TeachingNote' commands to emit and which ref the
-- parent (lesson or phase) should carry after save, given the existing
-- ref and the new prose value from the editor.
--
-- Allocates a fresh 'TeachingNoteId' (in IO) only for the create case.
decideNoteOps
  :: SyncContext
  -> Maybe TeachingNoteId
  -> RichContent
  -> IO (Maybe TeachingNoteId, [TeachingNotesCommand])
decideNoteOps r mRef newContent = case (mRef, newContent == mempty) of
  (Nothing, True) ->
    pure (Nothing, [])
  (Just nid, True) ->
    pure (Nothing, [DeleteTeachingNote nid])
  (Just nid, False) ->
    pure (Just nid, [SetTeachingNote nid newContent])
  (Nothing, False) -> do
    nid <- nextId r
    pure (Just nid, [SetTeachingNote nid newContent])

-- ============================================================================
-- Phase items: selectable sum type + search config + editor
-- ============================================================================

-- | Mixed-domain carrier for the phase/supplemental items selector.
data PhaseSelectableItem
  = SelectableResource !Resource
  | SelectableTask !Task
  | SelectableAssignment !Assignment
  deriving (Eq, Ord, Show)

phaseSelectableId :: PhaseSelectableItem -> LessonItemContent
phaseSelectableId = \case
  SelectableResource rsc -> PhaseResource rsc.id
  SelectableTask t -> PhaseTask t.id
  SelectableAssignment a -> PhaseAssignment a.id

phaseItemsSearchConfig :: SearchSelectConfig PhaseSelectableItem LessonItemContent
phaseItemsSearchConfig =
  SearchSelectConfig
    { projectItems = \doc ->
        map SelectableResource (Ix.toList doc.resources)
          <> map SelectableTask (Ix.toList doc.tasks)
          <> map SelectableAssignment (Ix.toList doc.assignments)
    , itemId = phaseSelectableId
    , itemLabel = \case
        SelectableResource rsc -> let ResourceIdentifier rid = rsc.identifier in rid
        SelectableTask t -> taskDisplayName t
        SelectableAssignment a -> let AssignmentName n = a.name in n
    , metaFilters =
        [ keywordsFilter ["material", "ressource"] $ \case SelectableResource _ -> True; _ -> False
        , keywordsFilter ["aufgabe", "task"] $ \case SelectableTask _ -> True; _ -> False
        , keywordsFilter ["auftrag", "assignment"] $ \case SelectableAssignment _ -> True; _ -> False
        ]
    , viewTag = \case
        SelectableResource rsc -> let ResourceIdentifier rid = rsc.identifier in (Icon.IcnResources, M.ms rid)
        SelectableTask t -> (Icon.IcnTask, M.ms (taskDisplayName t))
        SelectableAssignment a -> let AssignmentName n = a.name in (Icon.IcnAssignment, M.ms n)
    , placeholder = "Ressource / Aufgabe / Auftrag auswählen…"
    , selectionOrder = ManualReorder
    , tagLayout = TagsInline
    , onCreate = Nothing
    }

-- | Single SearchSelect picker + a per-row publish-toggle panel for an
-- items list. Mutations go through 'PhaseItemsOp' actions.
itemsEditor :: SyncContext -> M.MisoString -> ItemsScope -> [LessonItem] -> M.View Model Action
itemsEditor syncCtx key scope items =
  MH.div_
    [class_ "space-y-2"]
    [ inlineComponent (key <> "-picker")
        ( searchSelectComponent
            syncCtx
            key
            phaseItemsSearchConfig
            (map (.content) items)
            (selectorTransformedLens phaseSelectableId id (itemsSelectorLens scope))
        )
    , if null items
        then Layout.empty
        else
          MH.div_
            [class_ "flex flex-col gap-1"]
            (zipWith (viewItemRow scope) [0 ..] items)
    ]

-- | Lens adapting between the selector's [LessonItemContent] view and
-- the model's [LessonItem] storage for a given scope. Preserves the
-- per-item @publish@ flag across selection changes.
itemsSelectorLens :: ItemsScope -> Lens' Model [LessonItemContent]
itemsSelectorLens scope = lens getter setter
  where
    currentItems :: Model -> [LessonItem]
    currentItems m = case scope of
      InPhase i -> maybe [] (.items) (listIndex i m.phases)
      InSupplemental -> m.supplementalItems

    getter m = map (.content) (currentItems m)

    setter m newContents =
      let old = currentItems m
          byContent = Map.fromList [(i.content, i) | i <- old]
          newItems =
            [ Map.findWithDefault (LessonItem {content = c, publish = True}) c byContent
            | c <- newContents
            ]
       in case scope of
            InPhase i -> m & #phases .~ updateAt i (\p -> p & #items .~ newItems) m.phases
            InSupplemental -> m & #supplementalItems .~ newItems

viewItemRow :: ItemsScope -> Int -> LessonItem -> M.View Model Action
viewItemRow scope j item =
  let (icn, label) = describeItem item.content
      action = PhaseItemsOp scope (ItemTogglePublish j)
   in MH.div_
        [class_ "flex items-center gap-2 text-sm"]
        [ Icon.icon [class_ "text-muted-foreground shrink-0"] icn
        , MH.span_ [class_ "flex-1 truncate"] [M.text label]
        , Button.toggleSm item.publish
            (Button.button (Icon.IcnView, C.LblVisibleToStudents) action)
        ]

describeItem :: LessonItemContent -> (Icon.Icon, M.MisoString)
describeItem = \case
  PhaseResource _ -> (Icon.IcnResources, "Ressource")
  PhaseTask _ -> (Icon.IcnTask, "Aufgabe")
  PhaseAssignment _ -> (Icon.IcnAssignment, "Auftrag")

-- | Apply a single 'ItemOp' to a list of 'LessonItem'. Out-of-range
-- indices are no-ops.
applyItemOp :: ItemOp -> [LessonItem] -> [LessonItem]
applyItemOp (ItemAdd c) xs = xs <> [LessonItem {content = c, publish = True}]
applyItemOp (ItemRemove i) xs = deleteAt i xs
applyItemOp (ItemTogglePublish i) xs =
  updateAt i (\it -> it {publish = not it.publish}) xs

-- ============================================================================
-- Helpers
-- ============================================================================

socialFormOption :: TeachingSocialForm -> TeachingSocialForm -> M.View Model Action
socialFormOption current sf =
  MH.option_
    [ MP.value_ (M.ms $ show $ fromEnum sf)
    , MP.selected_ (current == sf)
    ]
    [M.text $ C.translate' (C.LblTeachingSocialForm sf)]

actionFormOption :: ActionForm -> ActionForm -> M.View Model Action
actionFormOption current af =
  MH.option_
    [ MP.value_ (M.ms $ show $ fromEnum af)
    , MP.selected_ (current == af)
    ]
    [M.text $ C.translate' (C.LblActionForm af)]

parseDate :: Text -> Maybe Day
parseDate t
  | Text.null t = Nothing
  | otherwise = parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack t)

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs =
  let (before, after) = splitAt idx xs
   in case after of
        [] -> before
        (x : rest) -> before <> [f x] <> rest

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs =
  let (before, after) = splitAt idx xs
   in before <> drop 1 after

listIndex :: Int -> [a] -> Maybe a
listIndex _ [] = Nothing
listIndex 0 (x : _) = Just x
listIndex n (_ : xs)
  | n < 0 = Nothing
  | otherwise = listIndex (n - 1) xs

phaseNoteState :: Model -> Int -> ContentState RichContent
phaseNoteState m idx = Map.findWithDefault
  (Valid $ maybe mempty (.notes) $ listIndex idx m.phases)
  idx m.phaseNoteStates

phaseNoteStateLens :: Int -> Lens' Model (ContentState RichContent)
phaseNoteStateLens idx = lens getter setter
  where
    getter m = phaseNoteState m idx
    setter m cs@(Valid rc) = m
      & #phaseNoteStates .~ Map.insert idx cs m.phaseNoteStates
      & #phases .~ updateAt idx (\p -> p & #notes .~ rc) m.phases
    setter m cs = m & #phaseNoteStates .~ Map.insert idx cs m.phaseNoteStates

allContentReady :: Model -> Bool
allContentReady m =
  isContentValid m.description
    && isContentValid m.notes
    && all isContentValid (Map.elems m.phaseNoteStates)
