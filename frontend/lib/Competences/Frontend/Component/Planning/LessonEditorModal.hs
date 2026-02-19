-- |
-- Module      : Competences.Frontend.Component.Planning.LessonEditorModal
-- Description : Unified modal for editing all Lesson fields
--
-- Replaces EntryEditorModal, NotesEditorModal, PhaseEditorModal, and LessonPlanEditor.
-- A single modal with all lesson fields (title, description, competence levels,
-- date, assignments, resources, phases, notes) and a single Save button.
module Competences.Frontend.Component.Planning.LessonEditorModal
  ( lessonEditorModal
  )
where

import Competences.Command (Command (Lessons), EntityCommand (..), LessonNotesCommand (..), LessonPatch (..), LessonsCommand (..), ModifyCommand (..))
import Competences.Command qualified as Cmd
import Competences.Command.LessonNotes (LessonNotesPatch (..))
import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Lesson (ActionForm (..), Lesson (..), LessonPhase (..), TeachingSocialForm (..))
import Competences.Document.LessonNotes (LessonNotes (..), LessonNotesId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Common.ListReorder (ListReorderAction (..), ListReorderState (..), initialListReorderState, listReorderButtons, ListReorderButtons (..), moveElement)
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelSelectorComponent)
import Competences.Frontend.Component.Selector.MultiSelectAssignmentSelector (multiSelectAssignmentSelectorComponent)
import Competences.Frontend.Component.Selector.MultiSelectLessonNotesSelector (multiSelectLessonNotesSelectorComponent)
import Competences.Frontend.Component.Selector.MultiStageSelector (MultiStageSelectorStyle (..))
import Competences.Frontend.Component.MarkdownEditor (ContentState (..), contentValue, isContentValid, richContentEditorComponent)
import Competences.TaskContent.RichContent (RichContent)
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Component (componentA)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core (Lens', lens, (&), (.~), (?~))
import Text.Read (readMaybe)

-- ============================================================================
-- Model
-- ============================================================================

-- | Internal model for the lesson editor modal
data Model = Model
  { lesson :: !Lesson
  , -- Editable fields:
    titleValue :: !Text
  , description :: !(ContentState RichContent)
  , competenceLevels :: ![CompetenceLevelId]
  , dateValue :: !(Maybe Day)
  , initialAssignments :: ![AssignmentId]
    -- ^ Assignment IDs linked to this lesson at time of opening the editor
  , selectedAssignments :: ![AssignmentId]
  , initialLessonNotes :: ![LessonNotesId]
    -- ^ Lesson note IDs linked to this lesson at time of opening the editor
  , selectedLessonNotes :: ![LessonNotesId]
  , phases :: ![LessonPhase]
  , notes :: !(ContentState RichContent)
  , phaseNoteStates :: !(Map.Map Int (ContentState RichContent))
  , -- UI state:
    editingPhaseIndex :: !(Maybe Int)
  , phaseReorderState :: !ListReorderState
  }
  deriving (Eq, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = SetTitle !Text
  | SetDate !(Maybe Day)
  | -- Phase actions:
    AddPhase
  | DeletePhase !Int
  | TogglePhaseEdit !Int
  | SetPhaseTitle !Int !Text
  | SetPhaseDuration !Int !Int
  | SetPhaseSocialForm !Int !TeachingSocialForm
  | SetPhaseActionForm !Int !ActionForm
  | -- Reorder actions:
    PhaseReorder !ListReorderAction
  | -- Save/Cancel:
    SaveAndClose
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create the lesson editor content component.
-- Pass @Just closeAction@ when used in a context that supports programmatic close.
lessonEditorModal :: SyncContext -> Maybe (IO ()) -> Lesson -> [LessonNotesId] -> M.Component p Model Action
lessonEditorModal r mClose lesson' lessonNotesIds =
  M.component model update (view r)
  where
    model =
      Model
        { lesson = lesson'
        , titleValue = lesson'.title
        , description = Valid lesson'.description
        , competenceLevels = lesson'.competenceLevels
        , dateValue = lesson'.date
        , initialAssignments = lesson'.assignments
        , selectedAssignments = lesson'.assignments
        , initialLessonNotes = lessonNotesIds
        , selectedLessonNotes = lessonNotesIds
        , phases = lesson'.phases
        , notes = Valid lesson'.notes
        , phaseNoteStates = Map.empty
        , editingPhaseIndex = Nothing
        , phaseReorderState = initialListReorderState
        }

    update (SetTitle t) =
      M.modify $ \m -> m {titleValue = t}

    update (SetDate d) =
      M.modify $ \m -> m {dateValue = d}

    -- Phase actions
    update AddPhase =
      M.modify $ \m ->
        let newPhase =
              LessonPhase
                { title = ""
                , socialForm = WholeClass
                , duration = 10
                , actionForm = Presenting
                , notes = mempty
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
            -- Remove deleted index and shift keys > idx down by 1
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

    -- Reorder actions
    update (PhaseReorder (StartListReorder idx)) =
      M.modify $ \m -> m & #phaseReorderState .~ ListReorderState (Just idx)

    update (PhaseReorder CancelListReorder) =
      M.modify $ \m -> m & #phaseReorderState .~ initialListReorderState

    update (PhaseReorder (ListReorderTo src tgt)) =
      M.modify $ \m ->
        let newPhases = moveElement src tgt m.phases
            -- Adjust editingPhaseIndex to follow the moved phase
            newExpanded = case m.editingPhaseIndex of
              Just i
                | i == src -> Just (if tgt > src then tgt - 1 else tgt)
                | i >= min src tgt && i <= max src tgt ->
                    Just (if src < tgt then i - 1 else i + 1)
              other -> other
            -- Remap phaseNoteStates keys to follow the move
            remapKey k
              | k == src = if tgt > src then tgt - 1 else tgt
              | k >= min src tgt && k <= max src tgt =
                  if src < tgt then k - 1 else k + 1
              | otherwise = k
            newNoteStates = Map.fromList
              [(remapKey k, v) | (k, v) <- Map.toList m.phaseNoteStates]
         in m & #phases .~ newPhases & #editingPhaseIndex .~ newExpanded & #phaseReorderState .~ initialListReorderState & #phaseNoteStates .~ newNoteStates

    update SaveAndClose = do
      m <- M.get
      M.io_ $ do
        let old = m.lesson
            descriptionValue = contentValue old.description m.description
            notesValue = contentValue old.notes m.notes
            -- Merge phase note states into phases
            resolvedPhases = zipWith resolvePhaseNotes [0 ..] m.phases
            resolvePhaseNotes i phase = case Map.lookup i m.phaseNoteStates of
              Just (Valid rc) -> phase & #notes .~ rc
              _ -> phase
            -- Build lesson patch with only changed fields
            patch =
              def
                & (if old.title /= m.titleValue then #title ?~ (old.title, m.titleValue) else id)
                & (if old.description /= descriptionValue then #description ?~ (old.description, descriptionValue) else id)
                & (if old.competenceLevels /= m.competenceLevels then #competenceLevels ?~ (old.competenceLevels, m.competenceLevels) else id)
                & (if old.date /= m.dateValue then #date ?~ (old.date, m.dateValue) else id)
                & (if old.assignments /= m.selectedAssignments then #assignments ?~ (old.assignments, m.selectedAssignments) else id)
                & (if old.phases /= resolvedPhases then #phases ?~ (old.phases, resolvedPhases) else id)
                & (if old.notes /= notesValue then #notes ?~ (old.notes, notesValue) else id)
            hasLessonChanges =
              old.title /= m.titleValue
                || old.description /= descriptionValue
                || old.competenceLevels /= m.competenceLevels
                || old.date /= m.dateValue
                || old.assignments /= m.selectedAssignments
                || old.phases /= resolvedPhases
                || old.notes /= notesValue

        -- Save lesson field changes (including assignment list)
        if hasLessonChanges
          then do
            modifySyncDocument r (Lessons $ OnLessons $ Modify m.lesson.id Lock)
            modifySyncDocument r (Lessons $ OnLessons $ Modify m.lesson.id (Release patch))
          else pure ()

        -- Compute lesson notes diff (lesson notes are linked via LessonNotes.lessonId)
        let lessonNotesAdded = filter (`notElem` m.initialLessonNotes) m.selectedLessonNotes
            lessonNotesRemoved = filter (`notElem` m.selectedLessonNotes) m.initialLessonNotes

        -- Link newly added lesson notes to this lesson
        let linkLessonNote lnId oldLessonId newLessonId = do
              modifySyncDocument r (Cmd.LessonNotes $ OnLessonNotes $ Modify lnId Lock)
              modifySyncDocument r (Cmd.LessonNotes $ OnLessonNotes $ Modify lnId (Release (def & #lessonId ?~ (oldLessonId, newLessonId))))
        mapM_ (\lnId -> linkLessonNote lnId Nothing (Just m.lesson.id)) lessonNotesAdded
        -- Unlink removed lesson notes from this lesson
        mapM_ (\lnId -> linkLessonNote lnId (Just m.lesson.id) Nothing) lessonNotesRemoved

        case mClose of
          Just close -> close
          Nothing -> pure ()

    -- ========================================================================
    -- View
    -- ========================================================================

    view :: SyncContext -> Model -> M.View Model Action
    view syncCtx m =
      Layout.vFlow Layout.hFull
        [ -- Scrollable form content
          Layout.scrollContent $ Layout.padL $ Layout.vFlow Layout.gapM
            [ -- Section 1: Title
              titleSection m
            , -- Section 2: Description (split-panel)
              descriptionSection m
            , -- Section 3: Competence levels
              competenceLevelSection syncCtx m
            , -- Section 4: Date
              dateSection m
            , -- Section 5: Assignments
              assignmentsSection syncCtx m
            , -- Section 6: Lesson Notes
              lessonNotesSection syncCtx m
            , -- Section 7: Notes (split-panel)
              notesSection m
            , -- Section 8: Phases
              phasesSection m
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
        componentA "lesson-description" [] (richContentEditorComponent (contentValue mempty m.description) #description)

    competenceLevelSection syncCtx m =
      Input.fieldWrapper (C.translate' C.LblLessonCompetences) $
        componentA
          "lesson-editor-competence-selector"
          []
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
        componentA
          "lesson-editor-assignment-selector"
          []
          ( multiSelectAssignmentSelectorComponent
              syncCtx
              ( \a ->
                  a.activityType == SchoolExercise || a.activityType == Exam
              )
              m.selectedAssignments
              (selectorLens #selectedAssignments)
          )

    lessonNotesSection syncCtx m =
      Input.fieldWrapper (C.translate' C.LblLessonNotesEntries) $
        componentA
          "lesson-editor-lesson-notes-selector"
          []
          ( multiSelectLessonNotesSelectorComponent
              syncCtx
              (\ln -> ln.lessonId == Nothing || ln.lessonId == Just m.lesson.id)
              m.selectedLessonNotes
              (selectorLens #selectedLessonNotes)
          )

    notesSection m =
      Input.fieldWrapper (C.translate' C.LblTeachingNotes) $
        componentA "lesson-notes" [] (richContentEditorComponent (contentValue mempty m.notes) #notes)

    phasesSection m =
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
                (zipWith (viewPhaseCard m) [0 ..] m.phases)
        ]

    -- ========================================================================
    -- Phase card (collapsible inline editor)
    -- ========================================================================

    viewPhaseCard :: Model -> Int -> LessonPhase -> M.View Model Action
    viewPhaseCard m idx phase =
      let isExpanded = m.editingPhaseIndex == Just idx
          titleView = Disclosure.titleText $ M.ms $ if Text.null phase.title then "(Phase " <> Text.pack (show (idx + 1)) <> ")" else phase.title
          actions = case listReorderButtons m.phaseReorderState idx of
            ShowReorderStart ->
              [ Disclosure.Action Icon.IcnReorder (PhaseReorder (StartListReorder idx))
              , Disclosure.DestructiveAction Icon.IcnDelete (DeletePhase idx)
              ]
            ShowReorderCancel ->
              [Disclosure.DestructiveAction Icon.IcnCancel (PhaseReorder CancelListReorder)]
            ShowReorderTargets fromIdx thisIdx ->
              [ Disclosure.Action Icon.IcnArrowUp (PhaseReorder (ListReorderTo fromIdx thisIdx))
              , Disclosure.Action Icon.IcnArrowDown (PhaseReorder (ListReorderTo fromIdx (thisIdx + 1)))
              ]
       in Disclosure.disclosure (TogglePhaseEdit idx) $
            Disclosure.contents titleView isExpanded (viewPhaseEditor idx phase) actions

    viewPhaseEditor :: Int -> LessonPhase -> M.View Model Action
    viewPhaseEditor idx phase =
      MH.div_
        [class_ "p-4 border-t border-border space-y-3 bg-muted/30"]
        [ -- Title
          Input.fieldWrapper (C.translate' C.LblPhaseTitle) $
            Input.textInput (M.ms phase.title) (SetPhaseTitle idx . M.fromMisoString)
        , -- Duration + Social form + Action form (inline row)
          Layout.hFlow Layout.gapM
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
        , -- Phase notes
          Input.fieldWrapper (C.translate' C.LblPhaseNotes) $
            componentA ("phase-notes-" <> M.ms (show idx)) []
              (richContentEditorComponent phase.notes (phaseNoteStateLens idx))
        ]

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

-- | Parse a date string in YYYY-MM-DD format
parseDate :: Text -> Maybe Day
parseDate t
  | Text.null t = Nothing
  | otherwise = parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack t)

-- | Update element at index
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs =
  let (before, after) = splitAt idx xs
   in case after of
        [] -> before
        (x : rest) -> before <> [f x] <> rest

-- | Delete element at index
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs =
  let (before, after) = splitAt idx xs
   in before <> drop 1 after

-- | Safe list indexing
listIndex :: Int -> [a] -> Maybe a
listIndex _ [] = Nothing
listIndex 0 (x : _) = Just x
listIndex n (_ : xs)
  | n < 0 = Nothing
  | otherwise = listIndex (n - 1) xs

-- | Get the 'ContentState' for a specific phase's notes.
phaseNoteState :: Model -> Int -> ContentState RichContent
phaseNoteState m idx = Map.findWithDefault
  (Valid $ maybe mempty (.notes) $ listIndex idx m.phases)
  idx m.phaseNoteStates

-- | Lens into a specific phase's notes as 'ContentState'.
-- On 'Valid', dual-writes into both 'phaseNoteStates' and 'phases'.
phaseNoteStateLens :: Int -> Lens' Model (ContentState RichContent)
phaseNoteStateLens idx = lens getter setter
  where
    getter m = phaseNoteState m idx
    setter m cs@(Valid rc) = m
      & #phaseNoteStates .~ Map.insert idx cs m.phaseNoteStates
      & #phases .~ updateAt idx (\p -> p & #notes .~ rc) m.phases
    setter m cs = m & #phaseNoteStates .~ Map.insert idx cs m.phaseNoteStates

-- | Check if all rich-content fields are ready (not debouncing or invalid).
allContentReady :: Model -> Bool
allContentReady m =
  isContentValid m.description
    && isContentValid m.notes
    && all isContentValid (Map.elems m.phaseNoteStates)
