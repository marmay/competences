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

import Competences.Command (AssignmentPatch (..), AssignmentsCommand (..), Command (..), EntityCommand (..), LessonPatch (..), LessonsCommand (..), ModifyCommand (..))
import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Assignment (Assignment (..), AssignmentId)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Lesson (ActionForm (..), Lesson (..), LessonPhase (..), TeachingSocialForm (..))
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.CompetenceLevelSelector (competenceLevelSelectorComponent)
import Competences.Frontend.Component.Selector.MultiSelectAssignmentSelector (multiSelectAssignmentSelectorComponent)
import Competences.Frontend.Component.Selector.MultiSelectResourceSelector (multiSelectResourceSelectorComponent)
import Competences.Frontend.Component.Selector.MultiStageSelector (MultiStageSelectorStyle (..))
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.TaskContent.RichContent (toRawText, fromTrustedInput)
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument)
import Competences.Frontend.SyncContext.WindowManager (WindowManagerRef, closeModal)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Component (componentA)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Modal qualified as Modal
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~), (?~))
import Text.Read (readMaybe)

-- ============================================================================
-- Model
-- ============================================================================

-- | Internal model for the lesson editor modal
data Model = Model
  { lesson :: !Lesson
  , -- Editable fields:
    titleValue :: !Text
  , descriptionValue :: !Text
  , competenceLevels :: ![CompetenceLevelId]
  , dateValue :: !(Maybe Day)
  , initialAssignments :: ![AssignmentId]
    -- ^ Assignment IDs linked to this lesson at time of opening the editor
  , selectedAssignments :: ![AssignmentId]
  , selectedResources :: ![ResourceId]
  , phases :: ![LessonPhase]
  , notesValue :: !Text
  , -- UI state:
    editingPhaseIndex :: !(Maybe Int)
  }
  deriving (Eq, Generic)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = SetTitle !Text
  | SetDescription !Text
  | SetDate !(Maybe Day)
  | SetNotes !Text
  | -- Phase actions:
    AddPhase
  | DeletePhase !Int
  | TogglePhaseEdit !Int
  | SetPhaseTitle !Int !Text
  | SetPhaseDuration !Int !Int
  | SetPhaseSocialForm !Int !TeachingSocialForm
  | SetPhaseActionForm !Int !ActionForm
  | SetPhaseNotes !Int !Text
  | -- Save/Cancel:
    SaveAndClose
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create the lesson editor modal component.
-- The @assignmentIds@ parameter provides the assignment IDs currently linked
-- to this lesson (queried from the document via Assignment.lessonId index).
lessonEditorModal :: SyncContext -> WindowManagerRef -> Lesson -> [AssignmentId] -> M.Component p Model Action
lessonEditorModal r modalMgr lesson' assignmentIds =
  M.component model update (view r)
  where
    model =
      Model
        { lesson = lesson'
        , titleValue = lesson'.title
        , descriptionValue = toRawText lesson'.description
        , competenceLevels = lesson'.competenceLevels
        , dateValue = lesson'.date
        , initialAssignments = assignmentIds
        , selectedAssignments = assignmentIds
        , selectedResources = lesson'.resources
        , phases = lesson'.phases
        , notesValue = toRawText lesson'.notes
        , editingPhaseIndex = Nothing
        }

    update (SetTitle t) =
      M.modify $ \m -> m {titleValue = t}

    update (SetDescription d) =
      M.modify $ \m -> m {descriptionValue = d}

    update (SetDate d) =
      M.modify $ \m -> m {dateValue = d}

    update (SetNotes n) =
      M.modify $ \m -> m {notesValue = n}

    -- Phase actions
    update AddPhase =
      M.modify $ \m ->
        let newPhase =
              LessonPhase
                { title = ""
                , socialForm = WholeClass
                , duration = 10
                , actionForm = Presenting
                , notes = ""
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
         in m & #phases .~ newPhases & #editingPhaseIndex .~ newExpanded

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

    update (SetPhaseNotes idx n) =
      M.modify $ \m -> m & #phases .~ updateAt idx (\p -> p & #notes .~ n) m.phases

    update SaveAndClose = do
      m <- M.get
      M.io_ $ do
        let old = m.lesson
            -- Build lesson patch with only changed fields
            newDescription = fromTrustedInput m.descriptionValue
            newNotes = fromTrustedInput m.notesValue
            patch =
              def
                & (if old.title /= m.titleValue then #title ?~ (old.title, m.titleValue) else id)
                & (if old.description /= newDescription then #description ?~ (old.description, newDescription) else id)
                & (if old.competenceLevels /= m.competenceLevels then #competenceLevels ?~ (old.competenceLevels, m.competenceLevels) else id)
                & (if old.date /= m.dateValue then #date ?~ (old.date, m.dateValue) else id)
                & (if old.resources /= m.selectedResources then #resources ?~ (old.resources, m.selectedResources) else id)
                & (if old.phases /= m.phases then #phases ?~ (old.phases, m.phases) else id)
                & (if old.notes /= newNotes then #notes ?~ (old.notes, newNotes) else id)
            hasLessonChanges =
              old.title /= m.titleValue
                || old.description /= newDescription
                || old.competenceLevels /= m.competenceLevels
                || old.date /= m.dateValue
                || old.resources /= m.selectedResources
                || old.phases /= m.phases
                || old.notes /= newNotes

            -- Compute assignment diff (assignments are now linked via Assignment.lessonId)
            assignmentsAdded = filter (`notElem` m.initialAssignments) m.selectedAssignments
            assignmentsRemoved = filter (`notElem` m.selectedAssignments) m.initialAssignments

        -- Save lesson field changes
        if hasLessonChanges
          then do
            modifySyncDocument r (Lessons $ OnLessons $ Modify m.lesson.id Lock)
            modifySyncDocument r (Lessons $ OnLessons $ Modify m.lesson.id (Release patch))
          else pure ()

        -- Link newly added assignments to this lesson
        let linkAssignment aId oldLessonId newLessonId = do
              modifySyncDocument r (Assignments $ OnAssignments $ Modify aId Lock)
              modifySyncDocument r (Assignments $ OnAssignments $ Modify aId (Release (def & #lessonId ?~ (oldLessonId, newLessonId))))
        mapM_ (\aId -> linkAssignment aId Nothing (Just m.lesson.id)) assignmentsAdded
        -- Unlink removed assignments from this lesson
        mapM_ (\aId -> linkAssignment aId (Just m.lesson.id) Nothing) assignmentsRemoved

        closeModal modalMgr

    update CloseModal =
      M.io_ $ closeModal modalMgr

    -- ========================================================================
    -- View
    -- ========================================================================

    view :: SyncContext -> Model -> M.View Model Action
    view syncCtx m =
      MH.div_
        [ class_ "bg-popover text-popover-foreground rounded-xl shadow-lg"
        , class_ "w-[900px] max-w-[95vw] max-h-[90vh] flex flex-col"
        ]
        [ Modal.modalHeader (C.translate' C.LblLesson) CloseModal
        , -- Scrollable form content
          MH.div_
            [class_ "px-6 py-4 space-y-6 overflow-y-auto"]
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
            , -- Section 6: Resources
              resourcesSection syncCtx m
            , -- Section 7: Notes (split-panel)
              notesSection m
            , -- Section 8: Phases
              phasesSection m
            ]
        , Modal.modalFooter
            [ Button.cancelButton CloseModal
            , Button.primary (Button.button C.LblSave SaveAndClose)
            ]
        ]

    -- ========================================================================
    -- Sections
    -- ========================================================================

    titleSection m =
      MH.div_
        []
        [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblLessonTitle]
        , MH.input_
            [ MP.type_ "text"
            , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
            , MP.value_ (M.ms m.titleValue)
            , MH.onInput (SetTitle . M.fromMisoString)
            , MP.autofocus_ True
            ]
        ]

    descriptionSection m =
      MH.div_
        []
        [ MH.label_ [class_ "text-sm font-medium mb-2 block"] [M.text $ C.translate' C.LblLessonDescription]
        , MH.div_
            [class_ "flex gap-4"]
            [ MH.div_
                [class_ "flex-1"]
                [ MH.label_
                    [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                    [M.text "Markup"]
                , MH.textarea_
                    [ class_ "w-full px-3 py-2 border border-input rounded-md bg-background min-h-[150px] font-mono text-sm"
                    , MP.value_ (M.ms m.descriptionValue)
                    , MH.onInput (SetDescription . M.fromMisoString)
                    ]
                    []
                ]
            , MH.div_
                [class_ "flex-1"]
                [ MH.label_
                    [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                    [M.text $ C.translate' C.LblPreview]
                , MH.div_
                    [class_ "min-h-[150px] p-3 border border-input rounded-md bg-muted/50"]
                    [renderRichText (fromTrustedInput m.descriptionValue)]
                ]
            ]
        ]

    competenceLevelSection syncCtx m =
      MH.div_
        []
        [ MH.label_ [class_ "text-sm font-medium mb-2 block"] [M.text $ C.translate' C.LblLessonCompetences]
        , componentA
            "lesson-editor-competence-selector"
            []
            ( competenceLevelSelectorComponent
                syncCtx
                (\_ -> m.competenceLevels)
                MultiStageSelectorEnabled
                0
                (selectorLens #competenceLevels)
            )
        ]

    dateSection m =
      MH.div_
        []
        [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblLessonDate]
        , MH.input_
            [ MP.type_ "date"
            , class_ "mt-1 w-64 px-3 py-2 border border-input rounded-md bg-background"
            , MP.value_ (M.ms $ maybe "" (formatTime defaultTimeLocale "%Y-%m-%d") m.dateValue)
            , MH.onInput (SetDate . parseDate . M.fromMisoString)
            ]
        ]

    assignmentsSection syncCtx m =
      MH.div_
        []
        [ MH.label_ [class_ "text-sm font-medium mb-2 block"] [M.text $ C.translate' C.LblLessonAssignments]
        , componentA
            "lesson-editor-assignment-selector"
            []
            ( multiSelectAssignmentSelectorComponent
                syncCtx
                (\a -> (a.activityType == SchoolExercise || a.activityType == Exam)
                    && (a.lessonId == Nothing || a.lessonId == Just m.lesson.id))
                m.selectedAssignments
                (selectorLens #selectedAssignments)
            )
        ]

    resourcesSection syncCtx m =
      MH.div_
        []
        [ MH.label_ [class_ "text-sm font-medium mb-2 block"] [M.text $ C.translate' C.LblLessonResources]
        , componentA
            "lesson-editor-resource-selector"
            []
            ( multiSelectResourceSelectorComponent
                syncCtx
                m.selectedResources
                (selectorLens #selectedResources)
            )
        ]

    notesSection m =
      MH.div_
        []
        [ MH.label_ [class_ "text-sm font-medium mb-2 block"] [M.text $ C.translate' C.LblLessonNotes]
        , MH.div_
            [class_ "flex gap-4"]
            [ MH.div_
                [class_ "flex-1"]
                [ MH.label_
                    [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                    [M.text "Markup"]
                , MH.textarea_
                    [ class_ "w-full px-3 py-2 border border-input rounded-md bg-background min-h-[120px] font-mono text-sm"
                    , MP.value_ (M.ms m.notesValue)
                    , MH.onInput (SetNotes . M.fromMisoString)
                    ]
                    []
                ]
            , MH.div_
                [class_ "flex-1"]
                [ MH.label_
                    [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                    [M.text $ C.translate' C.LblPreview]
                , MH.div_
                    [class_ "min-h-[120px] p-3 border border-input rounded-md bg-muted/50"]
                    [renderRichText (fromTrustedInput m.notesValue)]
                ]
            ]
        ]

    phasesSection m =
      MH.div_
        [class_ "border-t border-border pt-4"]
        [ MH.div_
            [class_ "flex items-center justify-between mb-2"]
            [ Typography.h4 (C.translate' C.LblLessonPhases)
            , Button.secondarySm (Button.button (Icon.IcnAdd, C.LblAddPhase) AddPhase)
            ]
        , if null m.phases
            then
              MH.div_
                [class_ "text-center text-muted-foreground py-4"]
                [M.text $ C.translate' C.LblNoPhases]
            else
              MH.div_
                [class_ "space-y-2"]
                (zipWith (viewPhaseCard m) [0 ..] m.phases)
        ]

    -- ========================================================================
    -- Phase card (collapsible inline editor)
    -- ========================================================================

    viewPhaseCard :: Model -> Int -> LessonPhase -> M.View Model Action
    viewPhaseCard m idx phase =
      let isExpanded = m.editingPhaseIndex == Just idx
       in Disclosure.collapsibleWithActions isExpanded (TogglePhaseEdit idx)
            -- Title
            ( MH.div_
                []
                [ MH.span_
                    [class_ "font-medium"]
                    [M.text $ M.ms $ if Text.null phase.title then "(Phase " <> Text.pack (show (idx + 1)) <> ")" else phase.title]
                , MH.span_
                    [class_ "text-sm text-muted-foreground ml-3"]
                    [ M.text $ M.ms (show phase.duration) <> " min"
                    , M.text " · "
                    , M.text $ C.translate' (C.LblTeachingSocialForm phase.socialForm)
                    , M.text " · "
                    , M.text $ C.translate' (C.LblActionForm phase.actionForm)
                    ]
                ]
            )
            -- Actions
            [ Button.ghostSm (Button.button Icon.IcnDelete (DeletePhase idx)) ]
            -- Content
            (viewPhaseEditor idx phase)

    viewPhaseEditor :: Int -> LessonPhase -> M.View Model Action
    viewPhaseEditor idx phase =
      MH.div_
        [class_ "p-4 border-t border-border space-y-3 bg-muted/30"]
        [ -- Title
          MH.div_
            []
            [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseTitle]
            , MH.input_
                [ MP.type_ "text"
                , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background text-sm"
                , MP.value_ (M.ms phase.title)
                , MH.onInput (SetPhaseTitle idx . M.fromMisoString)
                ]
            ]
        , -- Duration + Social form + Action form (inline row)
          MH.div_
            [class_ "flex gap-4"]
            [ MH.div_
                [class_ "w-24"]
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseDuration]
                , MH.input_
                    [ MP.type_ "number"
                    , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background text-sm"
                    , MP.value_ (M.ms $ show phase.duration)
                    , MH.onInput (SetPhaseDuration idx . maybe phase.duration id . readMaybe . M.fromMisoString)
                    ]
                ]
            , MH.div_
                [class_ "flex-1"]
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseSocialForm]
                , MH.select_
                    [ class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background text-sm"
                    , MH.onChange (SetPhaseSocialForm idx . toEnum . maybe 0 id . readMaybe . M.fromMisoString)
                    ]
                    (map (socialFormOption phase.socialForm) [minBound .. maxBound])
                ]
            , MH.div_
                [class_ "flex-1"]
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseActionForm]
                , MH.select_
                    [ class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background text-sm"
                    , MH.onChange (SetPhaseActionForm idx . toEnum . maybe 0 id . readMaybe . M.fromMisoString)
                    ]
                    (map (actionFormOption phase.actionForm) [minBound .. maxBound])
                ]
            ]
        , -- Phase notes
          MH.div_
            []
            [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseNotes]
            , MH.textarea_
                [ class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background min-h-[60px] font-mono text-sm"
                , MP.value_ (M.ms phase.notes)
                , MH.onInput (SetPhaseNotes idx . M.fromMisoString)
                ]
                []
            ]
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
