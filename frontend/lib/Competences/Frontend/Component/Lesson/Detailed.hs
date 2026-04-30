-- | Detailed rendering of a 'Lesson' — a customizable card-based view
-- that drives both the student-facing "Schulübung" and the teacher's
-- lecture / print plan.
--
-- Structure:
--
--   * title;
--   * optional lesson-level 'TeachingNote' block (teacher mode only);
--   * collapsible homework block listing every 'PhaseAssignment' whose
--     assignment is a home exercise;
--   * per-phase sections (phase header + optional phase 'TeachingNote'
--     in teacher mode + per-item cards);
--   * a supplemental section for the 'supplementalItems' list.
--
-- Student mode filters unpublished items out. Teacher mode keeps them,
-- dimmed with a "not visible" badge, and inlines teacher notes.
--
-- Items are deduplicated globally across the view. Phases and
-- supplemental sections with no surviving content are omitted.
module Competences.Frontend.Component.Lesson.Detailed
  ( lessonDetailedComponent
  , LessonDetailedConfig (..)
  , LessonDetailedMode (..)
  , pinStudentLessonView
  , pinTeacherLessonPlan
  , lessonDerivedTitle
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Common.Set (memberLens)
import Competences.Document
  ( Assignment (..)
  , Document (..)
  , User
  )
import Competences.Document.ActivityType (ActivityType (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.Id (idToText)
import Competences.Document.Lesson
  ( ActionForm (..)
  , Lesson (..)
  , LessonId
  , LessonItem (..)
  , LessonItemContent (..)
  , LessonPhase (..)
  , TeachingSocialForm (..)
  )
import Competences.Document.TeachingNote (TeachingNote (..), TeachingNoteId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Assignment.Detailed
  ( RenderStyle (..)
  , bindVisibility
  , viewerComponent
  )
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Resource.Detailed
  ( ResourceDetailedConfig (..)
  , defaultResourceDetailedSettings
  , resourceDetailedComponent
  )
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.Component.Task.Detailed
  ( TaskDetailedConfig (..)
  , TaskDetailedSettings (..)
  , defaultTaskDetailedSettings
  , taskDetailedComponent
  )
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , SyncDocumentEnv (..)
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.SyncContext.WindowManager
  ( PinCategory (..)
  , PinId
  , PinMeta (..)
  , SortAtom (..)
  , SortKey (..)
  , WindowChrome (..)
  , inlineComponent
  , inlineComponentWith
  , justLens
  , mkPinId
  , pinDialogWith
  , pinSaveStateLens
  )
import Competences.Frontend.SyncContext.WindowManager qualified as WM (Model)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.TaskContent.RichContent (RichContent)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core ((.~), (&), (%), (^.))
import Optics.Core qualified as O

-- | Audience the rendering is tuned for.
data LessonDetailedMode
  = StudentMode
  -- ^ Student-facing: only published items, no teacher notes.
  | TeacherMode
  -- ^ Teacher-facing lecture / print view: all items (unpublished
  -- ones dimmed with a badge), plus lesson- and phase-level teacher
  -- notes where present.
  deriving (Eq, Generic, Show)

data LessonDetailedConfig = LessonDetailedConfig
  { lessonId :: !LessonId
  , mode :: !LessonDetailedMode
  }
  deriving (Eq, Generic, Show)

-- | Pin the student-facing "Schulübung" rendering. Used from the
-- Lesson 'EntityMenu' so teachers can preview what students will see.
pinStudentLessonView :: SyncContext -> Lesson -> IO ()
pinStudentLessonView r lsn =
  pinDetailed r lsn StudentMode "lesson-record-"

-- | Pin the teacher lesson plan rendering (full detail, teacher notes,
-- unpublished items shown with a badge). Used while lecturing and as
-- the basis for the print view.
pinTeacherLessonPlan :: SyncContext -> Lesson -> IO ()
pinTeacherLessonPlan r lsn =
  pinDetailed r lsn TeacherMode "lesson-plan-"

pinDetailed :: SyncContext -> Lesson -> LessonDetailedMode -> T.Text -> IO ()
pinDetailed r lsn mode keyPrefix =
  let meta = PinMeta
        { key = keyPrefix <> idToText lsn.id
        , category = PinCatLesson
        , sortKey = SortKey [SortAtom lsn.order, SortAtom lsn.date, SortAtom lsn.id]
        , context = fmap C.formatDayShort lsn.date
        , isEditor = False
        , followUp = True
        }
      chrome = WindowChrome (ms (lessonDerivedTitle lsn)) Icon.IcnLessonRecord Nothing
      pid = mkPinId meta.key
      cfg = LessonDetailedConfig lsn.id mode
   in pinDialogWith r.windowManager meta chrome
        (\_wm mSaved -> lessonDetailedPinComponent r cfg pid mSaved)

data Projection = Projection
  { lesson :: !(Maybe Lesson)
  , resolvedAssignments :: !(Map AssignmentId Assignment)
  , lessonNoteContent :: !(Maybe RichContent)
  , phaseNoteContents :: ![Maybe RichContent]
  -- ^ Aligned positionally with @lesson.phases@; consumed by zipping
  -- rather than by index lookup so the alignment can't drift.
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

data Model = Model
  { projection :: !Projection
  , expandedAssignments :: !(Set AssignmentId)
    -- ^ Which embedded assignment disclosures are open. Owned here
    -- (rather than per assignment-component) so the set is captured
    -- by the pin's save state — closing and re-opening a pinned
    -- lesson restores the same expansion. Synced bidirectionally
    -- via 'Assignment.Detailed.bindVisibility'.
  }
  deriving (Eq, Generic, Show)

data Action
  = ProjectionChanged !(ProjectedChange Projection)
  deriving (Eq, Show)

-- | Mount the lesson detailed view inline. No save-state binding —
-- the component's lifetime is tied to its parent's. For pin use, see
-- 'lessonDetailedPinComponent'.
lessonDetailedComponent
  :: SyncContext -> LessonDetailedConfig -> M.Component p Model Action
lessonDetailedComponent r cfg =
  (M.component (initialModel Nothing) (update' r cfg) (view' r cfg))
    { M.subs = [subscribeWithProjection r (projection cfg) ProjectionChanged]
    }

-- | Mount the lesson detailed view as a pinned dialog. The pin's
-- 'PinId' wires the save-state binding so the top-level expansion
-- state (homework disclosure, expanded assignments) survives the pin
-- being closed and re-opened.
--
-- Note: child components mounted inline (tasks, resources, assignment
-- viewers) keep their own state and currently DO reset on remount.
-- Preserving theirs requires a separate per-child save-state design.
lessonDetailedPinComponent
  :: SyncContext
  -> LessonDetailedConfig
  -> PinId
  -> Maybe Model
  -> M.Component WM.Model Model Action
lessonDetailedPinComponent r cfg pid mSaved =
  (M.component (initialModel mSaved) (update' r cfg) (view' r cfg))
    { M.subs = [subscribeWithProjection r (projection cfg) ProjectionChanged]
    , M.bindings = [O.toLensVL (pinSaveStateLens pid) M.<--- O.toLensVL justLens]
    }

initialModel :: Maybe Model -> Model
initialModel = fromMaybe emptyModel
  where
    emptyModel =
      Model
        { projection =
            Projection
              { lesson = Nothing
              , resolvedAssignments = Map.empty
              , lessonNoteContent = Nothing
              , phaseNoteContents = []
              , focusedUser = Nothing
              }
        , expandedAssignments = Set.empty
        }

update' :: SyncContext -> LessonDetailedConfig -> Action -> M.Effect p Model Action
update' _ _ (ProjectionChanged change) = M.modify $ #projection .~ change.projection

view' :: SyncContext -> LessonDetailedConfig -> Model -> M.View Model Action
view' r cfg m = case m.projection.lesson of
  Nothing -> Layout.empty
  Just lsn -> renderLesson r cfg.mode m lsn

projection :: LessonDetailedConfig -> Document -> Maybe User -> Projection
projection cfg doc mUser =
  let mLesson = Ix.getOne (doc.lessons Ix.@= cfg.lessonId)
      referencedIds = maybe [] allAssignmentIds mLesson
      resolved = Map.fromList
        [ (aid, a)
        | aid <- referencedIds
        , Just a <- [Ix.getOne (doc.assignments Ix.@= aid)]
        ]
      (lessonNote, phaseNotes) = case (cfg.mode, mLesson) of
        (TeacherMode, Just lsn) ->
          ( resolveNote doc lsn.privateNoteRef
          , map (resolveNote doc . (.privateNoteRef)) lsn.phases
          )
        _ -> (Nothing, maybe [] (map (const Nothing) . (.phases)) mLesson)
   in Projection
        { lesson = mLesson
        , resolvedAssignments = resolved
        , lessonNoteContent = lessonNote
        , phaseNoteContents = phaseNotes
        , focusedUser = mUser
        }

resolveNote :: Document -> Maybe TeachingNoteId -> Maybe RichContent
resolveNote _ Nothing = Nothing
resolveNote doc (Just tnId) =
  (.content) <$> Ix.getOne (doc.teachingNotes Ix.@= tnId)

-- | All assignment ids referenced by any item (phase or supplemental)
-- in the lesson, deduplicated.
allAssignmentIds :: Lesson -> [AssignmentId]
allAssignmentIds lsn =
  let fromItem = \case
        LessonItem {content = PhaseAssignment aid} -> [aid]
        _ -> []
      phaseItems = concatMap (.items) lsn.phases
   in Set.toList $ Set.fromList $ concatMap fromItem (phaseItems <> lsn.supplementalItems)

-- ============================================================================
-- Rendering
-- ============================================================================

renderLesson :: SyncContext -> LessonDetailedMode -> Model -> Lesson -> M.View Model Action
renderLesson r mode m lsn =
  let findAssignment aid = Map.lookup aid m.projection.resolvedAssignments
      -- All items across phases + supplemental, filtered by visibility
      -- rule for the mode.
      visibleItems = case mode of
        StudentMode -> filter (.publish)
        TeacherMode -> id
      allVisibleItems =
        concatMap (visibleItems . (.items)) lsn.phases
          <> visibleItems lsn.supplementalItems

      -- Home-exercise assignments: any visible PhaseAssignment whose
      -- assignment is a HomeExercise. Deduplicated by id, first-wins.
      homeExerciseRows =
        dedupByFst
          [ (aid, a)
          | item <- allVisibleItems
          , PhaseAssignment aid <- [item.content]
          , Just a <- [findAssignment aid]
          , a.activityType == HomeExercise
          ]

      homeExIds = Set.fromList [ PhaseAssignment aid | (aid, _) <- homeExerciseRows ]

      body = case mode of
        TeacherMode -> teacherBody r m lsn findAssignment visibleItems homeExIds homeExerciseRows
        StudentMode -> studentBody r m lsn findAssignment allVisibleItems homeExIds homeExerciseRows
   in Layout.vFlow Layout.gapM (titleBlock lsn : body)

teacherBody
  :: SyncContext
  -> Model
  -> Lesson
  -> (AssignmentId -> Maybe Assignment)
  -> ([LessonItem] -> [LessonItem])
  -> Set.Set LessonItemContent
  -> [(AssignmentId, Assignment)]
  -> [M.View Model Action]
teacherBody r m lsn findAssignment visibleItems homeExIds homeExerciseRows =
  let renderAssignment = assignmentRow r m
      phasesWithNotes = zip lsn.phases (m.projection.phaseNoteContents <> repeat Nothing)
      renderPhaseSection (phase, phaseNote) seenBefore =
        let phaseItems = visibleItems phase.items
            rendered = renderPhaseItems r renderAssignment findAssignment phaseItems homeExIds seenBefore
            newlySeen = seenAfter phaseItems homeExIds seenBefore
         in (phaseShell r phase phaseNote rendered, newlySeen)
      phaseBlocks = phasesAccum renderPhaseSection homeExIds phasesWithNotes
      suppItems = visibleItems lsn.supplementalItems
      suppBlock =
        let afterPhases = lastSeen phaseBlocks
         in renderPhaseItems r renderAssignment findAssignment suppItems homeExIds afterPhases
   in concat
        [ [descriptionBlock r lsn.description | lsn.description /= mempty]
        , [lessonNoteBlock r content | Just content <- [m.projection.lessonNoteContent]]
        , [homeExerciseBlock renderAssignment homeExerciseRows | not (null homeExerciseRows)]
        , [blocks | (blocks, _) <- phaseBlocks]
        , [supplementalBlock suppBlock | not (null suppBlock)]
        ]

-- | Student mode: homework disclosure + a flat deduplicated list of
-- every published item across phases + supplemental. No phase headers,
-- no supplemental heading — lesson notes stay compact.
studentBody
  :: SyncContext
  -> Model
  -> Lesson
  -> (AssignmentId -> Maybe Assignment)
  -> [LessonItem]
  -> Set.Set LessonItemContent
  -> [(AssignmentId, Assignment)]
  -> [M.View Model Action]
studentBody r m _lsn findAssignment allVisibleItems homeExIds homeExerciseRows =
  let renderAssignment = assignmentRow r m
      flatRendered =
        renderPhaseItems r renderAssignment findAssignment allVisibleItems homeExIds Set.empty
   in concat
        [ [homeExerciseBlock renderAssignment homeExerciseRows | not (null homeExerciseRows)]
        , [MH.div_ [class_ "space-y-2"] flatRendered | not (null flatRendered)]
        ]

-- | Accumulate per-phase rendered items, threading "already seen" set.
phasesAccum
  :: (a -> Set.Set LessonItemContent -> (M.View Model Action, Set.Set LessonItemContent))
  -> Set.Set LessonItemContent
  -> [a]
  -> [(M.View Model Action, Set.Set LessonItemContent)]
phasesAccum _ _ [] = []
phasesAccum f seen (p : ps) =
  let (v, newSeen) = f p seen
   in (v, newSeen) : phasesAccum f newSeen ps

lastSeen :: [(M.View Model Action, Set.Set LessonItemContent)] -> Set.Set LessonItemContent
lastSeen = foldr (\(_, s) _ -> s) Set.empty . reverse

seenAfter
  :: [LessonItem]
  -> Set.Set LessonItemContent
  -> Set.Set LessonItemContent
  -> Set.Set LessonItemContent
seenAfter items homeEx seen =
  let newIds =
        [ it.content
        | it <- items
        , not (it.content `Set.member` homeEx)
        , not (it.content `Set.member` seen)
        ]
   in Set.union seen (Set.fromList newIds)

dedupByFst :: (Eq a, Ord a) => [(a, b)] -> [(a, b)]
dedupByFst = go Set.empty
  where
    go _ [] = []
    go seen ((k, v) : xs)
      | k `Set.member` seen = go seen xs
      | otherwise = (k, v) : go (Set.insert k seen) xs

-- ============================================================================
-- Blocks
-- ============================================================================

titleBlock :: Lesson -> M.View Model Action
titleBlock lsn = Typography.h2 (ms (lessonDerivedTitle lsn))

-- | The title shown for a lesson in any student- or teacher-facing
-- rendering: the explicit override if present, else "Schulübung vom
-- DD.MM.YYYY" for dated lessons, else the plain lesson title.
lessonDerivedTitle :: Lesson -> T.Text
lessonDerivedTitle lsn = case lsn.notesTitleOverride of
  Just t | not (T.null t) -> t
  _ -> case lsn.date of
    Just d ->
      M.fromMisoString (C.translate' C.LblLessonRecordFromPrefix) <> " "
        <> T.pack (formatTime defaultTimeLocale "%d.%m.%Y" d)
    Nothing -> lsn.title

descriptionBlock :: SyncContext -> RichContent -> M.View Model Action
descriptionBlock r content =
  MH.div_ [class_ "text-sm"] [renderRichText r.formulaCache content]

lessonNoteBlock :: SyncContext -> RichContent -> M.View Model Action
lessonNoteBlock r content =
  MH.div_
    [class_ "rounded-md border border-border bg-muted/30 p-3 space-y-1 text-sm"]
    [ MH.div_
        [class_ "text-xs font-medium uppercase tracking-wide text-muted-foreground"]
        [M.text (C.translate' C.LblTeachingNotes)]
    , renderRichText r.formulaCache content
    ]

homeExerciseBlock
  :: (Assignment -> M.View Model Action)
  -> [(AssignmentId, Assignment)]
  -> M.View Model Action
homeExerciseBlock renderAssignment rows =
  -- Plain flat list — most lessons have a single home exercise; even
  -- with several, the assignment rows are themselves disclosures, so
  -- a wrapping "Hausübungen" heading would just nest.
  MH.div_ [class_ "space-y-1"] (map (renderAssignment . snd) rows)

-- | Always renders the phase header so the teacher sees the planned
-- structure of the lesson even when a phase is empty (no items, no
-- note). The note and items are only included when present.
phaseShell
  :: SyncContext
  -> LessonPhase
  -> Maybe RichContent
  -> [M.View Model Action]
  -> M.View Model Action
phaseShell r phase phaseNote renderedItems =
  MH.div_
    [class_ "space-y-2"]
    $ concat
      [ [phaseHeader phase]
      , [phaseNoteBlock r note | Just note <- [phaseNote]]
      , [MH.div_ [class_ "space-y-2"] renderedItems | not (null renderedItems)]
      ]

phaseHeader :: LessonPhase -> M.View Model Action
phaseHeader phase =
  MH.div_
    [class_ "flex items-center justify-between gap-2 border-b border-border pb-1"]
    [ MH.span_
        [class_ "font-medium truncate"]
        [M.text (if T.null phase.title then "(Phase)" else ms phase.title)]
    , MH.div_
        [class_ "flex items-center gap-2 shrink-0 text-xs text-muted-foreground"]
        [ Icon.icon [class_ "w-4 h-4"] (actionFormIcon phase.actionForm)
        , Icon.icon [class_ "w-4 h-4"] (socialFormIcon phase.socialForm)
        , MH.span_ [] [M.text (ms (show phase.duration) <> " Min")]
        ]
    ]

phaseNoteBlock :: SyncContext -> RichContent -> M.View Model Action
phaseNoteBlock r note =
  MH.div_
    [class_ "pl-2 border-l-2 border-muted text-sm text-muted-foreground"]
    [renderRichText r.formulaCache note]

supplementalBlock :: [M.View Model Action] -> M.View Model Action
supplementalBlock rendered
  | null rendered = Layout.empty
  | otherwise =
      MH.div_
        [class_ "space-y-2 border-t border-border pt-4"]
        [ Typography.h4 (C.translate' C.LblSupplemental)
        , MH.div_ [class_ "space-y-2"] rendered
        ]

-- ============================================================================
-- Per-item rendering
-- ============================================================================

renderPhaseItems
  :: SyncContext
  -> (Assignment -> M.View Model Action)
  -> (AssignmentId -> Maybe Assignment)
  -> [LessonItem]
  -> Set.Set LessonItemContent
  -> Set.Set LessonItemContent
  -> [M.View Model Action]
renderPhaseItems r renderAssignment findAssignment items homeEx seen =
  let keep it =
        not (it.content `Set.member` homeEx)
          && not (it.content `Set.member` seen)
   in [ renderOne r renderAssignment findAssignment it | it <- items, keep it ]

-- | Task settings used when mounting a task inside a lesson record.
-- Solutions are open by default — the lesson record is a teaching
-- aid, so the teacher wants the answers visible without an extra
-- click.
lessonRecordTaskSettings :: TaskDetailedSettings
lessonRecordTaskSettings = defaultTaskDetailedSettings { solutionsExpandedByDefault = True }

renderOne
  :: SyncContext
  -> (Assignment -> M.View Model Action)
  -> (AssignmentId -> Maybe Assignment)
  -> LessonItem
  -> M.View Model Action
renderOne r renderAssignment findAssignment item =
  let card = case item.content of
        PhaseResource rid ->
          inlineComponent ("lesson-record-res-" <> ms (show rid))
            (resourceDetailedComponent r (ResourceDetailedConfig rid defaultResourceDetailedSettings))
        PhaseTask tid ->
          inlineComponent ("lesson-record-task-" <> ms (show tid))
            (taskDetailedComponent r (TaskDetailedConfig tid Published lessonRecordTaskSettings))
        PhaseAssignment aid ->
          case findAssignment aid of
            Nothing -> Layout.empty
            Just a -> renderAssignment a
   in if item.publish
        then card
        else unpublishedWrapper card

-- | Wrap a card in a dimmed container with a "not visible to students"
-- badge — only reached in teacher mode (student mode filters
-- unpublished items out upstream).
unpublishedWrapper :: M.View Model Action -> M.View Model Action
unpublishedWrapper card =
  MH.div_
    [class_ "relative opacity-60"]
    [ MH.div_
        [class_ "absolute right-2 top-2 z-10"]
        [Badge.outline (Badge.badgeText (C.translate' C.LblNotVisibleToStudents))]
    , card
    ]

-- | Mounts the assignment viewer inline in 'Embedded' render style for
-- both students and teachers. The viewer's action cluster branches by
-- role (students see the submission-status button + pin; teachers see
-- the status icon + pin + print + entity menu). Expansion state is
-- bound bidirectionally to the lesson's 'expandedAssignments' set,
-- seeded from the same slot at construction time so a freshly mounted
-- child renders the saved state immediately — a pinned lesson
-- restores the same open assignments after close/reopen.
assignmentRow :: SyncContext -> Model -> Assignment -> M.View Model Action
assignmentRow r m a =
  inlineComponentWith
    ("assignment-viewer-" <> ms (show a.id))
    (\wm ->
       viewerComponent r (syncDocumentEnv r).connectedUser a Embedded wm
         & bindVisibility lens (m ^. lens))
  where
    lens = #expandedAssignments % memberLens a.id

-- ============================================================================
-- Phase metadata icons
-- ============================================================================

actionFormIcon :: ActionForm -> Icon.Icon
actionFormIcon = \case
  Presenting -> Icon.IcnPhaseActPresenting
  Collaborating -> Icon.IcnPhaseActCollaborating
  Assigning -> Icon.IcnPhaseActAssigning

socialFormIcon :: TeachingSocialForm -> Icon.Icon
socialFormIcon = \case
  WholeClass -> Icon.IcnPhaseWholeClass
  SmallGroups -> Icon.IcnPhaseSmallGroup
  PairWork -> Icon.IcnPhasePair
  IndividualWork -> Icon.IcnSocialFormIndividual
