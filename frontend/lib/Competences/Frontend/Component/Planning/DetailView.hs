module Competences.Frontend.Component.Planning.DetailView
  ( detailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), LessonsCommand (..), MesoPlansCommand (..))
import Competences.Frontend.Clipboard (copyToClipboard)
import Competences.Import.Export (exportLesson)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), Lesson (..))
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Lesson (ActionForm (..), LessonId, LessonPhase (..))
import Competences.Document.LessonNotes (LessonNotes (..))
import Competences.Document.MesoPlan (MesoPlan (..))
import Competences.Document.Order (Reorder (..), orderMax, orderPosition)
import Competences.Query.Lesson qualified as QLesson
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal (openMesoPlanEditor)
import Competences.Frontend.Component.Planning.ImportModal qualified as ImportModal
import Competences.Frontend.Component.Planning.LessonEditorModal (openLessonEditor)
import Competences.Frontend.Component.Assignment.EditorDetail (pinAssignmentEvaluator)
import Competences.Frontend.Component.Planning.LessonEvaluator (pinLessonEvaluator)
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.DateDisplay qualified as DateDisplay
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH

-- ============================================================================
-- PLANNING DETAIL VIEW
-- ============================================================================

-- | Minimal model - only what's needed beyond Document projection
data DetailModel = DetailModel
  { mesoPlan :: !MesoPlan
  , lessons :: ![Lesson]
  , expandedLessonId :: !(Maybe LessonId)
  , expandedAssignments :: !(Set.Set LessonId)  -- Track which lessons have assignments expanded
  , expandedNotes :: !(Set.Set LessonId)  -- Notes section per lesson
  , expandedPhases :: !(Set.Set LessonId)  -- Phases section per lesson
  , document :: !Document
  , reorderFrom :: !(Maybe LessonId)  -- Which lesson is being moved (reorder mode)
  }
  deriving (Eq, Generic, Show)

-- | Actions for the planning component
data DetailAction
  = DocumentUpdated !DocumentChange
  | CreateNewLesson
  | ToggleLessonExpansion !LessonId
  | ToggleAssignmentsExpanded !LessonId
  | ToggleNotesExpanded !LessonId
  | TogglePhasesExpanded !LessonId
  | OpenLessonEditorModal !Lesson
  | OpenMesoPlanEditorModal !MesoPlan
  | DeleteLesson !LessonId
  | DeleteMesoPlan
  | PinLessonEvaluation !Lesson
  | PinAssignmentEvaluation !Assignment
  | OpenLessonImportModal
  | ExportLesson !Lesson
  | StartReorder !LessonId
  | CancelReorder
  | ReorderTo !(Reorder Lesson)
  deriving (Eq, Show)

-- | Project from document to minimal model, preserving UI state
projectDetail
  :: MesoPlan
  -> Maybe LessonId
  -> Set.Set LessonId
  -> Set.Set LessonId
  -> Set.Set LessonId
  -> Document
  -> DetailModel
projectDetail plan prevExpanded prevExpandedAssignments prevExpandedNotes prevExpandedPhases doc =
  let -- Get fresh plan from document (may have been updated)
      plan' = maybe plan id $ Ix.getOne (doc.mesoPlans Ix.@= plan.id)
      lessons' = QLesson.mesoPlanLessons doc plan'.id
      lessonIds = Set.fromList $ map (.id) lessons'
      -- Clear expansion if the lesson no longer exists
      expanded = case prevExpanded of
        Nothing -> Nothing
        Just lid -> if any (\l -> l.id == lid) lessons' then Just lid else Nothing
      -- Clean up expanded states for lessons that no longer exist
      expandedAssignments' = Set.intersection prevExpandedAssignments lessonIds
      expandedNotes' = Set.intersection prevExpandedNotes lessonIds
      expandedPhases' = Set.intersection prevExpandedPhases lessonIds
   in DetailModel plan' lessons' expanded expandedAssignments' expandedNotes' expandedPhases' doc Nothing

-- | View for planning - allows editing meso plan and lessons
detailView
  :: SyncContext
  -> MesoPlan
  -> M.View (SD.Model MesoPlan mode) (SD.Action mode)
detailView r plan =
  inlineComponent
    ("planning-detail-" <> M.ms (show plan.id))
    (detailComponent r plan)

detailComponent :: SyncContext -> MesoPlan -> M.Component p DetailModel DetailAction
detailComponent r initialPlan =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel = DetailModel initialPlan [] Nothing Set.empty Set.empty Set.empty emptyDocument Nothing

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
        , submissions = Ix.empty
        }

    update (DocumentUpdated dc) = M.modify $ \m -> projectDetail m.mesoPlan m.expandedLessonId m.expandedAssignments m.expandedNotes m.expandedPhases dc.document

    update CreateNewLesson = do
      m <- M.get
      M.io_ $ do
        lessonId <- nextId r
        let lesson =
              Lesson
                { id = lessonId
                , mesoPlanId = m.mesoPlan.id
                , order = orderMax
                , title = ""
                , description = mempty
                , competenceLevels = []
                , date = Nothing
                , assignments = []
                , resources = []
                , phases = []
                , notes = mempty
                }
        modifySyncDocument r (Lessons $ OnLessons $ CreateAndLock lesson)
        openLessonEditor r lesson []

    update (ToggleLessonExpansion lessonId) = M.modify $ \m ->
      if m.expandedLessonId == Just lessonId
        then m{expandedLessonId = Nothing}
        else m{expandedLessonId = Just lessonId}

    update (ToggleAssignmentsExpanded lessonId) = M.modify $ \m ->
      let newExpanded =
            if Set.member lessonId m.expandedAssignments
              then Set.delete lessonId m.expandedAssignments
              else Set.insert lessonId m.expandedAssignments
       in m {expandedAssignments = newExpanded}

    update (ToggleNotesExpanded lessonId) = M.modify $ \m ->
      let newExpanded =
            if Set.member lessonId m.expandedNotes
              then Set.delete lessonId m.expandedNotes
              else Set.insert lessonId m.expandedNotes
       in m {expandedNotes = newExpanded}

    update (TogglePhasesExpanded lessonId) = M.modify $ \m ->
      let newExpanded =
            if Set.member lessonId m.expandedPhases
              then Set.delete lessonId m.expandedPhases
              else Set.insert lessonId m.expandedPhases
       in m {expandedPhases = newExpanded}

    update (OpenLessonEditorModal lesson) = do
      m <- M.get
      let lessonNotesIds = map (.id) $ Ix.toList $ m.document.lessonNotes Ix.@= lesson.id
      M.io_ $ openLessonEditor r lesson lessonNotesIds

    update (OpenMesoPlanEditorModal plan) = M.io_ $
      openMesoPlanEditor r plan

    update (DeleteLesson lessonId) = M.io_ $
      modifySyncDocument r (Lessons $ OnLessons $ Delete lessonId)

    update DeleteMesoPlan = do
      m <- M.get
      M.io_ $ modifySyncDocument r (MesoPlans $ OnMesoPlans $ Delete m.mesoPlan.id)

    update (PinLessonEvaluation lesson) = do
      m <- M.get
      M.io_ $ pinLessonEvaluator r m.mesoPlan.dateFrom lesson

    update (PinAssignmentEvaluation assignment) = M.io_ $
      pinAssignmentEvaluator r assignment

    update (ExportLesson lesson) = do
      m <- M.get
      M.io_ $ copyToClipboard (exportLesson m.document lesson)

    update OpenLessonImportModal = do
      m <- M.get
      M.io_ $ ImportModal.openLessonImportModal r m.mesoPlan.id

    update (StartReorder lid) =
      M.modify $ \m -> m{reorderFrom = Just lid}

    update CancelReorder =
      M.modify $ \m -> m{reorderFrom = Nothing}

    update (ReorderTo target) = do
      m <- M.get
      case m.reorderFrom of
        Nothing -> pure ()
        Just fromId -> do
          M.io_ $ case orderPosition m.document.lessons fromId of
            Nothing -> pure ()
            Just pos -> modifySyncDocument r (Lessons $ ReorderLesson pos target)
          M.modify $ \m' -> m'{reorderFrom = Nothing}

    view m =
      Layout.vFlow
        (Layout.gapS <> Layout.wFull <> Layout.crossCenter)
        [ -- Plan header with title, dates, edit and delete buttons
          MH.div_
            [class_ "p-3 bg-muted/30 rounded-lg mb-2"]
            [ Layout.hFlow
                (Layout.hFull <> Layout.crossCenter <> Layout.mainBetween)
                [ Layout.vFlow Layout.gapT
                    [ Typography.h2 $ M.ms $ if Text.null m.mesoPlan.title then "(Untitled)" else m.mesoPlan.title
                    , let dr = DateDisplay.formatDateRange m.mesoPlan.dateFrom m.mesoPlan.dateTo
                       in if dr == ""
                            then M.text ""
                            else MH.span_ [class_ "text-sm text-muted-foreground"] [M.text dr]
                    ]
                , Layout.hFlow Layout.gapT
                    [ Button.ghostSm (Button.button Icon.IcnEdit (OpenMesoPlanEditorModal m.mesoPlan))
                    , Button.destructiveSm (Button.button Icon.IcnDelete DeleteMesoPlan)
                    ]
                ]
            ]
        , MH.div_
            [class_ "w-full"]
            [ Layout.vFlow Layout.gapS
                (map (viewLesson m) m.lessons)
            ]
        , Layout.hFlow Layout.gapS
            [ Button.primary (Button.button (Icon.IcnAdd, C.LblAddLesson) CreateNewLesson)
            , Button.ghost (Button.button (Icon.IcnImport, C.LblImportLessons) OpenLessonImportModal)
            ]
        ]

    viewLesson m lesson =
      let isExpanded = m.expandedLessonId == Just lesson.id
          titleView = Disclosure.titleText $ M.ms $ if Text.null lesson.title then "(Untitled)" else lesson.title
          actions = case m.reorderFrom of
            Nothing ->
              -- Normal mode: Export, Pin, Edit, Reorder, Delete
              [ Disclosure.Action Icon.IcnExport (ExportLesson lesson)
              , Disclosure.Action Icon.IcnPin (PinLessonEvaluation lesson)
              , Disclosure.Action Icon.IcnEdit (OpenLessonEditorModal lesson)
              , Disclosure.Action Icon.IcnReorder (StartReorder lesson.id)
              , Disclosure.DestructiveAction Icon.IcnDelete (DeleteLesson lesson.id)
              ]
            Just fromId
              | fromId == lesson.id ->
                  -- Source lesson: Cancel
                  [Disclosure.DestructiveAction Icon.IcnCancel CancelReorder]
              | otherwise ->
                  -- Target lesson: Before/After
                  [ Disclosure.Action Icon.IcnArrowUp (ReorderTo (Before lesson.id))
                  , Disclosure.Action Icon.IcnArrowDown (ReorderTo (After lesson.id))
                  ]
       in Disclosure.disclosure (ToggleLessonExpansion lesson.id) $
            Disclosure.contents titleView isExpanded (viewExpandedLesson m lesson) actions

    viewExpandedLesson m lesson =
      let lessonAssignmentIds = lesson.assignments
       in MH.div_
        [class_ "p-4 border-t border-border bg-background space-y-3"]
        [ -- Date
          case lesson.date of
            Nothing -> M.text ""
            Just d ->
              MH.div_
                [class_ "text-sm text-muted-foreground"]
                [M.text $ C.translate' C.LblLessonDate <> ": " <> C.formatDay d]
        , -- Description
          if lesson.description == mempty
            then M.text ""
            else MH.div_ [class_ "text-sm"] [renderRichText r.formulaCache lesson.description]
        , -- Assignments collapsible
          if null lessonAssignmentIds
            then M.text ""
            else
              let assignmentsExpanded = Set.member lesson.id m.expandedAssignments
                  assignmentsTitleView = Disclosure.titleText $ C.translate' C.LblLessonAssignments
                    <> " (" <> M.ms (show (length lessonAssignmentIds)) <> ")"
                  assignmentsBody = MH.div_
                    [class_ "space-y-1"]
                    (map (viewAssignmentSummary m.document) lessonAssignmentIds)
               in Disclosure.innerDisclosure (ToggleAssignmentsExpanded lesson.id) $
                    Disclosure.contents assignmentsTitleView assignmentsExpanded assignmentsBody []
        , -- Notes collapsible
          if lesson.notes == mempty
            then M.text ""
            else
              let notesExpanded = Set.member lesson.id m.expandedNotes
                  notesTitleView = Disclosure.titleText $ C.translate' C.LblTeachingNotes
                  notesBody = MH.div_ [class_ "text-sm"] [renderRichText r.formulaCache lesson.notes]
               in Disclosure.innerDisclosure (ToggleNotesExpanded lesson.id) $
                    Disclosure.contents notesTitleView notesExpanded notesBody []
        , -- Phases collapsible
          if null lesson.phases
            then M.text ""
            else
              let phasesExpanded = Set.member lesson.id m.expandedPhases
                  phasesTitleView = Disclosure.titleText $ C.translate' C.LblLessonPhases
                    <> " (" <> M.ms (show (length lesson.phases)) <> ")"
                  phasesBody = MH.div_
                    [class_ "space-y-2"]
                    (zipWith viewPhaseSummary [1 :: Int ..] lesson.phases)
               in Disclosure.innerDisclosure (TogglePhasesExpanded lesson.id) $
                    Disclosure.contents phasesTitleView phasesExpanded phasesBody []
        ]

    viewPhaseSummary idx phase =
      let borderColor = case phase.actionForm of
            Presenting -> "border-l-red-500"
            Collaborating -> "border-l-orange-500"
            Assigning -> "border-l-green-500"
          title = M.ms $ if Text.null phase.title then "Phase " <> Text.pack (show idx) else phase.title
       in MH.div_
            [class_ $ "text-sm p-2 bg-muted/30 rounded border-l-4 " <> borderColor]
            [ Layout.hFlow
                (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
                [ MH.span_ [class_ "font-medium"] [M.text title]
                , MH.span_ [class_ "text-muted-foreground"]
                    [ M.text $ M.ms (show phase.duration) <> " min"
                    , M.text " · "
                    , M.text $ C.translate' (C.LblTeachingSocialForm phase.socialForm)
                    , M.text " · "
                    , M.text $ C.translate' (C.LblActionForm phase.actionForm)
                    ]
                ]
            , if phase.notes == mempty
                then M.text ""
                else MH.div_ [class_ "mt-1 text-muted-foreground pl-2 border-l-2 border-muted text-sm"]
                  [renderRichText r.formulaCache phase.notes]
            ]

    viewAssignmentSummary doc aId =
      case Ix.getOne (doc.assignments Ix.@= aId) of
        Nothing -> MH.div_ [class_ "text-sm text-muted-foreground italic"] [M.text "(Unknown assignment)"]
        Just a ->
          let AssignmentName nameText = a.name
           in MH.div_
                [class_ "text-sm p-1 rounded hover:bg-muted/30"]
                [ Layout.hFlow
                    (Layout.hFull <> Layout.crossCenter <> Layout.mainBetween)
                    [ M.text $ M.ms nameText
                    , Button.ghost (Button.button Icon.IcnPin (PinAssignmentEvaluation a))
                    ]
                ]


