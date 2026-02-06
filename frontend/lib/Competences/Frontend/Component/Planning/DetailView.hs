module Competences.Frontend.Component.Planning.DetailView
  ( detailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), LessonsCommand (..), MesoPlansCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), Lesson (..))
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Id (idToText)
import Competences.Document.Lesson (ActionForm (..), LessonId, LessonPhase (..))
import Competences.Document.MesoPlan (MesoPlan (..))
import Competences.Document.Order (orderMax)
import Competences.Document.Resource (ResourceId)
import Competences.Query.Lesson qualified as QLesson
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal (mesoPlanEditorModal)
import Competences.Frontend.Component.Planning.LessonEditorModal (lessonEditorModal)
import Competences.Frontend.Component.Assignment.EvaluatorDetail (evaluatorComponent)
import Competences.Frontend.Component.Planning.LessonEvaluator (lessonEvaluatorComponent)
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager (AnyPinnedDialog (..), PinId (..), openModal, pinDialog)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.DateDisplay qualified as DateDisplay
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.ResourceList qualified as ResourceList
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- PLANNING DETAIL VIEW
-- ============================================================================

-- | Minimal model - only what's needed beyond Document projection
data DetailModel = DetailModel
  { mesoPlan :: !MesoPlan
  , lessons :: ![Lesson]
  , expandedLessonId :: !(Maybe LessonId)
  , expandedResources :: !(Set.Set ResourceId)
  , document :: !Document
  }
  deriving (Eq, Generic, Show)

-- | Actions for the planning component
data DetailAction
  = DocumentUpdated !DocumentChange
  | CreateNewLesson
  | ToggleLessonExpansion !LessonId
  | ToggleResourceExpanded !ResourceId
  | OpenLessonEditorModal !Lesson
  | OpenMesoPlanEditorModal !MesoPlan
  | DeleteLesson !LessonId
  | DeleteMesoPlan
  | PinLessonEvaluation !Lesson
  | PinAssignmentEvaluation !Assignment
  deriving (Eq, Show)

-- | Project from document to minimal model, preserving UI state
projectDetail :: MesoPlan -> Maybe LessonId -> Set.Set ResourceId -> Document -> DetailModel
projectDetail plan prevExpanded prevExpandedResources doc =
  let -- Get fresh plan from document (may have been updated)
      plan' = maybe plan id $ Ix.getOne (doc.mesoPlans Ix.@= plan.id)
      lessons' = QLesson.mesoPlanLessons doc plan'.id
      -- Clear expansion if the lesson no longer exists
      expanded = case prevExpanded of
        Nothing -> Nothing
        Just lid -> if any (\l -> l.id == lid) lessons' then Just lid else Nothing
   in DetailModel plan' lessons' expanded prevExpandedResources doc

-- | View for planning - allows editing meso plan and lessons
detailView
  :: SyncContext
  -> MesoPlan
  -> M.View (SD.Model MesoPlan mode) (SD.Action mode)
detailView r plan =
  V.component
    ("planning-detail-" <> M.ms (show plan.id))
    (detailComponent r plan)

detailComponent :: SyncContext -> MesoPlan -> M.Component p DetailModel DetailAction
detailComponent r initialPlan =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel = DetailModel initialPlan [] Nothing Set.empty emptyDocument

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
        , participationRecords = Ix.empty
        }

    update (DocumentUpdated dc) = M.modify $ \m -> projectDetail m.mesoPlan m.expandedLessonId m.expandedResources dc.document

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
                , resources = []
                , phases = []
                , notes = mempty
                }
        modifySyncDocument r (Lessons $ OnLessons $ CreateAndLock lesson)
        openModal r.windowManager (lessonEditorModal r r.windowManager lesson [])

    update (ToggleLessonExpansion lessonId) = M.modify $ \m ->
      if m.expandedLessonId == Just lessonId
        then m & #expandedLessonId .~ Nothing
        else m & #expandedLessonId .~ Just lessonId

    update (ToggleResourceExpanded resId) = M.modify $ \m ->
      let newExpanded =
            if Set.member resId m.expandedResources
              then Set.delete resId m.expandedResources
              else Set.insert resId m.expandedResources
       in m {expandedResources = newExpanded}

    update (OpenLessonEditorModal lesson) = do
      m <- M.get
      let assignmentIds = map (.id) $ Ix.toList $ m.document.assignments Ix.@= lesson.id
      M.io_ $
        openModal r.windowManager (lessonEditorModal r r.windowManager lesson assignmentIds)

    update (OpenMesoPlanEditorModal plan) = M.io_ $
      openModal r.windowManager (mesoPlanEditorModal r r.windowManager plan)

    update (DeleteLesson lessonId) = M.io_ $
      modifySyncDocument r (Lessons $ OnLessons $ Delete lessonId)

    update DeleteMesoPlan = do
      m <- M.get
      M.io_ $ modifySyncDocument r (MesoPlans $ OnMesoPlans $ Delete m.mesoPlan.id)

    update (PinLessonEvaluation lesson) = M.io_ $
      let pinTitle = C.translate' C.LblLessonEvaluation
            <> ": " <> M.ms lesson.title
            <> maybe "" (\d -> ", " <> C.formatDay d) lesson.date
       in pinDialog r.windowManager
            (PinId $ "lesson-evaluation-" <> idToText lesson.id)
            (AnyPinnedDialog (lessonEvaluatorComponent r lesson) Icon.IcnMesoPlan pinTitle)

    update (PinAssignmentEvaluation assignment) = M.io_ $
      let AssignmentName nameText = assignment.name
          pinTitle = C.translate' C.LblEvaluateAssignment
            <> ": " <> M.ms nameText
       in pinDialog r.windowManager
            (PinId $ "assignment-evaluation-" <> idToText assignment.id)
            (AnyPinnedDialog (evaluatorComponent r assignment) Icon.IcnAssignment pinTitle)

    view m =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ -- Plan header with title, dates, edit and delete buttons
          MH.div_
            [class_ "flex items-center justify-between p-3 bg-muted/30 rounded-lg mb-2"]
            [ MH.div_
                [class_ "flex flex-col gap-1"]
                [ Typography.h2 $ M.ms $ if Text.null m.mesoPlan.title then "(Untitled)" else m.mesoPlan.title
                , let dr = DateDisplay.formatDateRange m.mesoPlan.dateFrom m.mesoPlan.dateTo
                   in if dr == ""
                        then M.text ""
                        else MH.span_ [class_ "text-sm text-muted-foreground"] [M.text dr]
                ]
            , MH.div_
                [class_ "flex gap-1"]
                [ Button.ghostSm (Button.button Icon.IcnEdit (OpenMesoPlanEditorModal m.mesoPlan))
                , Button.destructiveSm (Button.button Icon.IcnDelete DeleteMesoPlan)
                ]
            ]
        , MH.div_
            [class_ "flex flex-col gap-2 w-full"]
            (map (viewLesson m) m.lessons)
        , MH.div_
            [class_ "flex gap-2"]
            [ Button.primary (Button.button (Icon.IcnAdd, C.LblAddLesson) CreateNewLesson)
            ]
        ]

    viewLesson m lesson =
      let isExpanded = m.expandedLessonId == Just lesson.id
       in Disclosure.collapsibleWithActions isExpanded (ToggleLessonExpansion lesson.id)
            -- Title
            ( MH.div_
                []
                [ MH.div_
                    [class_ "font-medium"]
                    [M.text $ M.ms $ if Text.null lesson.title then "(Untitled)" else lesson.title]
                , if lesson.description == mempty
                    then M.text ""
                    else
                      MH.div_
                        [class_ "text-sm text-muted-foreground"]
                        [renderRichText lesson.description]
                ]
            )
            -- Actions
            [ Button.ghostSm (Button.button Icon.IcnPin (PinLessonEvaluation lesson))
            , Button.ghostSm (Button.button Icon.IcnEdit (OpenLessonEditorModal lesson))
            , Button.destructiveSm (Button.button Icon.IcnDelete (DeleteLesson lesson.id))
            ]
            -- Content
            (viewExpandedLesson m lesson)

    viewExpandedLesson m lesson =
      let lessonAssignmentIds = map (.id) $ Ix.toList $ m.document.assignments Ix.@= lesson.id
       in MH.div_
        [class_ "p-4 border-t border-border bg-background space-y-3"]
        [ -- Date
          case lesson.date of
            Nothing -> M.text ""
            Just d ->
              MH.div_
                [class_ "text-sm text-muted-foreground"]
                [M.text $ C.translate' C.LblLessonDate <> ": " <> C.formatDay d]
        , -- Assignments collapsible
          if null lessonAssignmentIds
            then M.text ""
            else
              MH.nodeHtml "details"
                [class_ "border border-border rounded-md"]
                [ MH.nodeHtml "summary"
                    [class_ "cursor-pointer p-2 text-sm font-medium text-muted-foreground hover:bg-muted/50 rounded-md"]
                    [M.text $ C.translate' C.LblLessonAssignments <> " (" <> M.ms (show (length lessonAssignmentIds)) <> ")"]
                , MH.div_
                    [class_ "p-2 pt-0 space-y-1"]
                    (map (viewAssignmentSummary m.document) lessonAssignmentIds)
                ]
        , -- Resources (shown directly, each individually expandable)
          let resolvedResources = mapMaybe (\rId -> Ix.getOne (m.document.resources Ix.@= rId)) lesson.resources
           in if null resolvedResources
                then M.text ""
                else
                  MH.div_
                    []
                    [ Typography.h4 (C.translate' C.LblLessonResources)
                    , ResourceList.resourcesListView resolvedResources m.expandedResources ToggleResourceExpanded
                    ]
        , -- Notes preview
          if lesson.notes == mempty
            then M.text ""
            else
              MH.div_
                []
                [ MH.div_
                    [class_ "text-sm font-medium text-muted-foreground mb-1"]
                    [M.text $ C.translate' C.LblLessonNotes]
                , MH.div_
                    [class_ "bg-muted/50 rounded-md p-3 text-sm"]
                    [renderRichText lesson.notes]
                ]
        , -- Phases summary
          if null lesson.phases
            then M.text ""
            else
              MH.div_
                []
                [ Typography.h4 (C.translate' C.LblLessonPhases)
                , MH.div_
                    [class_ "space-y-1"]
                    (zipWith viewPhaseSummary [1 :: Int ..] lesson.phases)
                ]
        ]

    viewPhaseSummary idx phase =
      let borderColor = case phase.actionForm of
            Presenting -> "border-l-red-500"
            Collaborating -> "border-l-orange-500"
            Assigning -> "border-l-green-500"
       in MH.div_
            [class_ $ "flex items-center gap-2 text-sm p-2 bg-muted/30 rounded border-l-4 " <> borderColor]
            [ MH.span_ [class_ "font-medium"]
                [M.text $ M.ms $ if Text.null phase.title then "Phase " <> Text.pack (show idx) else phase.title]
            , MH.span_ [class_ "text-muted-foreground"]
                [ M.text $ M.ms (show phase.duration) <> " min"
                , M.text " · "
                , M.text $ C.translate' (C.LblTeachingSocialForm phase.socialForm)
                , M.text " · "
                , M.text $ C.translate' (C.LblActionForm phase.actionForm)
                ]
            ]

    viewAssignmentSummary doc aId =
      case Ix.getOne (doc.assignments Ix.@= aId) of
        Nothing -> MH.div_ [class_ "text-sm text-muted-foreground italic"] [M.text "(Unknown assignment)"]
        Just a ->
          let AssignmentName nameText = a.name
           in MH.div_
                [class_ "flex items-center justify-between text-sm p-1 rounded hover:bg-muted/30"]
                [ M.text $ M.ms nameText
                , Button.ghostSm (Button.button Icon.IcnApply (PinAssignmentEvaluation a))
                ]

