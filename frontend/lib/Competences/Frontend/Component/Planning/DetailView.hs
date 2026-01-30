module Competences.Frontend.Component.Planning.DetailView
  ( detailView
  )
where

import Competences.Command (Command (..), EntityCommand (..), LessonsCommand (..), MesoPlansCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..), Lesson (..), Resource (..))
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Document.Lesson (LessonId, LessonPhase (..))
import Competences.Document.MesoPlan (MesoPlan (..))
import Competences.Document.Order (orderMax)
import Competences.Document.Resource (ResourceIdentifier (..))
import Competences.Query.Lesson qualified as QLesson
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal (mesoPlanEditorModal)
import Competences.Frontend.Component.Planning.LessonEditorModal (lessonEditorModal)
import Competences.Frontend.View.TaskContent (renderRichText)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.ModalManager (openModal)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.DateDisplay qualified as DateDisplay
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
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
  , document :: !Document
  }
  deriving (Eq, Generic, Show)

-- | Actions for the planning component
data DetailAction
  = DocumentUpdated !DocumentChange
  | CreateNewLesson
  | ToggleLessonExpansion !LessonId
  | OpenLessonEditorModal !Lesson
  | OpenMesoPlanEditorModal !MesoPlan
  | DeleteLesson !LessonId
  | DeleteMesoPlan
  deriving (Eq, Show)

-- | Project from document to minimal model, preserving UI state
projectDetail :: MesoPlan -> Maybe LessonId -> Document -> DetailModel
projectDetail plan prevExpanded doc =
  let -- Get fresh plan from document (may have been updated)
      plan' = maybe plan id $ Ix.getOne (doc.mesoPlans Ix.@= plan.id)
      lessons' = QLesson.mesoPlanLessons doc plan'.id
      -- Clear expansion if the lesson no longer exists
      expanded = case prevExpanded of
        Nothing -> Nothing
        Just lid -> if any (\l -> l.id == lid) lessons' then Just lid else Nothing
   in DetailModel plan' lessons' expanded doc

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
    initialModel = DetailModel initialPlan [] Nothing emptyDocument

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

    update (DocumentUpdated dc) = M.modify $ \m -> projectDetail m.mesoPlan m.expandedLessonId dc.document

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
                , description = ""
                , competenceLevels = []
                , date = Nothing
                , assignments = []
                , resources = []
                , phases = []
                , notes = ""
                }
        modifySyncDocument r (Lessons $ OnLessons $ CreateAndLock lesson)
        openModal r.modalManager (lessonEditorModal r r.modalManager lesson)

    update (ToggleLessonExpansion lessonId) = M.modify $ \m ->
      if m.expandedLessonId == Just lessonId
        then m & #expandedLessonId .~ Nothing
        else m & #expandedLessonId .~ Just lessonId

    update (OpenLessonEditorModal lesson) = M.io_ $
      openModal r.modalManager (lessonEditorModal r r.modalManager lesson)

    update (OpenMesoPlanEditorModal plan) = M.io_ $
      openModal r.modalManager (mesoPlanEditorModal r r.modalManager plan)

    update (DeleteLesson lessonId) = M.io_ $
      modifySyncDocument r (Lessons $ OnLessons $ Delete lessonId)

    update DeleteMesoPlan = do
      m <- M.get
      M.io_ $ modifySyncDocument r (MesoPlans $ OnMesoPlans $ Delete m.mesoPlan.id)

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
                [ Button.buttonGhost ""
                    & Button.withIcon IcnEdit
                    & Button.withSize Button.Small
                    & Button.withClick (OpenMesoPlanEditorModal m.mesoPlan)
                    & Button.renderButton
                , Button.buttonDestructive ""
                    & Button.withIcon IcnDelete
                    & Button.withSize Button.Small
                    & Button.withClick DeleteMesoPlan
                    & Button.renderButton
                ]
            ]
        , MH.div_
            [class_ "flex flex-col gap-2 w-full"]
            (map (viewLesson m) m.lessons)
        , MH.div_
            [class_ "flex gap-2"]
            [ Button.buttonPrimary (C.translate' C.LblAddLesson)
                & Button.withIcon IcnAdd
                & Button.withClick CreateNewLesson
                & Button.renderButton
            ]
        ]

    viewLesson m lesson =
      let isExpanded = m.expandedLessonId == Just lesson.id
       in MH.div_
            [class_ "border border-border rounded-lg overflow-hidden"]
            [ -- Lesson header
              MH.div_
                [class_ "flex items-center gap-3 p-3 bg-muted/50"]
                [ -- Chevron and content (clickable to expand)
                  Disclosure.disclosureHeader (ToggleLessonExpansion lesson.id) isExpanded
                    [ -- Lesson title and description
                      MH.div_
                        [class_ "flex-1"]
                        [ MH.div_
                            [class_ "font-medium"]
                            [M.text $ M.ms $ if Text.null lesson.title then "(Untitled)" else lesson.title]
                        , if Text.null lesson.description
                            then M.text ""
                            else MH.div_
                                   [class_ "text-sm text-muted-foreground"]
                                   [renderRichText lesson.description]
                        ]
                    ]
                , -- Edit and delete buttons
                  MH.div_
                    [class_ "flex gap-1"]
                    [ Button.buttonGhost ""
                        & Button.withIcon IcnEdit
                        & Button.withSize Button.Small
                        & Button.withClick (OpenLessonEditorModal lesson)
                        & Button.renderButton
                    , Button.buttonDestructive ""
                        & Button.withIcon IcnDelete
                        & Button.withSize Button.Small
                        & Button.withClick (DeleteLesson lesson.id)
                        & Button.renderButton
                    ]
                ]
            , -- Expanded content (lesson detail)
              if isExpanded
                then viewExpandedLesson m lesson
                else M.text ""
            ]

    viewExpandedLesson m lesson =
      MH.div_
        [class_ "p-4 border-t border-border bg-background space-y-3"]
        [ -- Date
          case lesson.date of
            Nothing -> M.text ""
            Just d ->
              MH.div_
                [class_ "text-sm text-muted-foreground"]
                [M.text $ C.translate' C.LblLessonDate <> ": " <> C.formatDay d]
        , -- Assignments collapsible
          if null lesson.assignments
            then M.text ""
            else
              MH.nodeHtml "details"
                [class_ "border border-border rounded-md"]
                [ MH.nodeHtml "summary"
                    [class_ "cursor-pointer p-2 text-sm font-medium text-muted-foreground hover:bg-muted/50 rounded-md"]
                    [M.text $ C.translate' C.LblLessonAssignments <> " (" <> M.ms (show (length lesson.assignments)) <> ")"]
                , MH.div_
                    [class_ "p-2 pt-0 space-y-1"]
                    (map (viewAssignmentSummary m.document) lesson.assignments)
                ]
        , -- Resources collapsible
          if null lesson.resources
            then M.text ""
            else
              MH.nodeHtml "details"
                [class_ "border border-border rounded-md"]
                [ MH.nodeHtml "summary"
                    [class_ "cursor-pointer p-2 text-sm font-medium text-muted-foreground hover:bg-muted/50 rounded-md"]
                    [M.text $ C.translate' C.LblLessonResources <> " (" <> M.ms (show (length lesson.resources)) <> ")"]
                , MH.div_
                    [class_ "p-2 pt-0 space-y-1"]
                    (map (viewResourceSummary m.document) lesson.resources)
                ]
        , -- Notes preview
          if Text.null lesson.notes
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
                [ MH.div_
                    [class_ "text-sm font-medium text-muted-foreground mb-1"]
                    [M.text $ C.translate' C.LblLessonPhases]
                , MH.div_
                    [class_ "space-y-1"]
                    (zipWith viewPhaseSummary [1 :: Int ..] lesson.phases)
                ]
        ]

    viewPhaseSummary idx phase =
      MH.div_
        [class_ "flex items-center gap-2 text-sm p-2 bg-muted/30 rounded"]
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
                [class_ "text-sm p-1 rounded hover:bg-muted/30"]
                [M.text $ M.ms nameText]

    viewResourceSummary doc rId =
      case Ix.getOne (doc.resources Ix.@= rId) of
        Nothing -> MH.div_ [class_ "text-sm text-muted-foreground italic"] [M.text "(Unknown resource)"]
        Just res ->
          let ResourceIdentifier identText = res.identifier
           in MH.div_
                [class_ "text-sm p-1 rounded hover:bg-muted/30"]
                [M.text $ M.ms identText]
