module Competences.Frontend.Component.CompetenceGrid.LessonPlanEditor
  ( lessonPlanEditorView
  )
where

import Competences.Command (Command (..), EntityCommand (..), LessonPlanPatch (..), LessonPlansCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Assignment (AssignmentId)
import Competences.Document.LessonPlan (ActionForm (..), LessonPhase (..), LessonPlan (..), TeachingSocialForm (..))
import Competences.Document.MesoPlan (MesoPlanEntryId)
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.NotesEditorModal (notesEditorModal)
import Competences.Frontend.Component.CompetenceGrid.PhaseEditorModal (phaseEditorModal)
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.MultiSelectAssignmentSelector (multiSelectAssignmentSelectorComponent)
import Competences.Frontend.Component.Selector.MultiSelectResourceSelector (multiSelectResourceSelectorComponent)
import Competences.Frontend.Component.TaskContentView (renderRichText)
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
import Competences.Frontend.View.Component (componentA)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (def)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~), (?~))

-- ============================================================================
-- LESSON PLAN EDITOR (View mode with modals)
-- ============================================================================

-- | Model for the LessonPlan editor
data LessonPlanModel = LessonPlanModel
  { lessonPlan :: !(Maybe LessonPlan)
  , selectedAssignments :: ![AssignmentId]
  , selectedResources :: ![ResourceId]
  }
  deriving (Eq, Generic, Show)

-- | Actions for the LessonPlan editor
data LessonPlanAction
  = DocumentUpdated !DocumentChange
  | CreateLessonPlan
  | OpenNotesEditorModal !LessonPlan
  | OpenPhaseEditorModal !LessonPlan !Int
  | AddPhase
  | DeletePhase !Int
  | SaveAssignments
  | SaveResources
  deriving (Eq, Show)

-- | Project from document to minimal model
projectLessonPlan :: MesoPlanEntryId -> Document -> LessonPlanModel
projectLessonPlan entryId doc =
  let mPlan = Ix.getOne (doc.lessonPlans Ix.@= entryId)
   in LessonPlanModel
        { lessonPlan = mPlan
        , selectedAssignments = maybe [] (.assignments) mPlan
        , selectedResources = maybe [] (.resources) mPlan
        }

-- | View for the LessonPlan editor
lessonPlanEditorView
  :: SyncContext
  -> MesoPlanEntryId
  -> M.View p a
lessonPlanEditorView r entryId =
  V.component
    ("lesson-plan-editor-" <> M.ms (show entryId))
    (lessonPlanEditorComponent r entryId)

lessonPlanEditorComponent
  :: SyncContext
  -> MesoPlanEntryId
  -> M.Component p LessonPlanModel LessonPlanAction
lessonPlanEditorComponent r entryId =
  (M.component initialModel update (view r))
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel =
      LessonPlanModel
        { lessonPlan = Nothing
        , selectedAssignments = []
        , selectedResources = []
        }

    update (DocumentUpdated dc) = M.modify $ \m ->
      let projected = projectLessonPlan entryId dc.document
       in projected
            -- Preserve selections if they differ from document (user is editing)
            { selectedAssignments =
                if m.selectedAssignments /= maybe [] (.assignments) m.lessonPlan
                  then m.selectedAssignments
                  else projected.selectedAssignments
            , selectedResources =
                if m.selectedResources /= maybe [] (.resources) m.lessonPlan
                  then m.selectedResources
                  else projected.selectedResources
            }

    update CreateLessonPlan = M.io_ $ do
      planId <- nextId r
      let plan =
            LessonPlan
              { id = planId
              , mesoPlanEntryId = entryId
              , date = Nothing
              , assignments = []
              , resources = []
              , phases = []
              , notes = ""
              }
      modifySyncDocument r (LessonPlans $ OnLessonPlans $ CreateAndLock plan)
      -- Auto-open notes editor modal for immediate content entry
      openModal r.modalManager (notesEditorModal r r.modalManager plan)

    update (OpenNotesEditorModal plan) = M.io_ $
      openModal r.modalManager (notesEditorModal r r.modalManager plan)

    update (OpenPhaseEditorModal plan idx) = M.io_ $
      openModal r.modalManager (phaseEditorModal r r.modalManager plan idx)

    update AddPhase = do
      m <- M.get
      M.io_ $ case m.lessonPlan of
        Nothing -> pure ()
        Just plan -> do
          let newPhase =
                LessonPhase
                  { title = ""
                  , socialForm = WholeClass
                  , duration = 10
                  , actionForm = Presenting
                  , notes = ""
                  }
              newPhases = plan.phases <> [newPhase]
              newPhaseIndex = length plan.phases -- Index of the new phase
              patch = def & #phases ?~ (plan.phases, newPhases)
              -- Updated plan for the modal (includes the new phase)
              updatedPlan = plan & #phases .~ newPhases
          modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id Lock)
          modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id (Release patch))
          -- Auto-open phase editor modal for immediate configuration
          openModal r.modalManager (phaseEditorModal r r.modalManager updatedPlan newPhaseIndex)

    update (DeletePhase idx) = do
      m <- M.get
      M.io_ $ case m.lessonPlan of
        Nothing -> pure ()
        Just plan -> do
          let newPhases = deleteAt idx plan.phases
              patch = def & #phases ?~ (plan.phases, newPhases)
          modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id Lock)
          modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id (Release patch))

    update SaveAssignments = do
      m <- M.get
      M.io_ $ case m.lessonPlan of
        Nothing -> pure ()
        Just plan -> do
          let oldAssignments = plan.assignments
              newAssignments = m.selectedAssignments
          if oldAssignments /= newAssignments
            then do
              let patch = def & #assignments ?~ (oldAssignments, newAssignments)
              modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id Lock)
              modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id (Release patch))
            else pure ()

    update SaveResources = do
      m <- M.get
      M.io_ $ case m.lessonPlan of
        Nothing -> pure ()
        Just plan -> do
          let oldResources = plan.resources
              newResources = m.selectedResources
          if oldResources /= newResources
            then do
              let patch = def & #resources ?~ (oldResources, newResources)
              modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id Lock)
              modifySyncDocument r (LessonPlans $ OnLessonPlans $ Modify plan.id (Release patch))
            else pure ()

    view :: SyncContext -> LessonPlanModel -> M.View LessonPlanModel LessonPlanAction
    view syncCtx m = case m.lessonPlan of
      Nothing -> noPlanView
      Just plan -> planView syncCtx m plan

    noPlanView =
      MH.div_
        [class_ "flex items-center justify-center p-4"]
        [ Button.buttonSecondary (C.translate' C.LblCreateLessonPlan)
            & Button.withIcon IcnAdd
            & Button.withClick CreateLessonPlan
            & Button.renderButton
        ]

    planView syncCtx m plan =
      MH.div_
        [class_ "space-y-4"]
        [ -- Assignments section
          assignmentsSection syncCtx m
        , -- Resources section
          resourcesSection syncCtx m
        , -- Notes section
          notesSection plan
        , -- Phases section
          phasesSection plan
        ]

    assignmentsSection syncCtx m =
      let hasChanges = m.selectedAssignments /= maybe [] (.assignments) m.lessonPlan
       in MH.div_
            []
            [ MH.div_
                [class_ "flex items-center justify-between mb-2"]
                [ Typography.h4 (C.translate' C.LblAssignments)
                , if hasChanges
                    then
                      Button.buttonPrimary (C.translate' C.LblSave)
                        & Button.withSize Button.Small
                        & Button.withClick SaveAssignments
                        & Button.renderButton
                    else M.text ""
                ]
            , componentA
                "lesson-plan-assignment-selector"
                []
                ( multiSelectAssignmentSelectorComponent
                    syncCtx
                    (\_ -> m.selectedAssignments)
                    (selectorLens #selectedAssignments)
                )
            ]

    resourcesSection syncCtx m =
      let hasChanges = m.selectedResources /= maybe [] (.resources) m.lessonPlan
       in MH.div_
            []
            [ MH.div_
                [class_ "flex items-center justify-between mb-2"]
                [ Typography.h4 (C.translate' C.LblResources)
                , if hasChanges
                    then
                      Button.buttonPrimary (C.translate' C.LblSave)
                        & Button.withSize Button.Small
                        & Button.withClick SaveResources
                        & Button.renderButton
                    else M.text ""
                ]
            , componentA
                "lesson-plan-resource-selector"
                []
                ( multiSelectResourceSelectorComponent
                    syncCtx
                    (\_ -> m.selectedResources)
                    (selectorLens #selectedResources)
                )
            ]

    notesSection plan =
      MH.div_
        []
        [ MH.div_
            [class_ "flex items-center justify-between mb-2"]
            [ Typography.h4 (C.translate' C.LblLessonPlanNotes)
            , Button.buttonGhost ""
                & Button.withIcon IcnEdit
                & Button.withSize Button.Small
                & Button.withClick (OpenNotesEditorModal plan)
                & Button.renderButton
            ]
        , if Text.null plan.notes
            then
              MH.div_
                [class_ "text-muted-foreground py-2"]
                [M.text $ C.translate' C.LblNoNotes]
            else
              MH.div_
                [class_ "bg-muted/50 rounded-md p-3"]
                [renderRichText plan.notes]
        ]

    phasesSection plan =
      MH.div_
        [class_ "border-t border-border pt-4"]
        [ MH.div_
            [class_ "flex items-center justify-between mb-2"]
            [ Typography.h4 (C.translate' C.LblLessonPlanPhases)
            , Button.buttonSecondary (C.translate' C.LblAddPhase)
                & Button.withIcon IcnAdd
                & Button.withSize Button.Small
                & Button.withClick AddPhase
                & Button.renderButton
            ]
        , if null plan.phases
            then
              MH.div_
                [class_ "text-center text-muted-foreground py-4"]
                [M.text $ C.translate' C.LblNoPhases]
            else viewPhasesList plan
        ]

    viewPhasesList plan =
      MH.div_
        [class_ "space-y-2"]
        (zipWith (viewPhaseCard plan) [0 ..] plan.phases)

    viewPhaseCard :: LessonPlan -> Int -> LessonPhase -> M.View LessonPlanModel LessonPlanAction
    viewPhaseCard plan idx phase =
      MH.div_
        [class_ "flex items-start gap-3 p-3 border border-border rounded-md bg-card"]
        [ -- Phase info
          MH.div_
            [class_ "flex-1"]
            [ MH.div_
                [class_ "font-medium"]
                [M.text $ M.ms $ if Text.null phase.title then "(Phase " <> Text.pack (show (idx + 1)) <> ")" else phase.title]
            , MH.div_
                [class_ "text-sm text-muted-foreground flex gap-3 mt-1"]
                [ MH.span_ [] [M.text $ M.ms $ show phase.duration <> " min"]
                , MH.span_ [] [M.text $ C.translate' (C.LblTeachingSocialForm phase.socialForm)]
                , MH.span_ [] [M.text $ C.translate' (C.LblActionForm phase.actionForm)]
                ]
            , -- Display notes if present
              if Text.null phase.notes
                then M.text ""
                else
                  MH.div_
                    [class_ "mt-2 text-sm bg-muted/50 rounded p-2"]
                    [renderRichText phase.notes]
            ]
        , -- Action buttons
          MH.div_
            [class_ "flex gap-1"]
            [ Button.buttonGhost ""
                & Button.withIcon IcnEdit
                & Button.withSize Button.Small
                & Button.withClick (OpenPhaseEditorModal plan idx)
                & Button.renderButton
            , Button.buttonGhost ""
                & Button.withIcon IcnDelete
                & Button.withSize Button.Small
                & Button.withClick (DeletePhase idx)
                & Button.renderButton
            ]
        ]

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

-- | Delete element at index
deleteAt :: Int -> [a] -> [a]
deleteAt idx xs =
  let (before, after) = splitAt idx xs
   in before <> drop 1 after
