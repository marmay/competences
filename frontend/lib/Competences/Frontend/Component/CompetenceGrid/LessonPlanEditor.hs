module Competences.Frontend.Component.CompetenceGrid.LessonPlanEditor
  ( lessonPlanEditorView
  )
where

import Competences.Command (Command (..), LessonPlansCommand (..), LessonPlanPatch (..), EntityCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.LessonPlan (LessonPlan (..), LessonPhase (..), TeachingSocialForm (..), ActionForm (..))
import Competences.Document.MesoPlan (MesoPlanEntryId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.NotesEditorModal (notesEditorModal)
import Competences.Frontend.Component.CompetenceGrid.PhaseEditorModal (phaseEditorModal)
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
  deriving (Eq, Show)

-- | Project from document to minimal model
projectLessonPlan :: MesoPlanEntryId -> Document -> LessonPlanModel
projectLessonPlan entryId doc =
  let mPlan = Ix.getOne (doc.lessonPlans Ix.@= entryId)
   in LessonPlanModel mPlan

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
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel = LessonPlanModel Nothing

    update (DocumentUpdated dc) = M.modify $ \_ -> projectLessonPlan entryId dc.document

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
          let newPhase = LessonPhase
                { title = ""
                , socialForm = WholeClass
                , duration = 10
                , actionForm = Presenting
                , notes = ""
                }
              newPhases = plan.phases <> [newPhase]
              newPhaseIndex = length plan.phases  -- Index of the new phase
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

    view m = case m.lessonPlan of
      Nothing -> noPlanView
      Just plan -> planView plan

    noPlanView =
      MH.div_
        [class_ "flex items-center justify-center p-4"]
        [ Button.buttonSecondary (C.translate' C.LblCreateLessonPlan)
            & Button.withIcon IcnAdd
            & Button.withClick CreateLessonPlan
            & Button.renderButton
        ]

    planView plan =
      MH.div_
        [class_ "space-y-4"]
        [ -- Notes section
          notesSection plan
        , -- Phases section
          phasesSection plan
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
            then MH.div_
                   [class_ "text-muted-foreground py-2"]
                   [M.text $ C.translate' C.LblNoNotes]
            else MH.div_
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
            then MH.div_
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
                else MH.div_
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
