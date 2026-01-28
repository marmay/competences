-- |
-- Module      : Competences.Frontend.Component.CompetenceGrid.PhaseEditorModal
-- Description : Modal component for editing a LessonPhase
--
-- Used via the central ModalManager to edit a single phase of a lesson plan.
module Competences.Frontend.Component.CompetenceGrid.PhaseEditorModal
  ( phaseEditorModal
  )
where

import Competences.Command (Command (..), LessonPlansCommand (..), LessonPlanPatch (..), EntityCommand (..), ModifyCommand (..))
import Competences.Document.LessonPlan (LessonPlan (..), LessonPhase (..), TeachingSocialForm (..), ActionForm (..))
import Competences.Frontend.Component.TaskContentView (renderRichText)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument)
import Competences.Frontend.SyncContext.ModalManager (ModalManagerRef, closeModal)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (def)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (?~))
import Text.Read (readMaybe)

-- ============================================================================
-- Model
-- ============================================================================

-- | Internal model for the phase editor modal
data Model = Model
  { lessonPlan :: !LessonPlan
  , phaseIndex :: !Int
  , titleValue :: !Text
  , durationValue :: !Int
  , socialFormValue :: !TeachingSocialForm
  , actionFormValue :: !ActionForm
  , notesValue :: !Text
  , modalManager :: !ModalManagerRef
  , syncContext :: !SyncContext
  }
  deriving (Generic)

-- Manual Eq instance since ModalManagerRef and SyncContext don't have Eq
instance Eq Model where
  m1 == m2 =
    m1.lessonPlan == m2.lessonPlan
      && m1.phaseIndex == m2.phaseIndex
      && m1.titleValue == m2.titleValue
      && m1.durationValue == m2.durationValue
      && m1.socialFormValue == m2.socialFormValue
      && m1.actionFormValue == m2.actionFormValue
      && m1.notesValue == m2.notesValue

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = SetTitle !Text
  | SetDuration !Int
  | SetSocialForm !TeachingSocialForm
  | SetActionForm !ActionForm
  | SetNotes !Text
  | SaveAndClose
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create the phase editor modal component
phaseEditorModal :: SyncContext -> ModalManagerRef -> LessonPlan -> Int -> M.Component p Model Action
phaseEditorModal r modalMgr plan phaseIdx =
  M.component model update view
  where
    phase = plan.phases !! phaseIdx

    model =
      Model
        { lessonPlan = plan
        , phaseIndex = phaseIdx
        , titleValue = phase.title
        , durationValue = phase.duration
        , socialFormValue = phase.socialForm
        , actionFormValue = phase.actionForm
        , notesValue = phase.notes
        , modalManager = modalMgr
        , syncContext = r
        }

    update (SetTitle t) =
      M.modify $ \m -> m {titleValue = t}

    update (SetDuration d) =
      M.modify $ \m -> m {durationValue = d}

    update (SetSocialForm sf) =
      M.modify $ \m -> m {socialFormValue = sf}

    update (SetActionForm af) =
      M.modify $ \m -> m {actionFormValue = af}

    update (SetNotes n) =
      M.modify $ \m -> m {notesValue = n}

    update SaveAndClose = do
      m <- M.get
      M.io_ $ do
        let oldPhases = m.lessonPlan.phases
            newPhase = LessonPhase
              { title = m.titleValue
              , duration = m.durationValue
              , socialForm = m.socialFormValue
              , actionForm = m.actionFormValue
              , notes = m.notesValue
              }
            newPhases = updateAt m.phaseIndex (const newPhase) oldPhases
            patch = def & #phases ?~ (oldPhases, newPhases)
        modifySyncDocument m.syncContext (LessonPlans $ OnLessonPlans $ Modify m.lessonPlan.id Lock)
        modifySyncDocument m.syncContext (LessonPlans $ OnLessonPlans $ Modify m.lessonPlan.id (Release patch))
        closeModal m.modalManager

    update CloseModal = do
      m <- M.get
      M.io_ $ closeModal m.modalManager

    view :: Model -> M.View Model Action
    view m =
      MH.div_
        [ class_ "bg-popover text-popover-foreground rounded-xl shadow-lg"
        , class_ "w-[900px] max-w-[95vw] flex flex-col"
        ]
        [ -- Header with title and close button
          MH.div_
            [class_ "flex items-center justify-between border-b px-6 py-4"]
            [ Typography.h3 $ C.translate' C.LblPhaseTitle
            , MH.button_
                [ class_ "text-muted-foreground hover:text-foreground transition-colors"
                , MH.onClick CloseModal
                ]
                [icon [MP.width_ "20", MP.height_ "20"] IcnCancel]
            ]
        , -- Form content
          MH.div_
            [class_ "px-6 py-4 space-y-4"]
            [ -- Title input
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseTitle]
                , MH.input_
                    [ MP.type_ "text"
                    , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                    , MP.value_ (M.ms m.titleValue)
                    , MH.onInput (SetTitle . M.fromMisoString)
                    , MP.autofocus_ True
                    ]
                ]
            , -- Duration input
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseDuration]
                , MH.input_
                    [ MP.type_ "number"
                    , class_ "mt-1 w-24 px-3 py-2 border border-input rounded-md bg-background"
                    , MP.value_ (M.ms $ show m.durationValue)
                    , MH.onInput (SetDuration . maybe m.durationValue id . readMaybe . M.fromMisoString)
                    ]
                ]
            , -- Social form selector
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseSocialForm]
                , MH.select_
                    [ class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                    , MH.onChange (SetSocialForm . toEnum . maybe 0 id . readMaybe . M.fromMisoString)
                    ]
                    (map (socialFormOption m.socialFormValue) [minBound .. maxBound])
                ]
            , -- Action form selector
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblPhaseActionForm]
                , MH.select_
                    [ class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                    , MH.onChange (SetActionForm . toEnum . maybe 0 id . readMaybe . M.fromMisoString)
                    ]
                    (map (actionFormOption m.actionFormValue) [minBound .. maxBound])
                ]
            , -- Notes with split-panel (editor + preview)
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium mb-2 block"] [M.text $ C.translate' C.LblPhaseNotes]
                , MH.div_
                    [class_ "flex gap-4"]
                    [ -- Left: Markup editor
                      MH.div_
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
                    , -- Right: Live preview
                      MH.div_
                        [class_ "flex-1"]
                        [ MH.label_
                            [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                            [M.text $ C.translate' C.LblPreview]
                        , MH.div_
                            [class_ "min-h-[120px] p-3 border border-input rounded-md bg-muted/50"]
                            [renderRichText m.notesValue]
                        ]
                    ]
                ]
            ]
        , -- Footer with action buttons
          MH.div_
            [class_ "flex justify-end gap-2 border-t px-6 py-4"]
            [ Button.buttonSecondary (C.translate' C.LblCancel)
                & Button.withClick CloseModal
                & Button.renderButton
            , Button.buttonPrimary (C.translate' C.LblSave)
                & Button.withClick SaveAndClose
                & Button.renderButton
            ]
        ]

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

-- | Update element at index
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs =
  let (before, after) = splitAt idx xs
   in case after of
        [] -> before
        (x : rest) -> before <> [f x] <> rest
