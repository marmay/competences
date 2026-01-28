-- |
-- Module      : Competences.Frontend.Component.CompetenceGrid.NotesEditorModal
-- Description : Modal component for editing LessonPlan notes
--
-- Used via the central ModalManager to edit notes of a lesson plan.
module Competences.Frontend.Component.CompetenceGrid.NotesEditorModal
  ( notesEditorModal
  )
where

import Competences.Command (Command (..), LessonPlansCommand (..), LessonPlanPatch (..), EntityCommand (..), ModifyCommand (..))
import Competences.Document.LessonPlan (LessonPlan (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.TaskContentView (renderRichText)
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

-- ============================================================================
-- Model
-- ============================================================================

-- | Internal model for the notes editor modal
data Model = Model
  { lessonPlan :: !LessonPlan
  , notesValue :: !Text
  , modalManager :: !ModalManagerRef
  , syncContext :: !SyncContext
  }
  deriving (Generic)

-- Manual Eq instance since ModalManagerRef and SyncContext don't have Eq
instance Eq Model where
  m1 == m2 =
    m1.lessonPlan == m2.lessonPlan
      && m1.notesValue == m2.notesValue

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = SetNotes !Text
  | SaveAndClose
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create the notes editor modal component
notesEditorModal :: SyncContext -> ModalManagerRef -> LessonPlan -> M.Component p Model Action
notesEditorModal r modalMgr plan =
  M.component model update view
  where
    model =
      Model
        { lessonPlan = plan
        , notesValue = plan.notes
        , modalManager = modalMgr
        , syncContext = r
        }

    update (SetNotes n) =
      M.modify $ \m -> m {notesValue = n}

    update SaveAndClose = do
      m <- M.get
      M.io_ $ do
        let oldNotes = m.lessonPlan.notes
            newNotes = m.notesValue
            patch = def & #notes ?~ (oldNotes, newNotes)
        -- Only send command if something changed
        if oldNotes /= newNotes
          then do
            modifySyncDocument m.syncContext (LessonPlans $ OnLessonPlans $ Modify m.lessonPlan.id Lock)
            modifySyncDocument m.syncContext (LessonPlans $ OnLessonPlans $ Modify m.lessonPlan.id (Release patch))
          else pure ()
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
            [ Typography.h3 $ C.translate' C.LblLessonPlanNotes
            , MH.button_
                [ class_ "text-muted-foreground hover:text-foreground transition-colors"
                , MH.onClick CloseModal
                ]
                [icon [MP.width_ "20", MP.height_ "20"] IcnCancel]
            ]
        , -- Form content: split-panel with editor and preview
          MH.div_
            [class_ "px-6 py-4"]
            [ MH.div_
                [class_ "flex gap-4"]
                [ -- Left: Markup editor
                  MH.div_
                    [class_ "flex-1"]
                    [ MH.label_
                        [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                        [M.text "Markup"]
                    , MH.textarea_
                        [ class_ "w-full px-3 py-2 border border-input rounded-md bg-background min-h-[250px] font-mono text-sm"
                        , MP.value_ (M.ms m.notesValue)
                        , MH.onInput (SetNotes . M.fromMisoString)
                        , MP.autofocus_ True
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
                        [class_ "min-h-[250px] p-3 border border-input rounded-md bg-muted/50"]
                        [renderRichText m.notesValue]
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
