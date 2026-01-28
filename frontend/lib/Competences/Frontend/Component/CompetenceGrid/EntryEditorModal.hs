-- |
-- Module      : Competences.Frontend.Component.CompetenceGrid.EntryEditorModal
-- Description : Modal component for editing MesoPlanEntry fields
--
-- Used via the central ModalManager to edit title and description of a meso plan entry.
module Competences.Frontend.Component.CompetenceGrid.EntryEditorModal
  ( entryEditorModal
  )
where

import Competences.Command (Command (..), MesoPlansCommand (..), MesoPlanEntryPatch (..), EntityCommand (..), ModifyCommand (..))
import Competences.Document.MesoPlan (MesoPlanEntry (..))
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

-- ============================================================================
-- Model
-- ============================================================================

-- | Internal model for the entry editor modal
data Model = Model
  { entry :: !MesoPlanEntry
  , titleValue :: !Text
  , descriptionValue :: !Text
  , modalManager :: !ModalManagerRef
  , syncContext :: !SyncContext
  }
  deriving (Generic)

-- Manual Eq instance since ModalManagerRef and SyncContext don't have Eq
instance Eq Model where
  m1 == m2 =
    m1.entry == m2.entry
      && m1.titleValue == m2.titleValue
      && m1.descriptionValue == m2.descriptionValue

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = SetTitle !Text
  | SetDescription !Text
  | SaveAndClose
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create the entry editor modal component
entryEditorModal :: SyncContext -> ModalManagerRef -> MesoPlanEntry -> M.Component p Model Action
entryEditorModal r modalMgr entry' =
  M.component model update view
  where
    model =
      Model
        { entry = entry'
        , titleValue = entry'.title
        , descriptionValue = entry'.description
        , modalManager = modalMgr
        , syncContext = r
        }

    update (SetTitle t) =
      M.modify $ \m -> m {titleValue = t}

    update (SetDescription d) =
      M.modify $ \m -> m {descriptionValue = d}

    update SaveAndClose = do
      m <- M.get
      M.io_ $ do
        let oldTitle = m.entry.title
            newTitle = m.titleValue
            oldDesc = m.entry.description
            newDesc = m.descriptionValue
            -- Build patch with only changed fields
            patch =
              def
                & (if oldTitle /= newTitle then #title ?~ (oldTitle, newTitle) else id)
                & (if oldDesc /= newDesc then #description ?~ (oldDesc, newDesc) else id)
        -- Only send command if something changed
        if oldTitle /= newTitle || oldDesc /= newDesc
          then do
            modifySyncDocument m.syncContext (MesoPlans $ OnMesoPlanEntries $ Modify m.entry.id Lock)
            modifySyncDocument m.syncContext (MesoPlans $ OnMesoPlanEntries $ Modify m.entry.id (Release patch))
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
            [ Typography.h3 $ C.translate' C.LblMesoPlanEntry
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
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblMesoPlanEntryTitle]
                , MH.input_
                    [ MP.type_ "text"
                    , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                    , MP.value_ (M.ms m.titleValue)
                    , MH.onInput (SetTitle . M.fromMisoString)
                    , MP.autofocus_ True
                    ]
                ]
            , -- Description with split-panel (editor + preview)
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium mb-2 block"] [M.text $ C.translate' C.LblMesoPlanEntryDescription]
                , MH.div_
                    [class_ "flex gap-4"]
                    [ -- Left: Markup editor
                      MH.div_
                        [class_ "flex-1"]
                        [ MH.label_
                            [class_ "text-sm font-medium text-muted-foreground mb-1 block"]
                            [M.text "Markup"]
                        , MH.textarea_
                            [ class_ "w-full px-3 py-2 border border-input rounded-md bg-background min-h-[200px] font-mono text-sm"
                            , MP.value_ (M.ms m.descriptionValue)
                            , MH.onInput (SetDescription . M.fromMisoString)
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
                            [class_ "min-h-[200px] p-3 border border-input rounded-md bg-muted/50"]
                            [renderRichText m.descriptionValue]
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
