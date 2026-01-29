-- |
-- Module      : Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal
-- Description : Modal component for editing MesoPlan fields
--
-- Used via the central ModalManager to edit title and date range of a meso plan.
module Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal
  ( mesoPlanEditorModal
  )
where

import Competences.Command (Command (..), MesoPlansCommand (..), MesoPlanPatch (..), EntityCommand (..), ModifyCommand (..))
import Competences.Document.MesoPlan (MesoPlan (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument)
import Competences.Frontend.SyncContext.ModalManager (ModalManagerRef, closeModal)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Modal qualified as Modal
import Competences.Frontend.View.Tailwind (class_)
import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (?~))

-- ============================================================================
-- Model
-- ============================================================================

-- | Internal model for the meso plan editor modal
data Model = Model
  { mesoPlan :: !MesoPlan
  , titleValue :: !Text
  , dateFromValue :: !(Maybe Day)
  , dateToValue :: !(Maybe Day)
  , modalManager :: !ModalManagerRef
  , syncContext :: !SyncContext
  }
  deriving (Generic)

-- Manual Eq instance since ModalManagerRef and SyncContext don't have Eq
instance Eq Model where
  m1 == m2 =
    m1.mesoPlan == m2.mesoPlan
      && m1.titleValue == m2.titleValue
      && m1.dateFromValue == m2.dateFromValue
      && m1.dateToValue == m2.dateToValue

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = SetTitle !Text
  | SetDateFrom !(Maybe Day)
  | SetDateTo !(Maybe Day)
  | SaveAndClose
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create the meso plan editor modal component
mesoPlanEditorModal :: SyncContext -> ModalManagerRef -> MesoPlan -> M.Component p Model Action
mesoPlanEditorModal r modalMgr plan =
  M.component model update view
  where
    model =
      Model
        { mesoPlan = plan
        , titleValue = plan.title
        , dateFromValue = plan.dateFrom
        , dateToValue = plan.dateTo
        , modalManager = modalMgr
        , syncContext = r
        }

    update (SetTitle t) =
      M.modify $ \m -> m {titleValue = t}

    update (SetDateFrom d) =
      M.modify $ \m -> m {dateFromValue = d}

    update (SetDateTo d) =
      M.modify $ \m -> m {dateToValue = d}

    update SaveAndClose = do
      m <- M.get
      M.io_ $ do
        let oldTitle = m.mesoPlan.title
            newTitle = m.titleValue
            oldDateFrom = m.mesoPlan.dateFrom
            newDateFrom = m.dateFromValue
            oldDateTo = m.mesoPlan.dateTo
            newDateTo = m.dateToValue
            -- Build patch with only changed fields
            patch =
              def
                & (if oldTitle /= newTitle then #title ?~ (oldTitle, newTitle) else id)
                & (if oldDateFrom /= newDateFrom then #dateFrom ?~ (oldDateFrom, newDateFrom) else id)
                & (if oldDateTo /= newDateTo then #dateTo ?~ (oldDateTo, newDateTo) else id)
        -- Only send command if something changed
        if oldTitle /= newTitle || oldDateFrom /= newDateFrom || oldDateTo /= newDateTo
          then do
            modifySyncDocument m.syncContext (MesoPlans $ OnMesoPlans $ Modify m.mesoPlan.id Lock)
            modifySyncDocument m.syncContext (MesoPlans $ OnMesoPlans $ Modify m.mesoPlan.id (Release patch))
          else pure ()
        closeModal m.modalManager

    update CloseModal = do
      m <- M.get
      M.io_ $ closeModal m.modalManager

    view :: Model -> M.View Model Action
    view m =
      MH.div_
        [ class_ "bg-popover text-popover-foreground rounded-xl shadow-lg"
        , class_ "w-[500px] max-w-[95vw] flex flex-col"
        ]
        [ Modal.modalHeader (C.translate' C.LblEditMesoPlan) CloseModal
        , -- Form content
          MH.div_
            [class_ "px-6 py-4 space-y-4"]
            [ -- Title input
              MH.div_
                []
                [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblMesoPlanTitle]
                , MH.input_
                    [ MP.type_ "text"
                    , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                    , MP.value_ (M.ms m.titleValue)
                    , MH.onInput (SetTitle . M.fromMisoString)
                    , MP.autofocus_ True
                    ]
                ]
            , -- Date range inputs
              MH.div_
                [class_ "flex gap-4"]
                [ -- Date from
                  MH.div_
                    [class_ "flex-1"]
                    [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblMesoPlanDateFrom]
                    , MH.input_
                        [ MP.type_ "date"
                        , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                        , MP.value_ (M.ms $ maybe "" (formatTime defaultTimeLocale "%Y-%m-%d") m.dateFromValue)
                        , MH.onInput (SetDateFrom . parseDate . M.fromMisoString)
                        ]
                    ]
                , -- Date to
                  MH.div_
                    [class_ "flex-1"]
                    [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblMesoPlanDateTo]
                    , MH.input_
                        [ MP.type_ "date"
                        , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                        , MP.value_ (M.ms $ maybe "" (formatTime defaultTimeLocale "%Y-%m-%d") m.dateToValue)
                        , MH.onInput (SetDateTo . parseDate . M.fromMisoString)
                        ]
                    ]
                ]
            ]
        , Modal.modalFooter
            [ Button.buttonSecondary (C.translate' C.LblCancel)
                & Button.withClick CloseModal
                & Button.renderButton
            , Button.buttonPrimary (C.translate' C.LblSave)
                & Button.withClick SaveAndClose
                & Button.renderButton
            ]
        ]

-- | Parse a date string in YYYY-MM-DD format
parseDate :: Text -> Maybe Day
parseDate t
  | Text.null t = Nothing
  | otherwise = parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack t)
