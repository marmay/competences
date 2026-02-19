-- |
-- Module      : Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal
-- Description : Modal component for editing MesoPlan fields
--
-- Chrome-free content component. Wrapped by FramedModal when opened as a modal.
module Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal
  ( mesoPlanEditorModal
  )
where

import Competences.Command (Command (..), MesoPlansCommand (..), MesoPlanPatch (..), EntityCommand (..), ModifyCommand (..))
import Competences.Document.MesoPlan (MesoPlan (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Layout qualified as Layout
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
  , onClose :: !(Maybe (IO ()))
  }
  deriving (Generic)

instance Eq Model where
  a == b =
    a.mesoPlan == b.mesoPlan
      && a.titleValue == b.titleValue
      && a.dateFromValue == b.dateFromValue
      && a.dateToValue == b.dateToValue

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = SetTitle !Text
  | SetDateFrom !(Maybe Day)
  | SetDateTo !(Maybe Day)
  | SaveAndClose
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Create the meso plan editor content component.
-- Pass @Just closeAction@ when used in a context that supports programmatic close.
mesoPlanEditorModal :: SyncContext -> Maybe (IO ()) -> MesoPlan -> M.Component p Model Action
mesoPlanEditorModal r mClose plan =
  M.component model update view
  where
    model =
      Model
        { mesoPlan = plan
        , titleValue = plan.title
        , dateFromValue = plan.dateFrom
        , dateToValue = plan.dateTo
        , onClose = mClose
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
            modifySyncDocument r (MesoPlans $ OnMesoPlans $ Modify m.mesoPlan.id Lock)
            modifySyncDocument r (MesoPlans $ OnMesoPlans $ Modify m.mesoPlan.id (Release patch))
          else pure ()
        case m.onClose of
          Just close -> close
          Nothing -> pure ()

    view :: Model -> M.View Model Action
    view m =
      MH.div_
        [class_ "px-6 py-4 space-y-4"]
        [ -- Title input
          MH.div_
            []
            [ MH.label_ [class_ "text-sm font-medium"] [M.text $ C.translate' C.LblEditMesoPlan]
            , MH.input_
                [ MP.type_ "text"
                , class_ "mt-1 w-full px-3 py-2 border border-input rounded-md bg-background"
                , MP.value_ (M.ms m.titleValue)
                , MH.onInput (SetTitle . M.fromMisoString)
                , MP.autofocus_ True
                ]
            ]
        , -- Date range inputs
          Layout.hFlow Layout.gapM
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
        , Layout.actionFooter
            [ Button.applyButton SaveAndClose
            ]
        ]

-- | Parse a date string in YYYY-MM-DD format
parseDate :: Text -> Maybe Day
parseDate t
  | Text.null t = Nothing
  | otherwise = parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack t)
