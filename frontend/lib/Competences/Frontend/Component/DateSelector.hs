module Competences.Frontend.Component.DateSelector
  ( dateSelectorComponent
  )
where

import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Optics.Core (Lens', toLensVL, (.~))

newtype Model = Model {date :: Day}
  deriving (Eq, Generic, Show)

newtype Action = SetDate M.MisoString
  deriving (Eq, Show)

dateSelectorComponent :: Day -> Lens' p Day -> M.Component p Model Action
dateSelectorComponent day parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #date]
    }
  where
    model = Model day

    update (SetDate dateStr) = case parseTimeM False defaultTimeLocale "%Y-%m-%d" (M.fromMisoString dateStr) of
      Just date -> M.modify $ #date .~ date
      Nothing -> pure ()

    view m =
      M.div_ [] [M.input_ [M.type_ "date", M.value_ (M.toMisoString $ show m.date), M.onChange SetDate]]
