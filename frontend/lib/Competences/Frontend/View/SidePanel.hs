module Competences.Frontend.View.SidePanel
  ( SidePanelState (..)
  , sidePanel
  , mounted
  , mounted'
  )
where

import Competences.Frontend.View.Tailwind qualified as T
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M

data SidePanelState
  = MenuPanel
  | EditorPanel
  | LargePanel
  deriving (Eq, Show)

sidePanel
  :: SidePanelState
  -> [M.View model action]
  -> M.View model action
sidePanel state = M.div_ [T.tailwind (tailwindStyleFor state)]
  where
    tailwindStyleFor MenuPanel = []
    tailwindStyleFor EditorPanel = []
    tailwindStyleFor LargePanel = []

mounted
  :: (Eq childModel) => M.MisoString -> M.Component model childModel childAction -> M.View model action
mounted id' c = M.div_ [M.id_ id'] M.+> c

mounted'
  :: (Eq childModel) => M.Component model childModel childAction -> M.View model action
mounted' c = M.div_ [] M.+> c
