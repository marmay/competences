module Competences.Frontend.View.SidePanel
  ( SidePanelState (..)
  , sidePanel
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M

data SidePanelState
  = MenuPanel
  | EditorPanel
  | LargePanel
  deriving (Eq, Show)

sidePanel
  :: SidePanelState
  -> [M.View model action]
  -> M.View model action
sidePanel state = M.div_ [class_ (styleFor state)]
  where
    styleFor MenuPanel = ""
    styleFor EditorPanel = ""
    styleFor LargePanel = ""
