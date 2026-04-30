module Competences.Frontend.Page.Import
  ( importPage
  )
where

import Competences.Frontend.Component.ImportModal (Action, Model, importModalComponent)
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.SyncContext.WindowManager (inlineMode)
import Miso qualified as M

importPage :: SyncContext -> M.Component p Model Action
importPage r = importModalComponent r inlineMode
