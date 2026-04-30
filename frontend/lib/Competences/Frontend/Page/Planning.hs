module Competences.Frontend.Page.Planning
  ( planningPage
  , PlanningMode (..)
  )
where

import Competences.Document (MesoPlan (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Planning.DetailView (detailView)
import Competences.Frontend.Component.MesoPlan.ListSelector (mesoPlanListSelectorComponent)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext (..), SyncDocumentEnv (..))
import Competences.Query.DefaultSelection qualified as QDefault
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Miso qualified as M

-- | Mode for the planning component
-- Currently only Edit mode is supported (teachers only)
data PlanningMode = PlanningEdit
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Planning component using SelectorDetail pattern
-- Teachers: Edit meso plans and their entries
planningPage
  :: SyncContext
  -> M.Component p (SD.Model MesoPlan PlanningMode) (SD.Action PlanningMode)
planningPage r =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "planning"
      , SD.selectorComponent = mesoPlanListSelectorComponent r
          (Just $ QDefault.defaultMesoPlan r.env.currentDay)
      , SD.detailView = \mode plan -> case mode of
          PlanningEdit -> detailView r plan
      , SD.modeLabel = \case
          PlanningEdit -> C.translate' C.LblEdit
      , SD.modeIcon = \case
          PlanningEdit -> Just Icon.IcnEdit
      , SD.availableModes = PlanningEdit :| []
      , SD.defaultMode = PlanningEdit
      , SD.emptyView = Typography.muted (C.translate' C.LblSelectMesoPlan)
      }
