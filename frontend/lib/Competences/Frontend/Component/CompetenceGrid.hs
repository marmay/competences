module Competences.Frontend.Component.CompetenceGrid
  ( competenceGridComponent
  , CompetenceGridMode (..)
  )
where

import Competences.Document (CompetenceGrid (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.Assessment (assessmentDetailView)
import Competences.Frontend.Component.CompetenceGrid.Editor (editorDetailView)
import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode (..))
import Competences.Frontend.Component.CompetenceGrid.Viewer (viewerDetailView)
import Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridSelectorComponent
  )
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument (SyncContext)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty)
import Miso qualified as M

-- ============================================================================
-- MAIN COMPONENT
-- ============================================================================

-- | Competence grid component with view/edit mode switching
--
-- Uses SelectorDetail to provide:
-- - Selector on left (with create button for teachers)
-- - Mode switcher when multiple modes available
-- - View mode: displays competence grid with student evidence
-- - Edit mode: allows editing grid and competences
-- - Assessment mode: allows assessing student competences
competenceGridComponent
  :: SyncContext
  -> CompetenceGridMode
  -- ^ Initial mode (GridView or GridEdit)
  -> NonEmpty CompetenceGridMode
  -- ^ Available modes (for role-based filtering)
  -> M.Component p (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
competenceGridComponent r initialMode availableModes =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "competence-grid"
      , SD.selectorComponent = \sel ->
          -- Use create style if edit mode is available, otherwise view-only
          let style =
                if GridEdit `elem` availableModes
                  then CompetenceGridSelectorViewAndCreateStyle
                  else CompetenceGridSelectorViewOnlyStyle
           in competenceGridSelectorComponent r style sel
      , SD.detailView = \mode grid ->
          case mode of
            GridView -> viewerDetailView r grid
            GridEdit -> editorDetailView r grid
            GridAssessment -> assessmentDetailView r grid
      , SD.modeLabel = \case
          GridView -> C.translate' C.LblView
          GridEdit -> C.translate' C.LblEdit
          GridAssessment -> C.translate' C.LblAssess
      , SD.modeIcon = \case
          GridView -> Just IcnView
          GridEdit -> Just IcnEdit
          GridAssessment -> Just IcnApply
      , SD.availableModes = availableModes
      , SD.defaultMode = initialMode
      , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
      }
