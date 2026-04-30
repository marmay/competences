module Competences.Frontend.Page.CompetenceGrid
  ( competenceGridPage
  , CompetenceGridMode (..)
  )
where

import Competences.Document (CompetenceGrid (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.Assessment (assessmentDetailView)
import Competences.Frontend.Component.CompetenceGrid.Editor (editorDetailView)
import Competences.Frontend.Component.CompetenceGrid.Grading (gradingDetailView)
import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode (..))
import Competences.Frontend.Component.CompetenceGrid.Viewer (viewerDetailView)
import Competences.Frontend.Component.CompetenceGrid.ListSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridListSelectorComponent
  )
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Query.DefaultSelection qualified as QDefault
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Data.List.NonEmpty (NonEmpty)
import Miso qualified as M
import Miso.Html qualified as MH

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
competenceGridPage
  :: SyncContext
  -> CompetenceGridMode
  -- ^ Initial mode (GridView or GridEdit)
  -> NonEmpty CompetenceGridMode
  -- ^ Available modes (for role-based filtering)
  -> M.Component p (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
competenceGridPage r initialMode availableModes =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "competence-grid"
      , SD.selectorComponent = \sel ->
          -- Use create style if edit mode is available, otherwise view-only
          let style =
                if GridEdit `elem` availableModes
                  then CompetenceGridSelectorViewAndCreateStyle
                  else CompetenceGridSelectorViewOnlyStyle
           in competenceGridListSelectorComponent r style (Just QDefault.defaultCompetenceGrid) sel
      , SD.detailView = \mode grid ->
          MH.div_
            [class_ "h-full w-full"]
            [ MH.div_ [class_ "portrait-hide h-full w-full"]
                [ case mode of
                    GridView -> viewerDetailView r grid
                    GridEdit -> editorDetailView r grid
                    GridAssessment -> assessmentDetailView r grid
                    GridGrading -> gradingDetailView r grid
                ]
            , MH.div_
                [class_ "hidden portrait-show items-center justify-center h-full w-full"]
                [ MH.div_
                    [class_ "flex flex-col items-center gap-4 text-muted-foreground"]
                    [ Icon.iconS Icon.XLarge Icon.IcnCompetenceGrid
                    , MH.span_ [class_ "text-sm text-center max-w-64"] [M.text (C.translate' C.LblRotateDevice)]
                    ]
                ]
            ]
      , SD.modeLabel = \case
          GridView -> C.translate' C.LblView
          GridEdit -> C.translate' C.LblEdit
          GridAssessment -> C.translate' C.LblAssess
          GridGrading -> C.translate' C.LblGrade
      , SD.modeIcon = \case
          GridView -> Just Icon.IcnView
          GridEdit -> Just Icon.IcnEdit
          GridAssessment -> Just Icon.IcnApply
          GridGrading -> Just Icon.IcnEvidence
      , SD.availableModes = availableModes
      , SD.defaultMode = initialMode
      , SD.emptyView = Layout.centeredPlaceholder (C.translate' C.LblPleaseSelectItem)
      }
