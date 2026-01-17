module Competences.Frontend.Component.SolutionEditor
  ( solutionEditorComponent
  , SolutionMode (..)
  )
where

import Competences.Document (Solution (..), User (..))
import Competences.Document.User (isTeacher)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.SolutionSelector (solutionSelectorComponent)
import Competences.Frontend.Component.Solution.EditorDetail (editorDetailView)
import Competences.Frontend.Component.Solution.ViewerDetail (viewerDetailView)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Typography qualified as Typography
import Data.List.NonEmpty (NonEmpty (..))
import Miso qualified as M

-- | Mode for the solution component
-- Teachers have Edit and View modes
-- Students only have View mode
data SolutionMode = SolutionEdit | SolutionView
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Solution component using SelectorDetail pattern
-- Teachers: Edit (default) and View modes
-- Students: View mode only
solutionEditorComponent
  :: SyncContext
  -> User
  -> M.Component p (SD.Model Solution SolutionMode) (SD.Action SolutionMode)
solutionEditorComponent r user =
  SD.selectorDetailComponent
    SD.SelectorDetailConfig
      { SD.selectorId = "solution"
      , SD.selectorComponent = solutionSelectorComponent r
      , SD.detailView = \mode solution -> case mode of
          SolutionEdit -> editorDetailView r solution
          SolutionView -> viewerDetailView r solution
      , SD.modeLabel = \case
          SolutionEdit -> C.translate' C.LblEdit
          SolutionView -> C.translate' C.LblView
      , SD.modeIcon = \case
          SolutionEdit -> Just IcnEdit
          SolutionView -> Just IcnView
      , SD.availableModes = availableModes
      , SD.defaultMode = defaultMode
      , SD.emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
      }
  where
    -- Teachers get all modes, students only get View mode
    availableModes =
      if isTeacher user
        then SolutionEdit :| [SolutionView]
        else SolutionView :| []
    -- Teachers default to Edit mode, students default to View mode
    defaultMode =
      if isTeacher user
        then SolutionEdit
        else SolutionView
