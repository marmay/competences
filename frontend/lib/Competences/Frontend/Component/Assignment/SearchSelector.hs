-- | Combobox-style search selector for assignments — used by the
-- Editor framework to render an assignment-pick form field (e.g.
-- the assignment slot in the Evidence editor).
--
-- A different selector pattern from 'Component.Assignment.ListSelector':
-- this one is a searchable single-select combobox with its own
-- read-only viewer companion, plumbed into the Editor framework via
-- 'SelectorTransformedLens'. Different projection (just the list of
-- assignments) and different binding shape from the list selector.
module Competences.Frontend.Component.Assignment.SearchSelector
  ( searchableSingleAssignmentEditorField
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), AssignmentIxs, Document (..), User (..))
import Competences.Document.Assignment (AssignmentName (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, currentValue, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), SelectorTransformedLens (..), mkSelectorBinding)
import Competences.Frontend.SyncContext
  ( ChangeInfo (..)
  , ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (Default)
import Data.List (find, sortOn)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (fromMisoString, ms)
import Optics.Core (castOptic, (&), (.~), (^.), (%~))

-- ---------------------------------------------------------------------------
-- Public entry: editor-field wrapper
-- ---------------------------------------------------------------------------

-- | Searchable single-assignment editor field for use in editors.
searchableSingleAssignmentEditorField
  :: (Eq t, Ord p, Default patch)
  => SyncContext
  -> M.MisoString
  -> EntityPatchTransformedLens p patch Maybe Assignment Maybe t
  -> EditorField p patch f
searchableSingleAssignmentEditorField r k eptl =
  let conf currentAssignmentId =
        AssignmentEditorConfig
          { isInitialAssignment = \a -> currentAssignmentId == Just (eptl.transform a)
          }
   in selectorEditorFieldWithViewer
        k
        eptl
        (\e -> selectedAssignmentViewerComponent r (conf (e ^. eptl.viewLens)))
        ( \e p ->
            searchableSingleAssignmentSelectorComponent
              r
              (conf (currentValue e p eptl.viewLens eptl.patchLens))
        )

-- ---------------------------------------------------------------------------
-- Shared internals
-- ---------------------------------------------------------------------------

newtype AssignmentEditorConfig = AssignmentEditorConfig
  { isInitialAssignment :: Assignment -> Bool
  }

data EditorFieldProjection = EditorFieldProjection
  { assignments :: ![Assignment]
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

editorFieldProjection :: Document -> Maybe User -> EditorFieldProjection
editorFieldProjection doc mUser =
  let allAssignments :: Ix.IxSet AssignmentIxs Assignment
      allAssignments = doc.assignments
      filtered = case mUser of
        Nothing -> Ix.toList allAssignments
        Just u -> Ix.toList (allAssignments Ix.@= u.id)
   in EditorFieldProjection
        { assignments = sortOn (.assignmentDate) filtered
        , focusedUser = mUser
        }

unName :: AssignmentName -> Text
unName (AssignmentName t) = t

-- ---------------------------------------------------------------------------
-- Read-only viewer
-- ---------------------------------------------------------------------------

data SelectedAssignmentViewerModel = SelectedAssignmentViewerModel
  { possibleValues :: ![Assignment]
  , selectedValue :: !(Maybe Assignment)
  }
  deriving (Eq, Generic, Show)

newtype SelectedAssignmentViewerAction
  = AssignmentViewerProjectionChanged (ProjectedChange EditorFieldProjection)
  deriving (Eq, Show)

selectedAssignmentViewerComponent
  :: SyncContext
  -> AssignmentEditorConfig
  -> SelectorTransformedLens p Maybe Assignment f t
  -> M.Component p SelectedAssignmentViewerModel SelectedAssignmentViewerAction
selectedAssignmentViewerComponent r conf lensBinding =
  (M.component model0 update0 view0)
    { M.bindings = [mkSelectorBinding lensBinding (castOptic #selectedValue)]
    , M.subs = [subscribeWithProjection r editorFieldProjection AssignmentViewerProjectionChanged]
    }
  where
    model0 = SelectedAssignmentViewerModel{possibleValues = [], selectedValue = Nothing}

    update0 (AssignmentViewerProjectionChanged change) =
      M.modify $ \m ->
        let newPossibleValues = change.projection.assignments
            newSelectedValue
              | change.changeInfo == InitialSnapshot =
                  case filter conf.isInitialAssignment newPossibleValues of
                    (a : _) -> Just a
                    [] -> Nothing
              | otherwise =
                  m.selectedValue >>= \sel -> find (\a -> a.id == sel.id) newPossibleValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValue .~ newSelectedValue)

    view0 m = viewSelectedAssignment m.selectedValue

viewSelectedAssignment :: Maybe Assignment -> M.View m a
viewSelectedAssignment = \case
  Nothing -> Typography.muted (C.translate' C.LblNoAssignmentSelected)
  Just a ->
    M.span_
      []
      [M.text $ ms $ unName a.name <> " (" <> T.pack (show $ C.formatDay a.assignmentDate) <> ")"]

-- ---------------------------------------------------------------------------
-- Searchable single-select combobox
-- ---------------------------------------------------------------------------

data AssignmentSelectorModel = AssignmentSelectorModel
  { possibleValues :: ![Assignment]
  , selectedValue :: !(Maybe Assignment)
  , searchQuery :: !Text
  , isOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data AssignmentSelectorAction
  = SelectorProjectionChanged !(ProjectedChange EditorFieldProjection)
  | SelectorToggle !Assignment
  | SelectorSetSearchQuery !Text
  | SelectorSetOpen !Bool
  deriving (Eq, Show)

searchableSingleAssignmentSelectorComponent
  :: SyncContext
  -> AssignmentEditorConfig
  -> SelectorTransformedLens p Maybe Assignment f t
  -> M.Component p AssignmentSelectorModel AssignmentSelectorAction
searchableSingleAssignmentSelectorComponent r conf lensBinding =
  (M.component model0 update0 view0)
    { M.bindings = [mkSelectorBinding lensBinding #selectedValue]
    , M.subs = [subscribeWithProjection r editorFieldProjection SelectorProjectionChanged]
    }
  where
    model0 =
      AssignmentSelectorModel
        { possibleValues = []
        , selectedValue = Nothing
        , searchQuery = ""
        , isOpen = False
        }

    update0 (SelectorProjectionChanged change) =
      M.modify $ \m ->
        let newPossibleValues = change.projection.assignments
            newSelectedValue
              | change.changeInfo == InitialSnapshot = find conf.isInitialAssignment newPossibleValues
              | otherwise = m.selectedValue >>= \v -> find (\a -> a.id == v.id) newPossibleValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValue .~ newSelectedValue)
    update0 (SelectorToggle a) =
      M.modify $ \m ->
        m
          & (#selectedValue %~ \s -> if (fmap (.id) s) == Just a.id then Nothing else Just a)
          & (#isOpen .~ False)
    update0 (SelectorSetSearchQuery q) =
      M.modify (#searchQuery .~ q)
    update0 (SelectorSetOpen open) =
      M.modify (#isOpen .~ open)

    view0 m =
      let options =
            map
              (\v -> Combobox.ComboboxOption v (fromMisoString $ showAssignment v))
              m.possibleValues
          selectedSet = maybe Set.empty Set.singleton m.selectedValue
          displayTxt = fmap (fromMisoString . showAssignment) m.selectedValue
       in Combobox.singleSelectCombobox
            SelectorSetSearchQuery
            SelectorToggle
            SelectorSetOpen
            & Combobox.withPlaceholder (fromMisoString $ C.translate' C.LblSelectAssignment)
            & Combobox.withOptions options
            & Combobox.withSelected selectedSet
            & Combobox.withDisplayText displayTxt
            & Combobox.withSearchQuery m.searchQuery
            & Combobox.withIsOpen m.isOpen
            & Combobox.renderCombobox

    showAssignment a = ms $ unName a.name <> " (" <> T.pack (show $ C.formatDay a.assignmentDate) <> ")"
