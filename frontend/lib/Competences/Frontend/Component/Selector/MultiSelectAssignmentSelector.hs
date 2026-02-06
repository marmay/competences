module Competences.Frontend.Component.Selector.MultiSelectAssignmentSelector
  ( multiSelectAssignmentSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Assignment (..), Document (..))
import Competences.Document.Assignment (AssignmentId, AssignmentName (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , mkSelectorBinding
  )
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortOn)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- Projection
-- ============================================================================

-- | Projection from document - all assignments sorted by date,
-- plus a filtered subset of eligible assignments for the dropdown.
data SelectorProjection = SelectorProjection
  { allAssignments :: ![Assignment]
  , eligibleAssignments :: ![Assignment]
  }
  deriving (Eq, Generic, Show)

-- | Projection function - gets all assignments sorted by date
selectorProjection :: (Assignment -> Bool) -> Document -> Maybe user -> SelectorProjection
selectorProjection eligible doc _ =
  let allSorted = sortOn (.assignmentDate) $ Ix.toList doc.assignments
   in SelectorProjection
        { allAssignments = allSorted
        , eligibleAssignments = filter eligible allSorted
        }

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { projection :: !SelectorProjection
  , selectedResults :: ![AssignmentId]
  , searchQuery :: !Text
  , isOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = ProjectionChanged !(ProjectedChange SelectorProjection)
  | SetSearchQuery !Text
  | ToggleAssignment !AssignmentId
  | SetOpen !Bool
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Multi-select assignment selector component
-- Binds selected assignment IDs to parent model via lens.
-- The filter predicate controls which assignments appear in the dropdown.
-- Already-selected assignments are always shown as tags regardless of the filter.
multiSelectAssignmentSelectorComponent
  :: SyncContext
  -> (Assignment -> Bool) -- ^ Filter predicate for eligible assignments
  -> [AssignmentId] -- ^ Initial selection
  -> SelectorTransformedLens p [] AssignmentId f' a'
  -> M.Component p Model Action
multiSelectAssignmentSelectorComponent r eligible initResults lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding #selectedResults]
    , M.subs = [subscribeWithProjection r (selectorProjection eligible) ProjectionChanged]
    }
  where
    model =
      Model
        { projection = SelectorProjection [] []
        , selectedResults = initResults
        , searchQuery = ""
        , isOpen = False
        }

    update (ProjectionChanged change) =
      M.modify $ \m ->
        m & #projection .~ change.projection

    update (SetSearchQuery q) =
      M.modify $ #searchQuery .~ q

    update (ToggleAssignment aId) =
      M.modify $ \m ->
        let current = m.selectedResults
            new =
              if aId `elem` current
                then filter (/= aId) current
                else current <> [aId]
         in m & #selectedResults .~ new

    update (SetOpen open) =
      M.modify $ #isOpen .~ open

    view m =
      MH.div_
        [class_ "space-y-2"]
        [ -- Multi-select combobox
          let filteredAssignments = filterAssignments m.searchQuery m.projection.eligibleAssignments
              options =
                [ Combobox.ComboboxOption a.id (formatAssignment a)
                | a <- filteredAssignments
                ]
              selectedSet = Set.fromList m.selectedResults
           in Combobox.multiSelectCombobox SetSearchQuery ToggleAssignment SetOpen
                & Combobox.withPlaceholder (M.fromMisoString $ C.translate' C.LblSelectAssignments)
                & Combobox.withOptions options
                & Combobox.withSelected selectedSet
                & Combobox.withSearchQuery m.searchQuery
                & Combobox.withIsOpen m.isOpen
                & Combobox.renderCombobox
        , -- Display selected assignments as tags
          if null m.selectedResults
            then M.text ""
            else
              MH.div_
                [class_ "flex flex-wrap gap-2"]
                [ viewAssignmentTag a
                | aId <- m.selectedResults
                , Just a <- [lookupAssignment aId m.projection.allAssignments]
                ]
        ]

    filterAssignments query assignments =
      let q = T.toLower query
       in if T.null q
            then assignments
            else filter (\a -> q `T.isInfixOf` T.toLower (unAssignmentName a.name)) assignments

    formatAssignment a =
      unAssignmentName a.name <> " (" <> T.pack (show $ C.formatDay a.assignmentDate) <> ")"

    unAssignmentName (AssignmentName t) = t

    lookupAssignment aId assignments =
      case filter (\a -> a.id == aId) assignments of
        (a : _) -> Just a
        [] -> Nothing

    viewAssignmentTag :: Assignment -> M.View Model Action
    viewAssignmentTag a =
      Badge.interactive
        Badge.Secondary
        (Just (IcnCancel, ToggleAssignment a.id))
        (IcnAssignment, M.ms $ unAssignmentName a.name)
