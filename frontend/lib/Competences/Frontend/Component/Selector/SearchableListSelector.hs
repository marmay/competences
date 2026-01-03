module Competences.Frontend.Component.Selector.SearchableListSelector
  ( -- * Multi-select
    searchableMultiSelectorComponent
  , SearchableModel (..)
  , SearchableAction (..)

    -- * Single-select
  , searchableSingleSelectorComponent
  , SearchableSingleModel (..)
  , SearchableSingleAction (..)
  )
where

import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens, mkSelectorBinding)
import Competences.Frontend.Component.Selector.ListSelector (ListSelectorConfig (..))
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , isInitialUpdate
  , subscribeDocument
  )
import Competences.Frontend.View.Combobox qualified as Combobox
import Data.List (find)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (fromMisoString)
import Optics.Core ((%~), (&), (.~), (^.))
import Optics.Core qualified as O

-- | Model for searchable multi-selector
-- Extends the basic MultiModel with search state
data SearchableModel a = SearchableModel
  { possibleValues :: ![a]
  , selectedValues :: ![a]
  , searchQuery :: !Text
  , isOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- | Actions for searchable multi-selector
data SearchableAction a
  = UpdateDocument !DocumentChange
  | Toggle !a
  | SetSearchQuery !Text
  | SetOpen !Bool
  deriving (Eq, Show)

-- | Searchable multi-select component using combobox UI
-- Follows the same pattern as multiListSelectorComponent but with search capability
searchableMultiSelectorComponent
  :: forall p a f t
   . (Ord a, Show a)
  => SyncDocumentRef
  -> ListSelectorConfig a t
  -- ^ Configuration for getting values from document
  -> SelectorTransformedLens p [] a f t
  -- ^ Lens to bind to parent model
  -> M.Component p (SearchableModel a) (SearchableAction a)
searchableMultiSelectorComponent r config lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding (O.castOptic #selectedValues)]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model =
      SearchableModel
        { possibleValues = []
        , selectedValues = []
        , searchQuery = ""
        , isOpen = False
        }

    update (UpdateDocument (DocumentChange d info)) =
      let newPossibleValues = config.listValues d
       in M.modify $ \m ->
            let newSelectedValues =
                  if isInitialUpdate info
                    then filter config.isInitialValue newPossibleValues
                    else filter (`Set.member` Set.fromList newPossibleValues) (m ^. #selectedValues)
             in m
                  & (#possibleValues .~ newPossibleValues)
                  & (#selectedValues .~ newSelectedValues)
    update (Toggle a) = M.modify $ \m ->
      let selected = Set.fromList m.selectedValues
       in m
            & ( #selectedValues
                  .~ Set.toList
                    ( if a `Set.member` selected
                        then Set.delete a selected
                        else Set.insert a selected
                    )
              )
    update (SetSearchQuery q) = M.modify (#searchQuery .~ q)
    update (SetOpen open) = M.modify (#isOpen .~ open)

    view m =
      let options =
            map
              (\v -> Combobox.ComboboxOption v (fromMisoString $ config.showValue v))
              m.possibleValues
          selectedSet = Set.fromList m.selectedValues
       in Combobox.multiSelectCombobox SetSearchQuery Toggle SetOpen
            & Combobox.withPlaceholder "Auswählen..."
            & Combobox.withOptions options
            & Combobox.withSelected selectedSet
            & Combobox.withSearchQuery m.searchQuery
            & Combobox.withIsOpen m.isOpen
            & Combobox.renderCombobox

-- ============================================================================
-- SINGLE-SELECT
-- ============================================================================

-- | Model for searchable single-selector
data SearchableSingleModel a = SearchableSingleModel
  { possibleValues :: ![a]
  , selectedValue :: !(Maybe a)
  , searchQuery :: !Text
  , isOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- | Actions for searchable single-selector
data SearchableSingleAction a
  = SingleUpdateDocument !DocumentChange
  | SingleToggle !a
  | SingleSetSearchQuery !Text
  | SingleSetOpen !Bool
  deriving (Eq, Show)

-- | Searchable single-select component using combobox UI
-- Follows the same pattern as singleListSelectorComponent but with search capability
searchableSingleSelectorComponent
  :: forall p a f t
   . (Ord a)
  => SyncDocumentRef
  -> ListSelectorConfig a t
  -- ^ Configuration for getting values from document
  -> SelectorTransformedLens p Maybe a f t
  -- ^ Lens to bind to parent model
  -> M.Component p (SearchableSingleModel a) (SearchableSingleAction a)
searchableSingleSelectorComponent r config lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding #selectedValue]
    , M.subs = [subscribeDocument r SingleUpdateDocument]
    }
  where
    model =
      SearchableSingleModel
        { possibleValues = []
        , selectedValue = Nothing
        , searchQuery = ""
        , isOpen = False
        }

    update (SingleUpdateDocument (DocumentChange d info)) =
      let newPossibleValues = config.listValues d
       in M.modify $ \m ->
            let newSelectedValue
                  | isInitialUpdate info = find config.isInitialValue newPossibleValues
                  | otherwise = do
                      v <- m ^. #selectedValue
                      if v `elem` newPossibleValues then Just v else Nothing
             in m
                  & (#possibleValues .~ newPossibleValues)
                  & (#selectedValue .~ newSelectedValue)
    update (SingleToggle a) = do
      M.io_ $ M.consoleLog "SingleToggle"
      M.modify $ \m ->
        m
          & (#selectedValue %~ \s -> if s == Just a then Nothing else Just a)
          & (#isOpen .~ False) -- Close dropdown after selection
    update (SingleSetSearchQuery q) = do
      M.io_ $ M.consoleLog "SingleSetSearchQuery"
      M.modify (#searchQuery .~ q)
    update (SingleSetOpen open) = do
      M.io_ $ M.consoleLog "SingleSetOpen"
      M.modify (#isOpen .~ open)

    view m =
      let options =
            map
              (\v -> Combobox.ComboboxOption v (fromMisoString $ config.showValue v))
              m.possibleValues
          selectedSet = maybe Set.empty Set.singleton m.selectedValue
          displayTxt = fmap (fromMisoString . config.showValue) m.selectedValue
       in Combobox.singleSelectCombobox
            SingleSetSearchQuery
            SingleToggle
            SingleSetOpen
            & Combobox.withPlaceholder "Auswählen..."
            & Combobox.withOptions options
            & Combobox.withSelected selectedSet
            & Combobox.withDisplayText displayTxt
            & Combobox.withSearchQuery m.searchQuery
            & Combobox.withIsOpen m.isOpen
            & Combobox.renderCombobox
