module Competences.Frontend.View.Combobox
  ( -- * Types
    ComboboxOption (..)
  , Combobox (..)

    -- * Constructors
  , multiSelectCombobox
  , singleSelectCombobox

    -- * Builder functions
  , withPlaceholder
  , withOptions
  , withSelected
  , withSearchQuery
  , withOnSearch
  , withOnToggle
  , withOnOpenChange
  , withIsOpen
  , withDisplayText
  , withShowCheckboxes

    -- * Rendering
  , renderCombobox
  )
where

import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, fromMisoString, ms)

-- | A selectable option in the combobox
data ComboboxOption id = ComboboxOption
  { optionId :: !id
  -- ^ Unique identifier for the option
  , optionLabel :: !Text
  -- ^ Display text for the option
  }
  deriving (Eq, Show, Generic)

-- | Combobox configuration (supports single or multi-select)
-- @id@ is the type of option identifiers (must be Ord for Set operations)
-- @a@ is the action type
data Combobox id a = Combobox
  { placeholder :: !Text
  , displayText :: !(Maybe Text)
  -- ^ Override trigger text (for single-select: show selected label)
  , options :: ![ComboboxOption id]
  , selected :: !(Set id)
  , searchQuery :: !Text
  , onSearch :: !(Text -> a)
  , onToggle :: !(id -> a)
  , onOpenChange :: !(Bool -> a)
  , isOpen :: !Bool
  , showCheckboxes :: !Bool
  -- ^ Whether to show checkboxes (True for multi-select, False for single)
  }
  deriving (Generic)

-- | Create a multi-select combobox with default configuration
multiSelectCombobox :: (Text -> a) -> (id -> a) -> (Bool -> a) -> Combobox id a
multiSelectCombobox onSearchAction onToggleAction onOpenAction =
  Combobox
    { placeholder = "Select..."
    , displayText = Nothing
    , options = []
    , selected = Set.empty
    , searchQuery = ""
    , onSearch = onSearchAction
    , onToggle = onToggleAction
    , onOpenChange = onOpenAction
    , isOpen = False
    , showCheckboxes = True
    }

-- | Create a single-select combobox with default configuration
singleSelectCombobox :: (Text -> a) -> (id -> a) -> (Bool -> a) -> Combobox id a
singleSelectCombobox onSearchAction onToggleAction onOpenAction =
  Combobox
    { placeholder = "Select..."
    , displayText = Nothing
    , options = []
    , selected = Set.empty
    , searchQuery = ""
    , onSearch = onSearchAction
    , onToggle = onToggleAction
    , onOpenChange = onOpenAction
    , isOpen = False
    , showCheckboxes = False
    }

-- ============================================================================
-- BUILDER FUNCTIONS
-- ============================================================================

-- | Set placeholder text shown when no items selected
withPlaceholder :: Text -> Combobox id a -> Combobox id a
withPlaceholder p cb = cb{placeholder = p}

-- | Set the available options
withOptions :: [ComboboxOption id] -> Combobox id a -> Combobox id a
withOptions opts cb = cb{options = opts}

-- | Set the currently selected option IDs
withSelected :: Set id -> Combobox id a -> Combobox id a
withSelected sel cb = cb{selected = sel}

-- | Set the current search query
withSearchQuery :: Text -> Combobox id a -> Combobox id a
withSearchQuery q cb = cb{searchQuery = q}

-- | Set the search input handler
withOnSearch :: (Text -> a) -> Combobox id a -> Combobox id a
withOnSearch handler cb = cb{onSearch = handler}

-- | Set the option toggle handler
withOnToggle :: (id -> a) -> Combobox id a -> Combobox id a
withOnToggle handler cb = cb{onToggle = handler}

-- | Set the open/close handler
withOnOpenChange :: (Bool -> a) -> Combobox id a -> Combobox id a
withOnOpenChange handler cb = cb{onOpenChange = handler}

-- | Set whether the dropdown is open
withIsOpen :: Bool -> Combobox id a -> Combobox id a
withIsOpen open cb = cb{isOpen = open}

-- | Set custom display text for trigger button (overrides default count display)
withDisplayText :: Maybe Text -> Combobox id a -> Combobox id a
withDisplayText txt cb = cb{displayText = txt}

-- | Set whether to show checkboxes (True for multi-select, False for single)
withShowCheckboxes :: Bool -> Combobox id a -> Combobox id a
withShowCheckboxes show' cb = cb{showCheckboxes = show'}

-- ============================================================================
-- RENDERING
-- ============================================================================

-- | Render the multi-select combobox
renderCombobox :: (Ord id) => Combobox id a -> M.View m a
renderCombobox cb =
  M.div_
    [class_ "relative"]
    [ triggerButton cb
    , dropdown cb
    ]

-- | The button that opens/closes the dropdown
triggerButton :: Combobox id a -> M.View m a
triggerButton cb =
  M.button_
    [ class_ $
        Text.unwords
          [ "w-full flex items-center justify-between gap-2"
          , "px-3 py-2 rounded-md border border-input bg-background"
          , "text-sm text-foreground"
          , "hover:bg-accent focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2"
          ]
    , MP.type_ "button" -- Prevent form submission behavior
    , M.onClick (cb.onOpenChange (not cb.isOpen))
    , M.textProp "aria-haspopup" "listbox"
    , M.textProp "aria-expanded" (if cb.isOpen then "true" else "false")
    ]
    [ M.span_
        [class_ "truncate"]
        [M.text $ triggerText cb]
    , icon [class_ $ "h-4 w-4 text-muted-foreground " <> if cb.isOpen then "rotate-180" else ""] IcnArrowDown
    ]

-- | Text to show in the trigger button
triggerText :: Combobox id a -> MisoString
triggerText cb = case cb.displayText of
  Just txt -> ms txt
  Nothing ->
    let count = Set.size cb.selected
     in if count == 0
          then ms cb.placeholder
          else ms $ Text.pack (show count) <> " ausgewählt"

-- | The dropdown popover with search and options
-- Uses CSS visibility instead of conditional rendering to ensure
-- event handlers are properly bound when dropdown opens
dropdown :: (Ord id) => Combobox id a -> M.View m a
dropdown cb =
  M.div_
    [ class_ $
        Text.unwords
          [ "absolute z-50 mt-1 w-full"
          , "bg-card border border-border rounded-md shadow-lg"
          , if cb.isOpen then "" else "hidden"
          ]
    , M.textProp "role" "listbox"
    ]
    [ searchInput cb
    , optionsList cb
    ]

-- | Search input field
searchInput :: Combobox id a -> M.View m a
searchInput cb =
  M.div_
    [class_ "p-2 border-b border-border"]
    [ M.input_
        [ class_ $
            Text.unwords
              [ "w-full px-3 py-1.5 rounded-md"
              , "border border-input bg-background text-sm"
              , "focus:outline-none focus:ring-1 focus:ring-ring"
              ]
        , MP.type_ "text"
        , MP.placeholder_ "Suchen..."
        , MP.value_ (ms cb.searchQuery)
        , M.onInput (cb.onSearch . fromMisoString)
        ]
    ]

-- | List of filterable options
optionsList :: (Ord id) => Combobox id a -> M.View m a
optionsList cb =
  let filtered = filterOptions cb.searchQuery cb.options
   in M.div_
        [ class_ "max-h-60 overflow-y-auto p-1"
        ]
        ( if null filtered
            then [emptyState]
            else map (optionItem cb) filtered
        )

-- | Filter options based on search query
filterOptions :: Text -> [ComboboxOption id] -> [ComboboxOption id]
filterOptions query opts
  | Text.null query = opts
  | otherwise = filter (matchesQuery query) opts
  where
    matchesQuery q opt =
      Text.toLower q `Text.isInfixOf` Text.toLower opt.optionLabel

-- | Empty state when no options match
emptyState :: M.View m a
emptyState =
  M.div_
    [class_ "px-3 py-6 text-center text-sm text-muted-foreground"]
    [M.text "Keine Ergebnisse gefunden"]

-- | A single option item (with optional checkbox for multi-select)
optionItem :: (Ord id) => Combobox id a -> ComboboxOption id -> M.View m a
optionItem cb opt =
  let isSelected = Set.member opt.optionId cb.selected
   in M.button_
        [ class_ $
            Text.unwords
              [ "w-full text-left flex items-center gap-2 px-2 py-1.5 rounded-sm cursor-pointer"
              , "hover:bg-accent"
              , if isSelected then "bg-accent/50" else ""
              ]
        , MP.type_ "button"
        , M.onClick (cb.onToggle opt.optionId)
        , M.textProp "role" "option"
        , M.textProp "aria-checked" (if isSelected then "true" else "false")
        ]
        ( (if cb.showCheckboxes then [checkbox isSelected] else [])
            ++ [M.span_ [class_ "text-sm"] [M.text $ ms opt.optionLabel]]
        )

-- | Checkbox indicator
checkbox :: Bool -> M.View m a
checkbox isChecked =
  M.div_
    [ class_ $
        Text.unwords
          [ "h-4 w-4 rounded border flex items-center justify-center"
          , if isChecked
              then "bg-primary border-primary"
              else "border-input bg-background"
          ]
    ]
    [ if isChecked
        then icon [class_ "h-3 w-3 text-primary-foreground"] IcnApply
        else M.text ""
    ]
