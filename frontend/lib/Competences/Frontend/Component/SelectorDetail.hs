module Competences.Frontend.Component.SelectorDetail
  ( SelectorDetailConfig (..)
  , Model (..)
  , Action (..)
  , selectorDetailComponent
  , emptyModel
  )
where

import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (MisoString)
import Optics.Core (Lens', (&), (.~))

-- | Configuration for a selector-detail component
--
-- Type parameters:
-- * @a@ - The type of items being selected (e.g., CompetenceGrid, Assignment)
-- * @mode@ - Sum type for available detail view modes (e.g., ViewMode | EditMode)
-- * @sm@ - Selector component's internal model type
-- * @sa@ - Selector component's internal action type
data SelectorDetailConfig a mode sm sa = SelectorDetailConfig
  { selectorId :: !MisoString
  -- ^ Unique ID prefix for the selector component
  , selectorComponent :: !(Lens' (Model a mode) (Maybe a) -> M.Component (Model a mode) sm sa)
  -- ^ Selector component factory. Receives a lens to the selection field.
  , detailView :: !(mode -> a -> M.View (Model a mode) (Action mode))
  -- ^ View factory for each mode. The caller should use V.component/V.componentA
  -- to embed the appropriate component for each mode.
  , modeLabel :: !(mode -> MisoString)
  -- ^ Translation function for mode labels (e.g., translate' . LblMode)
  , modeIcon :: !(mode -> Maybe V.Icon)
  -- ^ Optional icon for each mode tab
  , availableModes :: !(NonEmpty mode)
  -- ^ Which modes are available (allows role-based filtering).
  -- NonEmpty ensures at least one mode is always present.
  , defaultMode :: !mode
  -- ^ Initial mode when an item is selected
  , emptyView :: !(M.View (Model a mode) (Action mode))
  -- ^ View to show when nothing is selected
  }
  deriving (Generic)

-- | Model for selector-detail components
data Model a mode = Model
  { selected :: !(Maybe a)
  -- ^ Currently selected item
  , activeMode :: !mode
  -- ^ Currently active detail view mode
  }
  deriving (Eq, Generic, Show)

-- | Actions for selector-detail components
newtype Action mode
  = SwitchMode mode
  -- ^ Switch to a different detail view mode
  deriving (Eq, Show)

-- | Create a selector-detail component
--
-- This creates a component with a selector on the left and detail view on the right.
-- When multiple modes are available, a mode switcher is shown above the detail view.
--
-- Example usage:
--
-- @
-- data GridMode = GridView | GridEdit
--   deriving (Eq, Ord, Enum, Bounded, Show)
--
-- competenceGridComponent :: SyncDocumentRef -> User -> M.Component p (SD.Model CompetenceGrid GridMode) (SD.Action GridMode)
-- competenceGridComponent r user =
--   SD.selectorDetailComponent
--     SD.SelectorDetailConfig
--       { selectorId = "competence-grid"
--       , selectorComponent = gridSelectorComponent r
--       , detailView = \\mode grid ->
--           V.component ("grid-" <> M.ms (show mode))
--             (case mode of
--               GridView -> gridViewerComponent r grid
--               GridEdit -> gridEditorComponent r grid)
--       , modeLabel = \\case
--           GridView -> C.translate' C.LblView
--           GridEdit -> C.translate' C.LblEdit
--       , modeIcon = \\case
--           GridView -> Just V.IcnView
--           GridEdit -> Just V.IcnEdit
--       , availableModes = if isTeacher user then GridView :| [GridEdit] else GridView :| []
--       , defaultMode = GridView
--       , emptyView = Typography.muted (C.translate' C.LblPleaseSelectItem)
--       }
-- @
selectorDetailComponent
  :: forall p a mode sm sa
   . (Eq a, Eq mode, Show mode, Eq sm)
  => SelectorDetailConfig a mode sm sa
  -> M.Component p (Model a mode) (Action mode)
selectorDetailComponent config =
  M.component model update view
  where
    model :: Model a mode
    model = emptyModel config.defaultMode

    update :: Action mode -> M.Effect p (Model a mode) (Action mode)
    update (SwitchMode mode) = M.modify (#activeMode .~ mode)

    view :: Model a mode -> M.View (Model a mode) (Action mode)
    view m =
      V.sideMenu
        ( V.componentA
            config.selectorId
            [class_ "h-full"]
            (config.selectorComponent #selected)
        )
        (detailPanel m)

    detailPanel :: Model a mode -> M.View (Model a mode) (Action mode)
    detailPanel m = case m.selected of
      Nothing -> config.emptyView
      Just item ->
        V.viewFlow
          ( V.vFlow
              & (#expandDirection .~ V.Expand V.Start)
              & (#expandOrthogonal .~ V.Expand V.Center)  -- Center content horizontally
              & (#gap .~ V.MediumSpace)  -- Spacing between mode switcher and content
          )
          [ modeSwitcher m
          , V.flexGrow (config.detailView m.activeMode item)
          ]

    -- Only show switcher if multiple modes available
    -- Uses button group, centered, with spacing below
    modeSwitcher :: Model a mode -> M.View (Model a mode) (Action mode)
    modeSwitcher m = case config.availableModes of
      _ :| [] -> V.empty  -- Single mode, no switcher needed
      modes ->
        V.viewFlow
          ( V.hFlow
              & (#expandDirection .~ V.Expand V.Center)  -- Center the button group
          )
          [Button.buttonGroup (map (modeButton m.activeMode) (NE.toList modes))]

    modeButton :: mode -> mode -> M.View (Model a mode) (Action mode)
    modeButton activeMode mode =
      let variant = if mode == activeMode then Button.Primary else Button.Outline
          baseButton = Button.button variant (config.modeLabel mode)
            & Button.withSize Button.Small
            & Button.withClick (SwitchMode mode)
       in case config.modeIcon mode of
            Nothing -> Button.renderButton baseButton
            Just icon -> Button.renderButton (Button.withIcon icon baseButton)

-- | Create an empty model with the given default mode
emptyModel :: mode -> Model a mode
emptyModel defaultMode =
  Model
    { selected = Nothing
    , activeMode = defaultMode
    }
