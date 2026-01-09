module Competences.Frontend.Component.Selector.UserSelector
  ( UserSelectorConfig (..)
  , singleUserSelectorComponent
  , singleUserEditorField
  , multiUserSelectorComponent
  , multiUserEditorField
  , defaultUserSelectorConfig
  , SingleUserSelectorStyle (..)
  , MultiUserSelectorStyle (..)

    -- * Searchable variants
  , searchableSingleUserSelectorComponent
  , searchableSingleUserEditorField
  , searchableMultiUserSelectorComponent
  , searchableMultiUserEditorField
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorField, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), SelectorTransformedLens (..), mkSelectorBinding)
import Competences.Frontend.Component.Selector.ListSelector qualified as L
import Competences.Frontend.Component.Selector.SearchableListSelector qualified as SL
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncContext, isInitialUpdate, subscribeDocument)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (Default)
import Data.Foldable (toList, find)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~), (^.))
import Optics.Core qualified as O

data SingleUserSelectorStyle
  = SingleUserSelectorStyleButtons
  | SingleUserSelectorStyleComboBox
  | SingleUserSelectorStyleShowOnly
  deriving (Eq, Show, Generic)

data UserSelectorConfig = UserSelectorConfig
  { isPossibleUser :: !(User -> Bool)
  , isInitialUser :: !(User -> Bool)
  }
  deriving (Generic)

defaultUserSelectorConfig :: UserSelectorConfig
defaultUserSelectorConfig =
  UserSelectorConfig
    { isPossibleUser = const True
    , isInitialUser = const False
    }

toListSelectorConfig :: UserSelectorConfig -> L.ListSelectorConfig User f
toListSelectorConfig config =
  (L.listSelectorConfig (listUsers config.isPossibleUser) showUser)
    { L.isInitialValue = config.isInitialUser
    }

toSingleSelectionStyle :: SingleUserSelectorStyle -> L.SingleSelectionStyle
toSingleSelectionStyle SingleUserSelectorStyleButtons = L.SButtons
toSingleSelectionStyle SingleUserSelectorStyleComboBox = L.SComboBox
toSingleSelectionStyle SingleUserSelectorStyleShowOnly = L.SShow

singleUserSelectorComponent
  :: SyncContext
  -> UserSelectorConfig
  -> SingleUserSelectorStyle
  -> SelectorTransformedLens p Maybe User f t
  -> M.Component p (L.SingleModel User) (L.Action User)
singleUserSelectorComponent r config style =
  L.singleListSelectorComponent r (toListSelectorConfig config) (toSingleSelectionStyle style)

singleUserEditorField
  :: (Eq t, Ord p, Default patch)
  => SyncContext
  -> M.MisoString
  -> (User -> Bool)
  -> EntityPatchTransformedLens p patch Maybe User Maybe t
  -> EditorField p patch f
singleUserEditorField r k p eptl =
  let config e =
        UserSelectorConfig
          { isPossibleUser = p
          , isInitialUser = \u -> e ^. eptl.viewLens == Just (eptl.transform u)
          }
   in selectorEditorField
        k
        eptl
        (singleUserSelectorComponent r . config)
        (SingleUserSelectorStyleButtons, SingleUserSelectorStyleShowOnly)

data MultiUserSelectorStyle
  = MultiUserSelectorStyleButtons
  | MultiUserSelectorStyleShowOnly
  deriving (Eq, Show)

toMultiSelectionStyle :: MultiUserSelectorStyle -> L.MultiSelectionStyle
toMultiSelectionStyle MultiUserSelectorStyleButtons = L.MButtons
toMultiSelectionStyle MultiUserSelectorStyleShowOnly = L.MShow

multiUserSelectorComponent
  :: SyncContext
  -> UserSelectorConfig
  -> MultiUserSelectorStyle
  -> SelectorTransformedLens p [] User f t
  -> M.Component p (L.MultiModel User) (L.Action User)
multiUserSelectorComponent r config style =
  L.multiListSelectorComponent r (toListSelectorConfig config) (toMultiSelectionStyle style)

multiUserEditorField
  :: (Ord p, Ord t, Foldable f, Default patch)
  => SyncContext
  -> M.MisoString
  -> (User -> Bool)
  -> EntityPatchTransformedLens p patch [] User f t
  -> EditorField p patch f'
multiUserEditorField r k p eptl =
  let config e =
        let initialSelection = Set.fromList (toList $ e ^. eptl.viewLens)
         in UserSelectorConfig
              { isPossibleUser = p
              , isInitialUser = \u -> eptl.transform u `Set.member` initialSelection
              }
   in selectorEditorField
        k
        eptl
        (multiUserSelectorComponent r . config)
        (MultiUserSelectorStyleShowOnly, MultiUserSelectorStyleButtons)

listUsers :: (User -> Bool) -> Document -> [User]
listUsers p d = filter p $ Ix.toAscList (Proxy @Text) d.users

showUser :: User -> M.MisoString
showUser u = M.ms u.name

-- ============================================================================
-- SEARCHABLE VARIANTS
-- ============================================================================

-- | Searchable single-select user component
searchableSingleUserSelectorComponent
  :: SyncContext
  -> UserSelectorConfig
  -> SelectorTransformedLens p Maybe User f t
  -> M.Component p (SL.SearchableSingleModel User) (SL.SearchableSingleAction User)
searchableSingleUserSelectorComponent r config =
  SL.searchableSingleSelectorComponent r (toListSelectorConfig config)

-- | Searchable multi-select user component
searchableMultiUserSelectorComponent
  :: SyncContext
  -> UserSelectorConfig
  -> SelectorTransformedLens p [] User f t
  -> M.Component p (SL.SearchableModel User) (SL.SearchableAction User)
searchableMultiUserSelectorComponent r config =
  SL.searchableMultiSelectorComponent r (toListSelectorConfig config)

-- | Searchable multi-user editor field for use in editors
-- Uses a read-only viewer (comma-separated names with count) and searchable combobox for editing
searchableMultiUserEditorField
  :: (Ord p, Ord t, Foldable f, Default patch)
  => SyncContext
  -> M.MisoString
  -> (User -> Bool)
  -> EntityPatchTransformedLens p patch [] User f t
  -> EditorField p patch f'
searchableMultiUserEditorField r k p eptl =
  let config e =
        let initialSelection = Set.fromList (toList $ e ^. eptl.viewLens)
         in UserSelectorConfig
              { isPossibleUser = p
              , isInitialUser = \u -> eptl.transform u `Set.member` initialSelection
              }
   in selectorEditorFieldWithViewer
        k
        eptl
        (selectedUsersViewerComponent r . config)
        (searchableMultiUserSelectorComponent r . config)

-- | Searchable single-user editor field for use in editors
-- Uses a read-only viewer (user name or placeholder) and searchable combobox for editing
searchableSingleUserEditorField
  :: (Eq t, Ord p, Default patch)
  => SyncContext
  -> M.MisoString
  -> (User -> Bool)
  -> EntityPatchTransformedLens p patch Maybe User Maybe t
  -> EditorField p patch f
searchableSingleUserEditorField r k p eptl =
  let config e =
        UserSelectorConfig
          { isPossibleUser = p
          , isInitialUser = \u -> e ^. eptl.viewLens == Just (eptl.transform u)
          }
   in selectorEditorFieldWithViewer
        k
        eptl
        (selectedSingleUserViewerComponent r . config)
        (searchableSingleUserSelectorComponent r . config)

-- ============================================================================
-- SELECTED USERS VIEWER (Read-only display)
-- ============================================================================

-- | Model for the selected users viewer
data SelectedUsersViewerModel = SelectedUsersViewerModel
  { possibleValues :: ![User]
  , selectedValues :: ![User]
  }
  deriving (Eq, Generic, Show)

-- | Action for the selected users viewer
newtype SelectedUsersViewerAction = ViewerUpdateDocument DocumentChange
  deriving (Eq, Show)

-- | Component that displays selected users as comma-separated text with count
-- Used as the viewer in editor fields (read-only display)
selectedUsersViewerComponent
  :: SyncContext
  -> UserSelectorConfig
  -> SelectorTransformedLens p [] User f t
  -> M.Component p SelectedUsersViewerModel SelectedUsersViewerAction
selectedUsersViewerComponent r config lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding (O.castOptic #selectedValues)]
    , M.subs = [subscribeDocument r ViewerUpdateDocument]
    }
  where
    model =
      SelectedUsersViewerModel
        { possibleValues = []
        , selectedValues = []
        }

    update (ViewerUpdateDocument (DocumentChange d info)) =
      M.modify $ \m ->
        let listSelectorCfg = toListSelectorConfig config
            newPossibleValues = listSelectorCfg.listValues d
            newSelectedValues =
              if isInitialUpdate info
                then filter listSelectorCfg.isInitialValue newPossibleValues
                else filter (`Set.member` Set.fromList newPossibleValues) m.selectedValues
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValues .~ newSelectedValues)

    view m = viewSelectedUsers m.selectedValues

-- | Render a list of users as comma-separated text with count in brackets
-- Empty list shows translated placeholder text
viewSelectedUsers :: [User] -> M.View m a
viewSelectedUsers users =
  case users of
    [] -> Typography.muted (C.translate' C.LblNoStudentsSelected)
    _ ->
      let names = Text.intercalate ", " (map (.name) users)
          count = Text.pack $ " (" <> show (length users) <> ")"
       in MH.span_ [] [M.text (M.ms $ names <> count)]

-- ============================================================================
-- SELECTED SINGLE USER VIEWER (Read-only display)
-- ============================================================================

-- | Model for the selected single user viewer
data SelectedSingleUserViewerModel = SelectedSingleUserViewerModel
  { possibleValues :: ![User]
  , selectedValue :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Action for the selected single user viewer
newtype SelectedSingleUserViewerAction = SingleViewerUpdateDocument DocumentChange
  deriving (Eq, Show)

-- | Component that displays selected user name or placeholder
-- Used as the viewer in editor fields (read-only display)
selectedSingleUserViewerComponent
  :: SyncContext
  -> UserSelectorConfig
  -> SelectorTransformedLens p Maybe User f t
  -> M.Component p SelectedSingleUserViewerModel SelectedSingleUserViewerAction
selectedSingleUserViewerComponent r config lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding (O.castOptic #selectedValue)]
    , M.subs = [subscribeDocument r SingleViewerUpdateDocument]
    }
  where
    model =
      SelectedSingleUserViewerModel
        { possibleValues = []
        , selectedValue = Nothing
        }

    update (SingleViewerUpdateDocument (DocumentChange d info)) =
      M.modify $ \m ->
        let listSelectorCfg = toListSelectorConfig config
            newPossibleValues = listSelectorCfg.listValues d
            newSelectedValue =
              if isInitialUpdate info
                then find listSelectorCfg.isInitialValue newPossibleValues
                else case m.selectedValue of
                  Just u | u `elem` newPossibleValues -> Just u
                  _ -> Nothing
         in m
              & (#possibleValues .~ newPossibleValues)
              & (#selectedValue .~ newSelectedValue)

    view m = viewSelectedSingleUser m.selectedValue

-- | Render a single user name or placeholder text
viewSelectedSingleUser :: Maybe User -> M.View m a
viewSelectedSingleUser = \case
  Nothing -> Typography.muted (C.translate' C.LblNoStudentSelected)
  Just u -> MH.span_ [] [M.text (M.ms u.name)]
