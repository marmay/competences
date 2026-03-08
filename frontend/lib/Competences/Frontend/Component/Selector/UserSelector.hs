module Competences.Frontend.Component.Selector.UserSelector
  ( UserSelectorConfig (..)
  , defaultUserSelectorConfig

    -- * Searchable variants
  , searchableSingleUserSelectorComponent
  , searchableSingleUserEditorField
  , searchableMultiUserSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common (EntityPatchTransformedLens (..), SelectorTransformedLens (..), mkSelectorBinding)
import Competences.Frontend.Component.Selector.ListSelector qualified as L
import Competences.Frontend.Component.Selector.SearchableListSelector qualified as SL
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, isInitialUpdate, subscribeDocument)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (Default)
import Data.Foldable (find)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~), (^.))
import Optics.Core qualified as O

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
