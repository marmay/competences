module Competences.Frontend.Component.Selector.UserSelector
  ( UserSelectorConfig (..)
  , singleUserSelectorComponent
  , singleUserEditorField
  , multiUserSelectorComponent
  , multiUserEditorField
  , defaultUserSelectorConfig
  , SingleUserSelectorStyle (..)
  , MultiUserSelectorStyle (..)
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorField)
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens (..))
import Competences.Frontend.Component.Selector.ListSelector qualified as L
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Data.Foldable (toList)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((^.))

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
  :: SyncDocumentRef
  -> UserSelectorConfig
  -> SingleUserSelectorStyle
  -> SelectorTransformedLens p Maybe User f t
  -> M.Component p (L.SingleModel User) (L.Action User)
singleUserSelectorComponent r config style =
  L.singleListSelectorComponent r (toListSelectorConfig config) (toSingleSelectionStyle style)

singleUserEditorField
  :: (Eq t, Ord p)
  => SyncDocumentRef
  -> M.MisoString
  -> (User -> Bool)
  -> SelectorTransformedLens p Maybe User Maybe t
  -> EditorField p f
singleUserEditorField r k p l =
  let config e =
        UserSelectorConfig
          { isPossibleUser = p
          , isInitialUser = \u -> e ^. l.lens == Just (l.transform u)
          }
   in selectorEditorField
        k
        l
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
  :: SyncDocumentRef
  -> UserSelectorConfig
  -> MultiUserSelectorStyle
  -> SelectorTransformedLens p [] User f t
  -> M.Component p (L.MultiModel User) (L.Action User)
multiUserSelectorComponent r config style =
  L.multiListSelectorComponent r (toListSelectorConfig config) (toMultiSelectionStyle style)

multiUserEditorField
  :: (Ord p, Ord t, Foldable f)
  => SyncDocumentRef
  -> M.MisoString
  -> (User -> Bool)
  -> SelectorTransformedLens p [] User f t
  -> EditorField p f'
multiUserEditorField r k p l =
  let config e =
        let initialSelection = Set.fromList (toList $ e ^. l.lens)
         in UserSelectorConfig
              { isPossibleUser = p
              , isInitialUser = \u -> l.transform u `Set.member` initialSelection
              }
   in selectorEditorField
        k
        l
        (multiUserSelectorComponent r . config)
        (MultiUserSelectorStyleShowOnly, MultiUserSelectorStyleButtons)

listUsers :: (User -> Bool) -> Document -> [User]
listUsers p d = filter p $ Ix.toAscList (Proxy @Text) d.users

showUser :: User -> M.MisoString
showUser u = M.ms u.name
