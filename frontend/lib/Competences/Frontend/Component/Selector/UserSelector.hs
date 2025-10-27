module Competences.Frontend.Component.Selector.UserSelector
  ( singleUserSelectorComponent
  , multiUserSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorField)
import Competences.Frontend.Component.Selector.Common (SelectorTransformedLens)
import Competences.Frontend.Component.Selector.ListSelector qualified as L
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M

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

toListSelectorConfig :: UserSelectorConfig -> L.ListSelectorConfig User
toListSelectorConfig config =
  L.ListSelectorConfig
    { L.isPossibleItem = p
    , L.isInitialItem = config.isInitialUser
    , L.showItem = showUser
    , L.listItems = listUsers p
    }

singleUserSelectorComponent
  :: SyncDocumentRef
  -> UserSelectorConfig
  -> SingleUserSelectorStyle
  -> SelectorTransformedLens p (Maybe User) t
  -> M.Component p (L.SingleModel User) (L.Action User)
singleUserSelectorComponent r config parentLens =
  L.singleListSelectorComponent r parentLens (toListSelectorConfig config) L.SButtons

singleUserEditorField
  :: (Ord p)
  => SyncDocumentRef
  -> (User -> Bool)
  -> SelectorTransformedLens p (Maybe User) (Maybe User)
  -> EditorField p f
singleUserEditorField r p l =
  selectorEditorField
    l
    (V.text_ . maybe (C.translate' C.LblNoUser) showUser)
    (\e -> singleUserSelectorComponent r p undefined)

singleUserIdEditorField r p l =
  selectorEditorField
    l
    (V.text_ . maybe (C.translate' C.LblNoUser) (V.text_ . C.userId))
    (\e -> singleUserSelectorComponent r p undefined)

multiUserSelectorComponent
  :: SyncDocumentRef
  -> (User -> Bool)
  -> (User -> Bool)
  -> SelectorTransformedLens p [User] t
  -> M.Component p (L.MultiModel User) (L.Action User)
multiUserSelectorComponent r p selectInitialUsers parentLens =
  L.multiListSelectorComponent r (listUsers p) showUser parentLens selectInitialUsers L.MButtons

listUsers :: (User -> Bool) -> Document -> [User]
listUsers p d = filter p $ Ix.toAscList (Proxy @Text) d.users

showUser :: User -> M.MisoString
showUser u = M.ms u.name
