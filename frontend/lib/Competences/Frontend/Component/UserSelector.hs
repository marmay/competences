module Competences.Frontend.Component.UserSelector
  ( singleUserSelectorComponent
  , multiUserSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document(..), User (..))
import Competences.Frontend.Component.ListSelector qualified as L
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Miso qualified as M
import Optics.Core (Lens')

singleUserSelectorComponent
  :: SyncDocumentRef
  -> (User -> Bool)
  -> Lens' p (Maybe User)
  -> M.Component p (L.SingleModel User) (L.Action User)
singleUserSelectorComponent r p parentLens =
  L.singleListSelectorComponent r (listUsers p) showUser parentLens L.SButtons

multiUserSelectorComponent
  :: SyncDocumentRef
  -> (User -> Bool)
  -> Lens' p [User]
  -> M.Component p (L.MultiModel User) (L.Action User)
multiUserSelectorComponent r p parentLens =
  L.multiListSelectorComponent r (listUsers p) showUser parentLens L.MButtons

listUsers :: (User -> Bool) -> Document -> [User]
listUsers p d = filter p $ Ix.toAscList (Proxy @Text) d.users

showUser :: User -> M.MisoString
showUser u = M.ms u.name
