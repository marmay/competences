module Competences.Frontend.Page.ManageUsersPage
  ( manageUsersPage
  , ManageUsersPage
  , ManageUsersView
  )
where

import Competences.Document (User)
import Competences.Frontend.Component.UserListEditor (userListEditorComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import GHC.Generics (Generic)
import Miso qualified as M

data Model = Model
  deriving (Eq, Generic, Show)

data Action
  deriving (Eq, Show)

type ManageUsersPage p = M.Component p Model Action

type ManageUsersView = M.View Model Action

manageUsersPage :: SyncDocumentRef -> User -> M.Component p Model Action
manageUsersPage r u =
  M.component Model update view
  where
    update _ = pure ()
    view Model = M.div_ [M.key_ @M.MisoString "user-list"] M.+> userListEditorComponent r u
