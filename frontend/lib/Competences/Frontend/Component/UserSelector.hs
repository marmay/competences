module Competences.Frontend.Component.UserSelector
  ( UserSelectorMessage (..)
  , userSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.User
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~), (?~), (^.))

data Model = Model
  { users :: [User]
  , selectedUser :: Maybe User
  }
  deriving (Eq, Generic, Show)

newtype UserSelectorMessage = UserSelectionChanged (Maybe User)
  deriving (Eq, Generic, Show)

instance FromJSON UserSelectorMessage

instance ToJSON UserSelectorMessage

selectUser :: User -> Model -> Model
selectUser u m
  | u `elem` m ^. #users = m & #selectedUser ?~ u
  | otherwise = cancelSelection m

cancelSelection :: Model -> Model
cancelSelection = #selectedUser .~ Nothing

updateUsers :: Document -> Model -> Model
updateUsers d = #users .~ Ix.toAscList (Proxy @Text) (d ^. #users)

notifySelection :: M.Effect p Model Action -> M.Effect p Model Action
notifySelection e = do
  e
  u <- M.gets (^. #selectedUser)
  M.mailParent $ UserSelectionChanged u

data Action
  = SelectUser !User
  | CancelSelection
  | UpdateDocument DocumentChange
  deriving (Eq, Show)

userSelectorComponent :: SyncDocumentRef -> M.Component p Model Action
userSelectorComponent syncDocumentRef =
  (M.component model update view)
    { M.subs = [subscribeDocument syncDocumentRef UpdateDocument]
    }
  where
    model = Model [] Nothing

    update :: Action -> M.Effect p Model Action
    update (SelectUser u) = notifySelection $ M.modify (selectUser u)
    update CancelSelection = notifySelection $ M.modify cancelSelection
    update (UpdateDocument c) = notifySelection $ M.modify (updateUsers (c ^. #document))

    view :: Model -> M.View Model Action
    view m = V.buttonRow $ map (viewUser m) (m ^. #users)

    viewUser :: Model -> User -> M.View Model Action
    viewUser m u =
      V.toggleButton onClicked toggleState (V.text_ $ u ^. #name)
      where
        onClicked V.ToggleOn = CancelSelection
        onClicked V.ToggleOff = SelectUser u

        toggleState
          | Just u == (m ^. #selectedUser) = V.ToggleOn
          | otherwise = V.ToggleOff
