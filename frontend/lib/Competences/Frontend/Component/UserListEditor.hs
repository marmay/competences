module Competences.Frontend.Component.UserListEditor
  ( userListEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserRole (..), Lock (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Static (StaticComponent, StaticView, staticComponent)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument, nextId
  )
import Competences.Frontend.View qualified as V
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~), (?~), (^.), (%))
import qualified Competences.Frontend.Component.Editor.TableView as TE
import qualified Data.Map as Map

newtype Model = Model
  { users :: [User]
  }
  deriving (Eq, Generic, Show)

data Action
  = NewUser
  deriving (Eq, Generic, Show)

userListEditorComponent :: SyncDocumentRef -> StaticComponent p Action
userListEditorComponent r =
  staticComponent update view
  where
    update :: Action -> M.Effect p () Action
    update NewUser = M.io_ $ do
      userId <- nextId r
      let user =
            User
              { id = userId
              , name = ""
              , role = Student
              }
      modifySyncDocument r (OnUsers $ Create user)
      modifySyncDocument r (OnUsers $ Modify userId Lock)

    view :: StaticView Action
    view =
      let title = V.title_ (C.translate' C.LblUserList)
          usersEditable =
            TE.editable
              ( \d ->
                  map
                    (\u -> (u, (d ^. #locks) Map.!? UserLock u.id))
                    (Ix.toAscList (Proxy @Text) (d ^. #users))
              )
              & (#modify ?~ \u m -> OnUsers $ Modify u.id m)
              & (#delete ?~ \u -> OnUsers $ Delete u.id)
          usersEditor =
            TE.editor
              TE.editorTableRowView'
              usersEditable
              `TE.addNamedField` (C.translate' C.LblUserName, TE.textEditorField (#name % TE.msIso))
              `TE.addNamedField` (C.translate' C.LblUserRole, TE.enumEditorField' #role)
          users = M.div_ [] M.+> TE.editorComponent usersEditor r
          addButton = V.iconLabelButton' V.IcnAdd C.LblAddUser NewUser
       in V.viewFlow V.vFlow [title, users, V.viewButton addButton]
