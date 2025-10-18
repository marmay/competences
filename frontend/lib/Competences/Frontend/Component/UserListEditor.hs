module Competences.Frontend.Component.UserListEditor
  ( userListEditorComponent
  )
where

import Competences.Command (Command (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserRole (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((.~), (^.))
import System.Random (randomIO)
import qualified Miso.Html.Property as M

newtype Model = Model
  { users :: [User]
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument DocumentChange
  | IssueCommand Command
  | NewUser
  deriving (Eq, Generic, Show)

data UserListColumn
  = NameColumn
  | RoleColumn
  | DeleteColumn
  deriving (Eq, Ord, Show)

userListEditorComponent :: SyncDocumentRef -> M.Component p Model Action
userListEditorComponent r =
  (M.component model update view) {M.subs = [subscribeDocument r UpdateDocument]}
  where
    model = Model []

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange d _)) = do
      M.io_ $ M.consoleLog $ "UserListEditor: UpdateDocument: " <> M.ms (show d)
      M.modify $ #users .~ Ix.toAscList (Proxy @Text) (d ^. #users)
    update (IssueCommand cmd) = M.io_ $ modifySyncDocument r cmd
    update NewUser = M.io_ $ do
      -- uid <- randomIO
      -- modifySyncDocument r $ AddUser $ User {id = uid, name = "", role = Student}
      pure ()

    view :: Model -> M.View Model Action
    view m =
      let title = V.title_ (C.translate' C.LblUserList)
          users =
            V.viewTable $
              V.Table
                { columns =
                    [ NameColumn
                    , RoleColumn
                    , DeleteColumn
                    ]
                , rows = m.users
                , columnSpec = \case
                    NameColumn -> V.TableColumnSpec V.AutoSizedColumn (C.translate' C.LblUserName)
                    RoleColumn -> V.TableColumnSpec V.AutoSizedColumn (C.translate' C.LblUserRole)
                    DeleteColumn -> V.TableColumnSpec V.SingleActionColumn ""
                , rowContents = V.cellContents $ \user -> \case
                    NameColumn -> undefined -- editableName user.id
                    RoleColumn -> undefined -- editableRole user.id
                    DeleteColumn -> undefined -- V.viewButton (deleteButton user.id)
                }
          addButton = V.iconLabelButton' V.IcnAdd C.LblAddUser NewUser
          -- editableName u' = M.div_ [M.key_ $ M.ms (show (UserName u'))] M.+> undefined
          -- editableRole u' = M.div_ [M.key_ $ M.ms (show (UserRole u'))] M.+> undefined
          -- deleteButton u' = V.deleteButton (IssueCommand (RemoveUser u'))
       in V.viewFlow V.vFlow [title, users, V.viewButton addButton]
