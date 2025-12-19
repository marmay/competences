module Competences.Frontend.Component.UserListEditor
  ( userListEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.User (Office365Id (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Static (StaticComponent, StaticView, staticComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument, nextId)
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Iso', iso, (%), (&), (.~), (?~), (^.))

data Action
  = NewUser
  deriving (Eq, Generic, Show)

-- | Iso for converting between Maybe Office365Id and MisoString
office365IdIso :: Iso' (Maybe Office365Id) M.MisoString
office365IdIso = iso toMiso fromMiso
  where
    toMiso :: Maybe Office365Id -> M.MisoString
    toMiso (Just (Office365Id email)) = M.ms email
    toMiso Nothing = ""

    fromMiso :: M.MisoString -> Maybe Office365Id
    fromMiso s =
      let email = M.fromMisoString s
       in if T.null email
            then Nothing
            else Just (Office365Id email)

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
              , office365Id = Nothing
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
              `TE.addNamedField` (C.translate' C.LblUserEmail, TE.textEditorField (#office365Id % office365IdIso))
          users = M.div_ [] M.+> TE.editorComponent usersEditor r
          addButton = V.viewButton $ V.iconLabelButton' V.IcnAdd C.LblAddUser NewUser
       in V.viewFlow
            ( V.vFlow
                & (#expandOrthogonal .~ V.Expand V.Start)
                & (#gap .~ V.SmallSpace)
            )
            [title, users, addButton]
