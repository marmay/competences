module Competences.Frontend.Component.UserListEditor
  ( userListEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), UsersCommand (..), UserPatch (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lock (..), User (..), UserRole (..))
import Competences.Document.User (Office365Id (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Static (StaticComponent, StaticView, staticComponent)
import Competences.Frontend.SyncDocument (SyncDocumentRef, modifySyncDocument, nextId)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Iso', iso, (%), (&), (?~), (^.))

data Action
  = NewUser
  deriving (Eq, Generic, Show)

-- | Iso for converting between Office365Id and Text
office365IdIso :: Iso' Office365Id Text
office365IdIso = iso (\(Office365Id t) -> t) Office365Id

-- | Iso for converting Change Office365Id to Change Text
office365IdChangeIso :: Iso' (Maybe (Office365Id, Office365Id)) (Maybe (Text, Text))
office365IdChangeIso = iso toText fromText
  where
    toText Nothing = Nothing
    toText (Just (Office365Id a, Office365Id b)) = Just (a, b)
    fromText Nothing = Nothing
    fromText (Just (a, b)) = Just (Office365Id a, Office365Id b)

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
              , office365Id = Office365Id ""
              }
      modifySyncDocument r (Users $ OnUsers $ CreateAndLock user)

    view :: StaticView Action
    view =
      let title = Typography.h2 (C.translate' C.LblUserList)
          usersEditable =
            TE.editable
              ( \d ->
                  map
                    (\u -> (u, (d ^. #locks) Map.!? UserLock u.id))
                    (Ix.toAscList (Proxy @Text) (d ^. #users))
              )
              & (#modify ?~ \u m -> Users $ OnUsers $ Modify u.id m)
              & (#delete ?~ \u -> Users $ OnUsers $ Delete u.id)
          usersEditor =
            TE.editor
              TE.editorTableRowView'
              usersEditable
              `TE.addNamedField` (C.translate' C.LblUserName, TE.textEditorField #name #name)
              `TE.addNamedField` (C.translate' C.LblUserRole, TE.enumEditorField' #role #role)
              `TE.addNamedField` (C.translate' C.LblUserEmail, TE.textEditorField (#office365Id % office365IdIso) (#office365Id % office365IdChangeIso))
          users = component "user-list-editor-users-editor" (TE.editorComponent usersEditor r)
          addButton =
            Button.buttonPrimary (C.translate' C.LblAddUser)
              & Button.withIcon IcnAdd
              & Button.withClick NewUser
              & Button.renderButton
          header =
            M.div_
              [class_ "flex items-center justify-between w-full"]
              [title, addButton]
       in V.centeredContent $
            M.div_
              [class_ "h-full min-h-0 flex flex-col gap-2 w-full max-w-5xl"]
              [ header
              , M.div_
                  [class_ "flex-1 min-h-0 overflow-y-auto"]
                  [users]
              ]
