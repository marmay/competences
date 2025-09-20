module Competences.Frontend.Component.UserListEditor
  ( userListEditorComponent
  )
where

import Competences.Command (Command)
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..), UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Map qualified as M
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (ix, (%~), (&), (.~), (^.), (^?))

newtype Model = Model
  { users :: [User]
  }
  deriving (Eq, Generic, Show)

newtype Action
  = UpdateDocument DocumentChange
  deriving (Eq, Generic, Show)

data UserListColumn
  = NameColumn
  | RoleColumn
  | DeleteColumn
  deriving (Eq, Ord, Show)

userListEditorComponent :: SyncDocumentRef -> User -> M.Component p Model Action
userListEditorComponent r u =
  (M.component model update view) {M.subs = [subscribeDocument r UpdateDocument]}
  where
    model = Model []

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange d _)) = M.modify $ #users .~ Ix.toAscList (Proxy @Text) (d ^. #users)

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
                , rows = m ^. #users
                , columnSpec = \case
                    DeleteColumn -> V.SingleActionColumn
                    _ -> V.AutoSizedColumn
                , columnHeader = \case
                    NameColumn -> C.translate' C.LblUserName
                    RoleColumn -> C.translate' C.LblUserRole
                    DeleteColumn -> ""
                , cellContents = \user -> \case
                    NameColumn -> editableName user
                    RoleColumn -> V.text_ "" -- editableRole (user ^. #id) (user ^. #role)
                    DeleteColumn -> V.text_ "" -- deleteButton (user ^. #id)
                }
          editableName = undefined
       in -- editableName userId userName lockedBy
          --   | u ^. #id == userId = V.text_ userName
          --   | otherwise = case lockedBy of
          --       NotLocked ->
          --         V.hBox_
          --           (V.Expand V.Start)
          --           V.NoExpand
          --           V.SmallGap
          --           [ V.text_ userName
          --           , V.spring_
          --           , V.editButton [M.onClick $ IssueCommand (LockField (UserName userId))]
          --           ]
          --       LockedBy _ -> V.text_ (userName <> " (" <> C.translate' C.LblLocked <> ")")
          --       LockedByUs t -> M.textInput_ userName [M.onInput $ UpdateName userId]
          V.vBox_ V.NoExpand (V.Expand V.Start) V.SmallGap [title, users]
