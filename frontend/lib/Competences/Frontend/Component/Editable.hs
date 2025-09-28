module Competences.Frontend.Component.Editable
  ( editableComponent
  )
where

import Competences.Command (Command (..))
import Competences.Command.ChangeField
  ( ChangableFieldMeta (..)
  , FieldEncoding(..)
  , FieldType(..)
  , changableFieldMeta
  , defaultEncoding
  )
import Competences.Document (ChangableField, Document (..), User (..))
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (ix, (%~), (&), (.~), (^?))
import Data.Either (fromRight)
import qualified Miso.Html.Property as M

data Model = Model
  { user :: !User
  , contents :: !(Either Text FieldEncoding)
  , editability :: !Editability
  }
  deriving (Eq, Generic, Show)

data Editability
  = Editable
  | LockedBy !(Maybe User)
  | LockedByUs !FieldEncoding
  deriving (Eq, Generic, Show)

data Action
  = EditField !FieldEncoding
  | IssueCommand !Command
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

editableComponent :: SyncDocumentRef -> User -> ChangableField -> M.Component p Model Action
editableComponent r u f = (M.component model update view) {M.subs = [subscribeDocument r UpdateDocument]}
  where
    meta = changableFieldMeta f

    model :: Model
    model =
      Model
        { user = u
        , contents = Left "not initialized"
        , editability = Editable
        }

    update (EditField value) = M.modify $ #editability %~ (`withContentsFrom` LockedByUs value)
    update (IssueCommand cmd) = M.io_ $ modifySyncDocument r cmd
    update (UpdateDocument (DocumentChange newDocument _)) = do
      M.modify $ \s ->
        s
          & (#contents .~ meta.get newDocument)
          & (#editability %~ updateEditability newDocument)

    updateEditability :: Document -> Editability -> Editability
    updateEditability newDocument oldEditability =
      newEditability `withContentsFrom` oldEditability
      where
        newEditability = case Map.lookup f newDocument.lockedFields of
          Nothing -> Editable
          Just lockedUid ->
            if lockedUid == u.id
              then LockedByUs $ fromRight (defaultEncoding meta.fieldType) $ meta.get newDocument
              else LockedBy $ newDocument.users ^? ix lockedUid

    withContentsFrom :: Editability -> Editability -> Editability
    withContentsFrom (LockedByUs _) (LockedByUs contents) = LockedByUs contents
    withContentsFrom new _ = new

    view :: Model -> M.View Model Action
    view m = view' m.editability m

    view' :: Editability -> Model -> M.View Model Action
    view' Editable m =
      case m.contents of
        Left err -> V.text_ $ M.ms err
        Right contents -> 
          withButtons [V.editButton (IssueCommand (LockField f u.id contents))] $
            renderContents meta.fieldType contents
    view' (LockedBy _) m =
      case m.contents of
        Left err -> V.text_ $ M.ms err
        Right contents ->
          withButtons [] $
            renderContents meta.fieldType contents
    view' (LockedByUs contents) _ =
      withButtons
        [ V.applyButton (IssueCommand (ReleaseField f u.id (Just contents)))
        , V.cancelButton (IssueCommand (ReleaseField f u.id Nothing))
        ]
        $ renderForm meta.fieldType contents

    withButtons :: [V.Button () Action] -> M.View Model Action -> M.View Model Action
    withButtons buttons content =
      V.viewFlow
        (V.hFlow & (#expandDirection .~ V.Expand V.Start) & (#gap .~ V.SmallSpace))
        [content
        , V.viewButtons V.hButtons buttons]

    renderContents TextField (TextEncoding contents) = V.text_ (M.ms contents)
    renderContents EnumField{} (TextEncoding contents) = V.text_ (M.ms contents)
    renderContents _ _ = V.text_ "Application error"

    renderForm TextField (TextEncoding contents) = V.textarea_ [M.value_ (M.ms contents), M.onInput (EditField . TextEncoding . M.fromMisoString)]
    -- renderForm EnumField{} (TextEncoding contents) = V.select_ [M.value_ contents, M.onChange (EditField . TextEncoding)] $ map (V.option_ . fst) options
    renderForm _ _ = V.text_ "Application error"
