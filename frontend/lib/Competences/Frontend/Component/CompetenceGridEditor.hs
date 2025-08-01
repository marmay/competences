module Competences.Frontend.Component.CompetenceGridEditor
  ( Model
  , Action (..)
  , mkModel
  , subscriptions
  , update
  , view
  )
where

import Competences.Command (Command (..))
import Competences.Document (ChangableField (..), Document (..), User (..), UserId, emptyDocument, fieldATraversal)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String qualified as M
import Optics.Core (at, (%), (%~), (&), (.~), (^.), (^?))

data Model = Model
  { user :: !User
  , translationData :: !C.TranslationData
  , document :: !Document
  , editFields :: !(Map.Map ChangableField M.MisoString)
  }
  deriving (Eq, Generic, Show)

data Action
  = -- | An internal Action to update the state of edit fields.
    EditField !ChangableField !M.MisoString
  | -- | The host component must trigger this action when the document
    -- has been updated.
    UpdateDocument !DocumentChange
  | -- | The host component must catch those actions and issue the
    -- commands.
    IssueCommand !Command
  deriving (Eq, Generic, Show)

mkModel :: User -> C.TranslationData -> Model
mkModel user translationData =
  Model
    { user = user
    , translationData = translationData
    , document = emptyDocument
    , editFields = Map.empty
    }

subscriptions :: SyncDocumentRef -> [M.Sub Action]
subscriptions r = [subscribeDocument r UpdateDocument]

update :: SyncDocumentRef -> Action -> M.Effect Model Action
update _ (EditField field value) = M.modify $ #editFields %~ Map.insert field value
update _ (UpdateDocument (DocumentChange newDocument _)) = do
  M.modify $ \s ->
    s
    & (#document .~ newDocument)
    & (#editFields %~ updateEditFields s.user newDocument)
update r (IssueCommand cmd) = M.io_ $ modifySyncDocument r cmd

updateEditFields
  :: User -> Document -> Map.Map ChangableField M.MisoString -> Map.Map ChangableField M.MisoString
updateEditFields u d m = Map.intersection m (mkEditFields u d)

mkEditFields :: User -> Document -> Map.Map ChangableField M.MisoString
mkEditFields u d =
  Map.toList d.lockedFields
    & filter (\(_, userId) -> userId == u.id)
    & map (\(field, _) -> (field, ""))
    & Map.fromList

view :: Model -> M.View Action
view m =
  let title = editable [C.styledClass C.ClsTitle] CompetenceGridTitle m
      description = editable [C.styledClass C.ClsDescription] CompetenceGridDescription m
      competences =
        M.div_
          [C.styledClass C.ClsCompetences]
          []
   in -- \$ map _ m.document.competences
      M.div_
        []
        [ title
        , description
        , competences
        ]

editable :: [M.Attribute Action] -> ChangableField -> Model -> M.View Action
editable attrs f m =
  let render :: M.View Action -> [M.View Action] -> M.View Action
      render content buttons =
        M.div_
          (C.styledClass C.ClsEditableContainer : attrs)
          [ M.span_ [C.styledClass C.ClsEditableContent] [content]
          , M.span_ [C.styledClass C.ClsEditableButtons] buttons
          ]
      fieldText = fromMaybe "" $ m.document ^? fieldATraversal f
      inputField t = M.input_ [M.id_ (M.ms $ show f), M.value_ (M.ms t), M.onInput (EditField f)]
      editButton =
        C.iconButton
          [M.onClick $ IssueCommand (LockField f m.user.id fieldText)]
          C.IcnEdit
          (C.translate m C.LblEdit)
      applyButton t =
        C.iconButton
          [M.onClick $ IssueCommand (ReleaseField f (Just t))]
          C.IcnApply
          (C.translate m C.LblApplyChange)
      cancelButton =
        C.iconButton
          [M.onClick $ IssueCommand (ReleaseField f Nothing)]
          C.IcnCancel
          (C.translate m C.LblCancelChange)
   in case editability m f of
        Editable -> render (M.text fieldText) [editButton]
        LockedByUs t -> render (inputField t) [applyButton t, cancelButton]
        LockedBy _u -> render (M.text fieldText) [] -- [lockedBy u]

data Editability
  = Editable
  | LockedBy !UserId
  | LockedByUs !M.MisoString

editability :: Model -> ChangableField -> Editability
editability m f =
  case Map.lookup f m.document.lockedFields of
    Nothing -> Editable
    Just lockedUid ->
      if lockedUid == m.user.id
        then LockedByUs $ fromMaybe "" $ Map.lookup f m.editFields
        else LockedBy lockedUid
