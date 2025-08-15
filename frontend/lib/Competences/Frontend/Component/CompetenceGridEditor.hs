module Competences.Frontend.Component.CompetenceGridEditor
  ( Model
  , Action
  , competenceGridEditorComponent
  )
where

import Competences.Command (Command (..))
import Competences.Document
  ( ChangableField (..)
  , Competence (..)
  , CompetenceId
  , Document (..)
  , Level (..)
  , User (..)
  , UserId
  , emptyDocument
  , levels
  , ordered
  )
import Competences.Document.Order (Reorder, orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (ix, (%~), (&), (.~), (^?))

data Model = Model
  { user :: !User
  , document :: !Document
  , editFields :: !(Map.Map ChangableField M.MisoString)
  , reorderFrom :: !(C.ReorderModel Competence)
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
  | ReorderAction !(C.ReorderAction Competence)
  deriving (Eq, Generic, Show)

competenceGridEditorComponent :: SyncDocumentRef -> User -> M.Component p Model Action
competenceGridEditorComponent r u = (M.component model update view) {M.subs = [subscribeDocument r UpdateDocument]}
  where
    model =
      Model
        { user = u
        , document = emptyDocument
        , editFields = Map.empty
        , reorderFrom = C.initialReorderModel
        }

    update (EditField field value) = M.modify $ #editFields %~ Map.insert field value
    update (UpdateDocument (DocumentChange newDocument _)) = do
      M.modify $ \s ->
        s
          & (#document .~ newDocument)
          & (#editFields %~ updateEditFields s.user newDocument)
      s <- M.get
      M.io_ $ M.consoleLog $ M.ms $ show s.document.lockedFields
    update (IssueCommand cmd) = M.io_ $ modifySyncDocument r cmd
    update (ReorderAction a) = do
      m <- M.get
      C.liftEffect #reorderFrom ReorderAction (C.updateReorderModel r (mkReorderCommand m) a)
      where
        mkReorderCommand :: Model -> CompetenceId -> Reorder Competence -> Maybe Command
        mkReorderCommand m id' to = do
          fromPosition <- orderPosition m.document.competences id'
          pure $ ReorderCompetence fromPosition to

updateEditFields
  :: User -> Document -> Map.Map ChangableField M.MisoString -> Map.Map ChangableField M.MisoString
updateEditFields u d m = Map.intersection m (mkEditFields u d)

mkEditFields :: User -> Document -> Map.Map ChangableField M.MisoString
mkEditFields u d =
  Map.toList d.lockedFields
    & filter (\(_, userId) -> userId == u.id)
    & map (\(field, _) -> (field, fromMaybe "Not set" $ d ^? ix field))
    & Map.fromList

data CompetenceGridColumn
  = MoveColumn
  | DescriptionColumn
  | LevelDescriptionColumn !Level
  | DeleteColumn
  deriving (Eq, Ord, Show)

view :: Model -> M.View m Action
view m =
  let title = editable V.title_ CompetenceGridTitle m
      description = editable V.text_ CompetenceGridDescription m
      fmtText t = M.span_ [T.tailwind [T.AlignMiddle]] [V.text_ t]
      competences =
        V.viewTable $
          V.Table
            { columns =
                [ MoveColumn
                , DescriptionColumn
                ]
                  <> map LevelDescriptionColumn levels
                  <> [DeleteColumn]
            , rows = ordered m.document.competences
            , columnSpec = \case
                MoveColumn -> V.TripleActionColumn
                DeleteColumn -> V.SingleActionColumn
                _ -> V.AutoSizedColumn
            , columnHeader = \c -> fromMaybe "" $ case c of
                MoveColumn -> Nothing
                DescriptionColumn -> Just $ C.translate' C.LblCompetenceDescription
                LevelDescriptionColumn level -> Just $ C.translate' $ C.LblCompetenceLevelDescription level
                DeleteColumn -> Nothing
            , cellContents = \competence -> \case
                MoveColumn -> V.buttonRow $ map (ReorderAction <$>) $ C.viewReorderItem m.reorderFrom competence
                DescriptionColumn -> editable fmtText (CompetenceDescription competence.id) m
                LevelDescriptionColumn level -> editable fmtText (CompetenceLevelDescription (competence.id, level)) m
                DeleteColumn -> V.buttonRow [V.deleteButton [M.onClick $ IssueCommand (RemoveCompetence competence.id)]]
            }
   in V.vBox_
        V.NoExpand
        (V.Expand V.Start)
        V.SmallGap
        [ title
        , description
        , competences
        ]

editable :: (M.MisoString -> M.View m Action) -> ChangableField -> Model -> M.View m Action
editable fmtText f m =
  let render :: M.View m Action -> [M.View m Action] -> M.View m Action
      render content buttons =
        V.hBox_
          (V.Expand V.Start)
          V.NoExpand
          V.SmallGap
          [V.growing_ [content], V.buttonColumn buttons]
      fieldText = fromMaybe "" $ m.document ^? ix f
      inputField t =
        V.textarea_
          [M.id_ (M.ms $ show f), M.value_ (M.ms t), M.onInput (EditField f), T.tailwind [T.WFull]]
   in case editability m f of
        Editable ->
          render
            (fmtText $ M.ms fieldText)
            [V.editButton [M.onClick $ IssueCommand (LockField f m.user.id fieldText)]]
        LockedByUs t ->
          render
            (inputField t)
            [ V.applyButton [M.onClick $ IssueCommand (ReleaseField f (Just $ M.fromMisoString t))]
            , V.cancelButton [M.onClick $ IssueCommand (ReleaseField f Nothing)]
            ]
        LockedBy _u -> render (M.text $ M.ms fieldText) [] -- [lockedBy u]

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
