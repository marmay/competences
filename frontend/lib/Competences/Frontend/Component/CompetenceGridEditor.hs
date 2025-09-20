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
  , emptyDocument
  , levels
  , ordered
  )
import Competences.Document.Order (Reorder, orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editable (editableComponent)
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((&), (.~))

data Model = Model
  { user :: !User
  , document :: !Document
  , reorderFrom :: !(C.ReorderModel Competence)
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  | IssueCommand !Command
  | ReorderAction !(C.ReorderAction Competence)
  deriving (Eq, Generic, Show)

data CompetenceGridColumn
  = MoveColumn
  | DescriptionColumn
  | LevelDescriptionColumn !Level
  | DeleteColumn
  deriving (Eq, Ord, Show)

competenceGridEditorComponent :: SyncDocumentRef -> User -> M.Component p Model Action
competenceGridEditorComponent r u = (M.component model update view) {M.subs = [subscribeDocument r UpdateDocument]}
  where
    model =
      Model
        { user = u
        , document = emptyDocument
        , reorderFrom = C.initialReorderModel
        }

    -- update (EditField field value) = M.modify $ #editFields %~ Map.insert field value
    update (UpdateDocument (DocumentChange newDocument _)) = do
      M.modify $ \s ->
        s
          & (#document .~ newDocument)
      -- & (#editFields %~ updateEditFields s.user newDocument)
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

    view :: Model -> M.View m Action
    view m =
      let title = M.div_ [] M.+> editableComponent r u CompetenceGridTitle
          description = M.div_ [] M.+> editableComponent r u CompetenceGridDescription
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
                    DescriptionColumn -> M.div_ [] M.+> editableComponent r u (CompetenceDescription competence.id)
                    LevelDescriptionColumn level -> M.div_ [] M.+> editableComponent r u (CompetenceLevelDescription (competence.id, level))
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
