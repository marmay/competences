module Competences.Frontend.Component.CompetenceGridViewer
  ( competenceGridViewerComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , User
  , UserId
  , emptyDocument
  , levels
  , ordered
  )
import Competences.Document.Competence (CompetenceLevelId, Level (..))
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.UserSelector (UserSelectorMessage (..), userSelectorComponent)
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument)
import Competences.Frontend.View qualified as V
import Data.Aeson (Result (..), fromJSON)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (ix, (%), (&), (.~), (?~), (^.), (^?))

competenceGridViewerComponent :: SyncDocumentRef -> User -> M.Component p Model Action
competenceGridViewerComponent r u =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    , M.mailbox = handleMail
    }
  where
    model = defaultSelection u emptyModel

    update :: Action -> M.Effect p Model Action
    update (SelectUser userId) =
      M.modify $ \m -> m & (#selectedUser .~ (m ^? (#document % #users % ix userId)))
    update CancelUserSelection = M.modify $ \m -> m & (#selectedUser .~ Nothing)
    update (SetHighlighted highlighted) =
      M.modify $ #highlighted .~ Set.fromList highlighted
    update (UpdateDocument (DocumentChange document _)) =
      M.modify $ #document .~ document
    update _ = pure ()

    view :: Model -> M.View Model Action
    view m =
      V.vBox_
        (V.Expand V.Start)
        (V.Expand V.Center)
        V.SmallGap
        [ title
        , description
        , userSelector
        , competences
        ]
      where
        title = V.title_ m.document.competenceGrid.title
        description = V.text_ m.document.competenceGrid.description
        userSelector = M.div_ [] M.+> userSelectorComponent r
        competences =
          V.viewTable $
            V.defTable
              { V.columns =
                  [ DescriptionColumn
                  ]
                    <> map LevelDescriptionColumn levels
              , V.rows = ordered m.document.competences
              , V.columnHeader =
                  \case
                    DescriptionColumn ->
                      C.translate'
                        C.LblCompetenceDescription
                    LevelDescriptionColumn l -> C.translate' $ C.LblCompetenceLevelDescription l
              , V.cellContents = \competence ->
                  let
                   in \case
                        DescriptionColumn -> V.text_ competence.description
                        LevelDescriptionColumn level -> V.text_ "..."
              }

    handleMail m =
      case fromJSON m of
        Success (UserSelectionChanged (Just u')) -> pure $ SelectUser (u' ^. #id)
        Success (UserSelectionChanged Nothing) -> pure CancelUserSelection
        _ -> Nothing

data Model = Model
  { document :: !Document
  , highlighted :: !(Set.Set CompetenceLevelId)
  , selectedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectUser !UserId
  | CancelUserSelection
  | SetHighlighted ![CompetenceLevelId]
  | ResourceDetailsOf ![CompetenceLevelId]
  | EvidenceDetailsOf ![CompetenceLevelId]
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

data Column
  = DescriptionColumn
  | LevelDescriptionColumn !Level
  deriving (Eq, Show)

emptyModel :: Model
emptyModel =
  Model
    { document = emptyDocument
    , highlighted = Set.empty
    , selectedUser = Nothing
    }

defaultSelection :: User -> Model -> Model
defaultSelection u m
  | u `elem` m.document.users = m & (#selectedUser ?~ u)
  | otherwise = m
