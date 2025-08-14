module Competences.Frontend.Component.CompetenceGridViewer
  ( Model (..)
  , Action (..)
  , emptyModel
  , update
  , view
  )
where

import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , User
  , UserId
  , emptyDocument
  , ordered, levels
  )
import Competences.Document.Competence (CompetenceLevelId, Level (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (DocumentChange (..))
import Competences.Frontend.View qualified as V
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (ix, (%), (&), (.~), (^?))

data Model = Model
  { document :: !Document
  , highlighted :: !(Set.Set CompetenceLevelId)
  , selectedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

emptyModel :: Model
emptyModel =
  Model
    { document = emptyDocument
    , highlighted = Set.empty
    , selectedUser = Nothing
    }

data Action
  = SelectUser !UserId
  | SetHighlighted ![CompetenceLevelId]
  | ResourceDetailsOf ![CompetenceLevelId]
  | EvidenceDetailsOf ![CompetenceLevelId]
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

update :: Action -> M.Effect p Model Action
update (SelectUser userId) =
  M.modify $ \m -> m & (#selectedUser .~ (m ^? (#document % #users % ix userId)))
update (SetHighlighted highlighted) =
  M.modify $ #highlighted .~ Set.fromList highlighted
update (UpdateDocument (DocumentChange document _)) =
  M.modify $ #document .~ document
update _ = pure ()

data Column
  = DescriptionColumn
  | LevelDescriptionColumn !Level
  deriving (Eq, Show)

view :: Model -> M.View Model Action
view m =
  V.vBox_
    (V.Expand V.Start)
    (V.Expand V.Center)
    V.SmallGap
    [ title
    , description
    , competences
    ]
  where
    title = V.title_ m.document.competenceGrid.title
    description = V.text_ m.document.competenceGrid.description
    competences =
      V.viewTable $
        V.defTable
          { V.columns =
              [ DescriptionColumn
              ] <> map LevelDescriptionColumn levels
          , V.rows = ordered m.document.competences
          , V.columnHeader = \case
              DescriptionColumn -> C.translate' C.LblCompetenceDescription
              LevelDescriptionColumn l -> C.translate' $ C.LblCompetenceLevelDescription l
          , V.cellContents = \competence ->
              let
               in \case
                    DescriptionColumn -> V.text_ competence.description
                    LevelDescriptionColumn level -> V.text_ "..."
          }
