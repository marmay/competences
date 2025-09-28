module Competences.Frontend.Component.CompetenceGridViewer
  ( competenceGridViewerComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Common.IxSet qualified as Ix
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
import Competences.Document.Evidence (Evidence)
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.UserSelector (UserSelectorMessage (..), userSelectorComponent)
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument)
import Competences.Frontend.View qualified as V
import Data.Aeson (Result (..), fromJSON)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (ix, (%), (&), (.~), (?~), (^.), (^?))

competenceGridViewerComponent :: SyncDocumentRef -> User -> M.Component p Model Action
competenceGridViewerComponent r u =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = emptyModel

    update :: Action -> M.Effect p Model Action
    update (SetHighlighted highlighted) =
      M.modify $ #highlighted .~ Set.fromList highlighted
    update (UpdateDocument (DocumentChange document _)) =
      M.modify $ #document .~ document
    update _ = pure ()

    view :: Model -> M.View Model Action
    view m =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ title
        , description
        , userSelector
        , competences
        ]
      where
        title = V.title_ (M.ms m.document.competenceGrid.title)
        description = V.text_ (M.ms m.document.competenceGrid.description)
        userSelector = M.div_ [] M.+> userSelectorComponent r #selectedUser
        evidences = case m.selectedUser of
          Just user -> m.document.evidences Ix.@= user.id
          Nothing -> Ix.empty
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
                        DescriptionColumn -> V.text_ (M.ms competence.description)
                        LevelDescriptionColumn level -> V.text_ "..."
              }

data Model = Model
  { document :: !Document
  , highlighted :: !(Set.Set CompetenceLevelId)
  , selectedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

data Action
  = SetHighlighted ![CompetenceLevelId]
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
