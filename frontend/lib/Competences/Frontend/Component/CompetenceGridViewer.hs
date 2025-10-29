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
  , UserRole (..)
  , emptyDocument
  , levels
  , ordered
  )
import Competences.Document.Competence (CompetenceLevelId, Level (..))
import Competences.Document.User (User (..), isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Common (selectorLens)
import Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridSelectorComponent
  )
import Competences.Frontend.Component.Selector.UserSelector
  ( SingleUserSelectorStyle (SingleUserSelectorStyleButtons)
  , UserSelectorConfig (..)
  , defaultUserSelectorConfig
  , singleUserSelectorComponent
  )
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Table qualified as C
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((&), (.~))

competenceGridViewerComponent :: SyncDocumentRef -> M.Component p Model Action
competenceGridViewerComponent r =
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
        (V.hFlow & (#expandDirection .~ V.Expand V.Start) & (#expandOrthogonal .~ V.Expand V.Start))
        [ V.component
            "competence-grid-viewer-selection"
            (competenceGridSelectorComponent r CompetenceGridSelectorViewOnlyStyle #competenceGrid)
        , V.flexGrow (viewCompetenceGrid m)
        ]

    viewCompetenceGrid :: Model -> M.View Model Action
    viewCompetenceGrid m =
      case m.competenceGrid of
        Just competenceGrid -> viewCompetenceGrid' m.document m.selectedUser competenceGrid
        Nothing -> V.text_ "Bitte wähle einen Kompetenzraster aus."
    viewCompetenceGrid' :: Document -> Maybe User -> CompetenceGrid -> M.View Model Action
    viewCompetenceGrid' document selectedUser competenceGrid =
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
        title = V.title_ (M.ms competenceGrid.title)
        description = V.text_ (M.ms competenceGrid.description)
        userSelector =
          V.component'
            ( singleUserSelectorComponent
                r
                defaultUserSelectorConfig {isPossibleUser = isStudent}
                SingleUserSelectorStyleButtons
                (selectorLens #selectedUser)
            )
        evidences = case selectedUser of
          Just user -> document.evidences Ix.@= user.id
          Nothing -> Ix.empty
        competences =
          V.viewTable $
            V.defTable
              { V.columns =
                  [ DescriptionColumn
                  ]
                    <> map LevelDescriptionColumn levels
              , V.rows = ordered document.competences
              , V.columnSpec = \case
                  DescriptionColumn ->
                    C.TableColumnSpec C.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                  LevelDescriptionColumn l ->
                    C.TableColumnSpec C.AutoSizedColumn (C.translate' $ C.LblCompetenceLevelDescription l)
              , V.rowContents = V.cellContents $ \competence -> \case
                  DescriptionColumn -> V.text_ (M.ms competence.description)
                  LevelDescriptionColumn level -> V.text_ "..."
              }

data Model = Model
  { document :: !Document
  , highlighted :: !(Set.Set CompetenceLevelId)
  , competenceGrid :: !(Maybe CompetenceGrid)
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
    , competenceGrid = Nothing
    , selectedUser = Nothing
    }
