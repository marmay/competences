module Competences.Frontend.Page.EditCompetenceGridPage
  ( editCompetenceGridPage
  , EditCompetenceGridPage
  , EditCompetenceGridView
  )
where

import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , User
  , orderMax
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGridEditor (competenceGridEditorComponent)
import Competences.Frontend.SyncDocument
  ( SyncDocument (..)
  , SyncDocumentRef
  , readSyncDocument
  )
import Competences.Frontend.View qualified as V
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%), (&), (.~), (?~), (^.))
import System.Random (randomIO)

newtype Model = Model
  { newCompetence :: Maybe Competence
  }
  deriving (Eq, Generic, Show)

data CloseId = CloseNewCompetenceEditor
  deriving (Eq, Generic, Show)

instance ToJSON CloseId

instance FromJSON CloseId

data Action
  = SpawnNewCompetenceEditor
  | SpawnNewCompetenceEditor' !Competence
  | HandleClose !CloseId
  | NoOp
  deriving (Eq, Generic, Show)

type EditCompetenceGridPage p = M.Component p Model Action

type EditCompetenceGridView = M.View Model Action

editCompetenceGridPage
  :: SyncDocumentRef -> M.Component p Model Action
editCompetenceGridPage r =
  (M.component model update view)
    { M.mailbox = M.checkMail HandleClose (const NoOp)
    }
  where
    model =
      Model
        { newCompetence = Nothing
        }

    update SpawnNewCompetenceEditor =
      M.withSink $ \sink -> do
        competenceId <- randomIO
        document <- readSyncDocument r
        let competenceGridId = document ^. #localDocument % #competenceGrid % #id
        sink $
          SpawnNewCompetenceEditor' $
            Competence
              { id = competenceId
              , competenceGridId = competenceGridId
              , description = ""
              , levelDescriptions = Map.empty
              , order = orderMax
              }
    update (SpawnNewCompetenceEditor' c) = M.modify (#newCompetence ?~ c)
    update (HandleClose CloseNewCompetenceEditor) = M.modify (#newCompetence .~ Nothing)
    update NoOp = pure ()

    view m =
      V.viewFlow
        ( V.vFlow
            & (#expandOrthogonal .~ V.Expand V.Start)
            & (#gap .~ V.LargeSpace)
        )
        [ M.div_ [] M.+> competenceGridEditorComponent r
        , V.viewFlow
            ( V.hFlow
                & (#expandDirection .~ V.Expand V.End)
            )
            [V.viewButton $ V.iconLabelButton' V.IcnAdd C.LblAddNewCompetence SpawnNewCompetenceEditor]
        ]
