module Competences.Frontend.GridEditor.Action
  ( Action (..)
  , InMail (..)
  )
where

import Competences.Command (Command)
import Competences.Document.ChangableField (ChangableField)
import Competences.Document.Competence (Competence)
import Competences.Frontend.CompetenceEditor qualified as CE
import Competences.Frontend.SyncDocument (DocumentChange)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Miso.String (MisoString)
import Miso (ComponentId)

data Action where
  Initialize :: Action
  EditField :: !ChangableField -> !MisoString -> Action
  UpdateDocument :: !DocumentChange -> Action
  IssueCommand :: !Command -> Action
  NewCompetenceEditor :: Action
  NewCompetenceEditorMounted :: ComponentId -> Action
  CancelNewCompetence :: Action
  AddNewCompetence :: !Competence -> Action
  Log :: !MisoString -> Action
  deriving (Eq, Show, Generic)

data InMail
  = CompetenceEditor CE.OutMail
  deriving (Eq, Show, Generic)

instance FromJSON InMail

instance ToJSON InMail
