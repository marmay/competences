module Competences.Frontend.GridEditor.Action
  ( Action (..)
  )
where

import Competences.Command (Command)
import Competences.Document.ChangableField (ChangableField)
import Competences.Frontend.Component.CompetenceEditor qualified as CE
import Competences.Frontend.SyncDocument (DocumentChange)
import GHC.Generics (Generic)
import Miso.String (MisoString)

data Action where
  EditField :: !ChangableField -> !MisoString -> Action
  UpdateDocument :: !DocumentChange -> Action
  IssueCommand :: !Command -> Action
  SpawnNewCompetenceEditor :: Action
  NewCompetenceEditorAction :: !CE.Action -> Action
  Log :: !MisoString -> Action
  deriving (Eq, Show, Generic)
