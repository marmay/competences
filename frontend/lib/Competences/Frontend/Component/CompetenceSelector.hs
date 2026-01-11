module Competences.Frontend.Component.EvidenceCompetenceSelector
  ( Model
  , Action (..)
  , emptyModel
  , subscriptions
  , update
  , view
  )
where

import Competences.Document (Competence (..), Document (..), Level (..), LevelInfo (..), allLevels, ordered)
import Competences.Document.Competence (CompetenceLevelId, competenceLevelIdsOf)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((%~), (.~))

data Model = Model
  { document :: ![Competence]
  , selected :: !(Set.Set CompetenceLevelId)
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleCompetence !Competence !V.TriState
  | ToggleCompetenceLevel !CompetenceLevelId !Bool
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

emptyModel :: Model
emptyModel =
  Model
    { document = []
    , selected = Set.empty
    }

subscriptions :: SyncContext -> [M.Sub Action]
subscriptions r = [subscribeDocument r UpdateDocument]

update :: Action -> M.Effect p Model Action
update (ToggleCompetence c V.TriStateOn) =
  M.modify (#selected %~ \s -> foldr Set.delete s [(c.id, l) | l <- allLevels])
update (ToggleCompetence c _) =
  M.modify (#selected %~ \s -> foldr Set.insert s $ competenceLevelIdsOf c)
update (ToggleCompetenceLevel l True) =
  M.modify (#selected %~ Set.delete l)
update (ToggleCompetenceLevel l False) =
  M.modify (#selected %~ Set.insert l)
update (UpdateDocument (DocumentChange newDocument _)) =
  M.modify (#document .~ ordered newDocument.competences)

data SelectionColumn
  = CompetenceDescriptionColumn
  | CompetenceLevelDescriptionColumn !Level
  deriving (Eq, Show, Ord)

view :: Model -> M.View Model Action
view m =
  V.viewTable $
    V.defTable
      { V.columns =
          [CompetenceDescriptionColumn] <> map CompetenceLevelDescriptionColumn allLevels
      , V.rows = m.document
      , V.columnHeader = \case
          CompetenceDescriptionColumn -> C.translate' C.LblCompetenceDescription
          CompetenceLevelDescriptionColumn l -> C.translate' $ C.LblCompetenceLevelDescription l
      , V.cellContents = \competence ->
          let levelsSelected :: Map.Map Level Bool
              levelsSelected =
                Map.fromList
                  [ (l, (competence.id, l) `Set.member` m.selected)
                  | l <- Map.keys competence.levels
                  ]
           in \case
                CompetenceDescriptionColumn ->
                  let s = V.toTriState $ Map.elems levelsSelected
                   in V.viewButton $
                        V.textButton
                          (M.ms competence.description)
                          s
                          (ToggleCompetence competence s)
                CompetenceLevelDescriptionColumn l ->
                  case l `Map.lookup` levelsSelected of
                    Just s ->
                      V.viewButton $
                        V.textButton
                          (M.ms (competence.levels Map.! l).description)
                          s
                          (ToggleCompetenceLevel (competence.id, l) s)
                    Nothing -> V.text_ ""
      }
