module Competences.Frontend.Component.CompetenceSelector
  ( Model
  , Action (..)
  , emptyModel
  , subscriptions
  , update
  , view
  )
where

import Competences.Document (Competence (..), Document (..), Level (..), levels, ordered)
import Competences.Document.Competence (CompetenceLevelId, competenceLevelIdsOf)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef, subscribeDocument)
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
  | ToggleCompetenceLevel !CompetenceLevelId !V.ToggleState
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

emptyModel :: Model
emptyModel =
  Model
    { document = []
    , selected = Set.empty
    }

subscriptions :: SyncDocumentRef -> [M.Sub Action]
subscriptions r = [subscribeDocument r UpdateDocument]

update :: Action -> M.Effect p Model Action
update (ToggleCompetence c V.TriStateOn) =
  M.modify (#selected %~ \s -> foldr Set.delete s [(c.id, l) | l <- levels])
update (ToggleCompetence c _) =
  M.modify (#selected %~ \s -> foldr Set.insert s $ competenceLevelIdsOf c)
update (ToggleCompetenceLevel l V.ToggleOn) =
  M.modify (#selected %~ Set.delete l)
update (ToggleCompetenceLevel l V.ToggleOff) =
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
          [CompetenceDescriptionColumn] <> map CompetenceLevelDescriptionColumn levels
      , V.rows = m.document
      , V.columnHeader = \case
          CompetenceDescriptionColumn -> C.translate' C.LblCompetenceDescription
          CompetenceLevelDescriptionColumn l -> C.translate' $ C.LblCompetenceLevelDescription l
      , V.cellContents = \competence ->
          let levelsSelected :: Map.Map Level V.ToggleState
              levelsSelected =
                Map.fromList
                  [ (l, V.toToggleState $ (competence.id, l) `Set.member` m.selected)
                  | l <- Map.keys competence.levelDescriptions
                  ]
           in \case
                CompetenceDescriptionColumn ->
                  V.triStateButton
                    (ToggleCompetence competence)
                    (V.toTriState $ Map.elems levelsSelected)
                    (V.text_ competence.description)
                CompetenceLevelDescriptionColumn l ->
                  case l `Map.lookup` levelsSelected of
                    Just s ->
                      V.toggleButton
                        (ToggleCompetenceLevel (competence.id, l))
                        s
                        (V.text_ $ competence.levelDescriptions Map.! l)
                    Nothing -> V.text_ ""
      }
