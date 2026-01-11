module Competences.Frontend.Component.EvidenceCompetenceSelector
  ( Model
  , Action (..)
  , evidenceCompetenceSelector
  )
where

import Competences.Document (Competence (..), Document (..), Level (..), LevelInfo (..), allLevels, ordered)
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.Evidence (Ability, SocialForm (..), abilities, socialForms)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens', lens, toLensVL, (%~), (&), (.~))

data Model = Model
  { competences :: ![Competence]
  , selected :: !(Map.Map CompetenceLevelId (SocialForm, Ability))
  }
  deriving (Eq, Generic, Show)

data Action
  = ToggleCompetenceLevel !CompetenceLevelId !(SocialForm, Ability)
  | UpdateDocument !DocumentChange
  deriving (Eq, Generic, Show)

data SelectionColumn
  = CompetenceDescriptionColumn
  | CompetenceLevelDescriptionColumn !Level
  deriving (Eq, Show, Ord)

flatSelectedL :: Lens' Model [(CompetenceLevelId, SocialForm, Ability)]
flatSelectedL = lens getter setter
  where
    getter m = flatten <$> Map.toList m.selected
    setter m l = m & (#selected .~ Map.fromList (unflatten <$> l))
    flatten (l, (s, a)) = (l, s, a)
    unflatten (l, s, a) = (l, (s, a))

evidenceCompetenceSelector
  :: SyncContext -> Lens' p [(CompetenceLevelId, SocialForm, Ability)] -> M.Component p Model Action
evidenceCompetenceSelector r parentLens =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    , M.bindings = [toLensVL parentLens M.<--- toLensVL flatSelectedL]
    }
  where
    model =
      Model
        { competences = []
        , selected = Map.empty
        }

    update (ToggleCompetenceLevel l sa) =
      M.modify (#selected %~ update')
      where
        update' m =
          case l `Map.lookup` m of
            Just sa' ->
              if sa == sa'
                then Map.delete l m
                else Map.insert l sa (Map.delete l m)
            Nothing -> Map.insert l sa m
    update (UpdateDocument (DocumentChange newDocument _)) =
      M.modify (#competences .~ ordered newDocument.competences)

    view m =
      V.viewTable $
        V.defTable
          { V.columns =
              [CompetenceDescriptionColumn] <> map CompetenceLevelDescriptionColumn allLevels
          , V.rows = m.competences
          , V.columnSpec = \case
              CompetenceDescriptionColumn ->
                V.TableColumnSpec V.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
              CompetenceLevelDescriptionColumn l ->
                V.TableColumnSpec V.AutoSizedColumn (C.translate' (C.LblCompetenceLevelDescription l))
          , V.rowContents = V.cellContents $ \competence ->
              \case
                CompetenceDescriptionColumn -> V.text_ (M.ms competence.description)
                CompetenceLevelDescriptionColumn l ->
                  case competence.levels Map.!? l of
                    Nothing -> V.empty
                    Just levelInfo ->
                      V.viewFlow
                        V.vFlow
                        [ V.text_ (M.ms levelInfo.description)
                        , V.flowSpring
                        , allButtons competence l
                        ]
          }
      where
        allButtons :: Competence -> Level -> M.View Model Action
        allButtons competence level =
          V.viewFlow
            (V.vFlow & (#gap .~ V.SmallSpace) & (#expandOrthogonal .~ V.Expand V.Center))
            (map (socialFormButtons competence level) socialForms)
        socialFormButtons :: Competence -> Level -> SocialForm -> M.View Model Action
        socialFormButtons competence level socialForm =
          V.viewFlow
            (V.hFlow & (#expandDirection .~ V.Expand V.Center))
            ( V.icon [] (socialFormIcon socialForm)
                : map (abilityButton competence level socialForm) abilities
            )
        abilityButton competence level socialForm ability =
          Button.toggleButton
            (m.selected Map.!? (competence.id, level) == Just (socialForm, ability))
            (C.translate' (C.LblAbility ability))
            (ToggleCompetenceLevel (competence.id, level) (socialForm, ability))
        socialFormIcon Individual = V.IcnSocialFormIndividual
        socialFormIcon Group = V.IcnSocialFormGroup
