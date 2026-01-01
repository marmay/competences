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
  , emptyDocument
  , levels
  , ordered
  )
import Competences.Document.Competence (CompetenceLevelId, Level (..))
import Competences.Document.Evidence
  ( Ability (..)
  , ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  )
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
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Svg.Property qualified as MSP
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
        Nothing -> Typography.paragraph "Bitte wähle einen Kompetenzraster aus."
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
        title = Typography.h2 (M.ms competenceGrid.title)
        description = Typography.paragraph (M.ms competenceGrid.description)
        userSelector =
          V.component
            "competence-grid-viewer-user-selector"
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
              , V.rows = ordered (document.competences Ix.@= competenceGrid.id)
              , V.columnSpec = \case
                  DescriptionColumn ->
                    C.TableColumnSpec C.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                  LevelDescriptionColumn l ->
                    C.TableColumnSpec C.AutoSizedColumn (C.translate' $ C.LblCompetenceLevelDescription l)
              , V.rowContents = V.cellContents $ \competence -> \case
                  DescriptionColumn -> Typography.small (M.ms competence.description)
                  LevelDescriptionColumn level ->
                    let competenceLevelId = (competence.id, level)
                        levelDescription = M.ms $ fromMaybe "" (competence.levelDescriptions Map.!? level)
                        evidences' = evidences Ix.@= competenceLevelId
                        showEvidence evidence =
                          case Ix.getOne (evidence.observations Ix.@= competenceLevelId) of
                            Just observation ->
                              showSummary evidence.activityType observation.socialForm observation.ability
                            Nothing -> V.empty
                        showSummary activityType socialForm ability =
                          let color = case ability of
                                SelfReliant -> "#00743f"
                                SelfReliantWithSillyMistakes -> "#42ab49"
                                WithSupport -> "#f2a104"
                                NotYet -> "#f25117"
                              activityTypeIcn = case activityType of
                                Conversation -> V.IcnActivityTypeConversation
                                Exam -> V.IcnActivityTypeExam
                                SchoolExercise -> V.IcnActivityTypeSchoolExercise
                                HomeExercise -> V.IcnActivityTypeHomeExercise
                              socialFormIcn = case socialForm of
                                Group -> V.IcnSocialFormGroup
                                Individual -> V.IcnSocialFormIndividual
                           in V.viewFlow
                                V.hFlow
                                [V.icon [MSP.stroke_ color] i | i <- [activityTypeIcn, socialFormIcn]]
                     in V.viewFlow
                          (V.vFlow & (#expandOrthogonal .~ V.Expand V.Start))
                          [ Typography.small levelDescription
                          , V.viewFlow
                              (V.hFlow & (#gap .~ V.SmallSpace) & (#expandDirection .~ V.Expand V.Start))
                              (V.flowSpring : map showEvidence (Ix.toAscList (Proxy @Day) evidences'))
                          ]
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
