module Competences.Frontend.Component.CompetenceGrid.Viewer
  ( viewerDetailView
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Level (..)
  , emptyDocument
  , levels
  , ordered
  )
import Competences.Document.Evidence
  ( ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  )
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncContext
  , getFocusedUserRef
  , subscribeDocument
  , subscribeFocusedUser
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Svg.Property qualified as MSP
import Optics.Core ((&), (.~))

import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)

-- ============================================================================
-- VIEW MODE DETAIL
-- ============================================================================

-- | Model for the viewer detail component
data ViewerModel = ViewerModel
  { document :: !Document
  , focusedUser :: !(Maybe User)  -- From global focused user subscription
  }
  deriving (Eq, Generic, Show)

-- | Action for the viewer detail component
data ViewerAction
  = ViewerUpdateDocument !DocumentChange
  | ViewerFocusedUserChanged !FocusedUserChange
  deriving (Eq, Show)

-- | View for the viewer detail - shows competence grid with student evidence
viewerDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
viewerDetailView r grid =
  V.component
    ("competence-grid-viewer-" <> M.ms (show grid.id))
    (viewerComponent r grid)

viewerComponent :: SyncContext -> CompetenceGrid -> M.Component p ViewerModel ViewerAction
viewerComponent r grid =
  (M.component model update view)
    { M.subs =
        [ subscribeDocument r ViewerUpdateDocument
        , subscribeFocusedUser (getFocusedUserRef r) ViewerFocusedUserChanged
        ]
    }
  where
    model = ViewerModel emptyDocument Nothing

    update (ViewerUpdateDocument (DocumentChange doc _)) =
      M.modify $ #document .~ doc

    update (ViewerFocusedUserChanged change) =
      M.modify $ #focusedUser .~ change.user

    view m =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ header
        , description
        , competencesTable m
        ]
      where
        -- User selector removed - now uses global focused user from nav bar
        header = Typography.h2 (M.ms grid.title)

        description = Typography.paragraph (M.ms grid.description)

        competencesTable vm =
          let evidences = case vm.focusedUser of
                Just user -> vm.document.evidences Ix.@= user.id
                Nothing -> Ix.empty
           in V.viewTable $
                V.defTable
                  { V.columns =
                      [ViewerDescriptionColumn]
                        <> map ViewerLevelColumn levels
                  , V.rows = ordered (vm.document.competences Ix.@= grid.id)
                  , V.columnSpec = \case
                      ViewerDescriptionColumn ->
                        Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                      ViewerLevelColumn l ->
                        Table.TableColumnSpec Table.EqualWidthColumn (C.translate' $ C.LblCompetenceLevelDescription l)
                  , V.rowContents = V.cellContents $ \competence -> \case
                      ViewerDescriptionColumn -> Typography.small (M.ms competence.description)
                      ViewerLevelColumn level ->
                        let competenceLevelId = (competence.id, level)
                            levelDescription = M.ms $ fromMaybe "" (competence.levelDescriptions Map.!? level)
                            evidences' = evidences Ix.@= competenceLevelId
                            showEvidence evidence =
                              case Ix.getOne (evidence.observations Ix.@= competenceLevelId) of
                                Just observation ->
                                  showSummary evidence.activityType observation.socialForm observation.ability
                                Nothing -> V.empty
                            showSummary activityType socialForm ability =
                              let abilityClass = Colors.abilityTextClass ability
                                  activityTypeIcn = case activityType of
                                    Conversation -> IcnActivityTypeConversation
                                    Exam -> IcnActivityTypeExam
                                    SchoolExercise -> IcnActivityTypeSchoolExercise
                                    HomeExercise -> IcnActivityTypeHomeExercise
                                  socialFormIcn = case socialForm of
                                    Group -> IcnSocialFormGroup
                                    Individual -> IcnSocialFormIndividual
                                  coloredIcon icn = MH.span_ [class_ abilityClass] [V.icon [MSP.stroke_ "currentColor"] icn]
                               in V.viewFlow V.hFlow [coloredIcon i | i <- [activityTypeIcn, socialFormIcn]]
                         in V.viewFlow
                              (V.vFlow & (#expandOrthogonal .~ V.Expand V.Start))
                              [ Typography.small levelDescription
                              , V.viewFlow
                                  (V.hFlow & (#gap .~ V.SmallSpace) & (#expandDirection .~ V.Expand V.Start))
                                  (V.flowSpring : map showEvidence (Ix.toAscList (Proxy @Day) evidences'))
                              ]
                  }

data ViewerColumn
  = ViewerDescriptionColumn
  | ViewerLevelColumn !Level
  deriving (Eq, Show)
