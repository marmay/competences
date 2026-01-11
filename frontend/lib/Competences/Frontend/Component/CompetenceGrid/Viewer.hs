module Competences.Frontend.Component.CompetenceGrid.Viewer
  ( viewerDetailView
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceAssessment (..)
  , CompetenceGrid (..)
  , Document (..)
  , Level (..)
  , LevelInfo (..)
  , allLevels
  , emptyDocument
  , getActiveAssessment
  , getActiveGridGrade
  , ordered
  )
import Competences.Document.Evidence
  ( ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  )
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..))
import Competences.Document.Grade (Grade (..))
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncContext
  , getFocusedUserRef
  , subscribeDocument
  , subscribeFocusedUser
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Table (TableCellSpec (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
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
        [ header m
        , description
        , competencesTable m
        ]
      where
        -- Header with title on left and grade badge on right
        header vm =
          MH.div_
            [class_ "flex items-center justify-between w-full"]
            [ Typography.h2 (M.ms grid.title)
            , case vm.focusedUser of
                Just user ->
                  case getActiveGridGrade vm.document user.id grid.id of
                    Just gridGrade -> gradeBadgeView gridGrade.grade
                    Nothing -> V.empty
                Nothing -> V.empty
            ]

        description = Typography.paragraph (M.ms grid.description)

        competencesTable vm =
          let evidences = case vm.focusedUser of
                Just user -> vm.document.evidences Ix.@= user.id
                Nothing -> Ix.empty
           in V.viewTable $
                V.defTable
                  { V.columns =
                      [ViewerDescriptionColumn]
                        <> map ViewerLevelColumn allLevels
                  , V.rows = ordered (vm.document.competences Ix.@= grid.id)
                  , V.columnSpec = \case
                      ViewerDescriptionColumn ->
                        Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                      ViewerLevelColumn l ->
                        Table.TableColumnSpec Table.EqualWidthColumn (C.translate' $ C.LblCompetenceLevelDescription l)
                  , V.rowContents = V.cellContentsWithSpec $ \competence -> \case
                      ViewerDescriptionColumn ->
                        -- Description cell: green if achieved, yellow if not achieved, white if no assessment
                        let mAssessment = case vm.focusedUser of
                              Just user -> getActiveAssessment vm.document user.id competence.id
                              Nothing -> Nothing
                            bgClass = case mAssessment of
                              Nothing -> "" -- No assessment: white
                              Just assessment -> case assessment.level of
                                Nothing -> "bg-yellow-100" -- Not achieved at all
                                Just _ -> "bg-green-100" -- Achieved at some level
                         in TableCellSpec
                              { cellClasses = "px-4 py-3 " <> bgClass
                              , cellStyle = []
                              , cellContent = Typography.small (M.ms competence.description)
                              }
                      ViewerLevelColumn level ->
                        let levelInfo = Map.findWithDefault (LevelInfo T.empty False) level competence.levels
                            competenceLevelId = (competence.id, level)
                            evidences' = evidences Ix.@= competenceLevelId
                            evidenceList = Ix.toAscList (Proxy @Day) evidences'
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
                            hasDescription = not (T.null levelInfo.description)

                            -- Get active assessment for focused user + competence
                            mAssessment = case vm.focusedUser of
                              Just user -> getActiveAssessment vm.document user.id competence.id
                              Nothing -> Nothing

                            -- Determine cell assessment status
                            cellStatus :: CellAssessmentStatus
                            cellStatus = case mAssessment of
                              Nothing -> NoAssessment -- Fall back to locked/normal
                              Just assessment -> case assessment.level of
                                Nothing -> NotYetAchieved -- Assessed but not achieved
                                Just assessedLevel ->
                                  if level <= assessedLevel
                                    then Achieved
                                    else NotYetAchieved

                            -- Cell background color based on status
                            -- Only "Achieved" overrides locked; "NotYetAchieved" does not
                            bgClass
                              | not hasDescription = "" -- Empty: will use striped background
                              | cellStatus == Achieved = "bg-green-100"
                              | levelInfo.locked = "bg-stone-200" -- Locked takes precedence over NotYetAchieved
                              | cellStatus == NotYetAchieved = "bg-yellow-100"
                              | otherwise = "bg-white" -- Normal: white

                            -- Striped background for empty cells
                            stripeStyle :: [(M.MisoString, M.MisoString)]
                            stripeStyle =
                              if not hasDescription
                                then
                                  [ ("background",
                                     "repeating-linear-gradient(135deg, rgb(245 245 244) 0px, rgb(245 245 244) 4px, rgb(231 229 228) 4px, rgb(231 229 228) 8px)")
                                  ]
                                else []

                            -- Status icon in top-right corner
                            -- No icons on cells without description
                            -- Only "Achieved" overrides locked status
                            statusIcon
                              | not hasDescription = V.empty
                              | cellStatus == Achieved =
                                  MH.div_
                                    [class_ "absolute top-1 right-1 text-green-600"]
                                    [icon [MP.width_ "14", MP.height_ "14"] IcnApply]
                              | levelInfo.locked =
                                  MH.div_
                                    [class_ "absolute top-1 right-1 text-stone-500"]
                                    [icon [MP.width_ "14", MP.height_ "14"] IcnLock]
                              | cellStatus == NotYetAchieved =
                                  MH.div_
                                    [class_ "absolute top-1 right-1 text-yellow-600"]
                                    [icon [MP.width_ "14", MP.height_ "14"] IcnProgress]
                              | otherwise = V.empty

                            -- Cell classes: relative for icon positioning, padding, and vertical centering
                            tdClasses = "relative px-4 py-3 " <> bgClass
                            -- Cell content wrapper for vertical centering
                            cellContent =
                              MH.div_
                                [class_ "flex flex-col justify-center min-h-[44px]"]
                                [ statusIcon
                                , -- Description text (only if present)
                                  if hasDescription
                                    then Typography.small (M.ms levelInfo.description)
                                    else V.empty
                                , -- Evidence icons (wrap to multiple lines as needed)
                                  if not (null evidenceList)
                                    then
                                      MH.div_
                                        [class_ "flex flex-wrap gap-1 mt-1"]
                                        (map showEvidence evidenceList)
                                    else V.empty
                                ]
                         in TableCellSpec
                              { cellClasses = tdClasses
                              , cellStyle = stripeStyle
                              , cellContent = cellContent
                              }
                  }

data ViewerColumn
  = ViewerDescriptionColumn
  | ViewerLevelColumn !Level
  deriving (Eq, Show)

-- | Assessment status for a cell in the viewer
data CellAssessmentStatus
  = Achieved       -- ^ Cell level is at or below the assessed level
  | NotYetAchieved -- ^ Cell level is above the assessed level, or assessment is "Not Achieved"
  | NoAssessment   -- ^ No assessment exists for this competence
  deriving (Eq, Show)

-- | Create a colored badge for a grade
-- Color coding: 1-3 green, 3-4/4/4-5 yellow, 5 red
gradeBadgeView :: Grade -> M.View m action
gradeBadgeView g =
  let (bgClass, textClass) = gradeColorClasses g
      shortLabel = gradeShortLabel g
   in MH.span_
        [ class_ $ "inline-flex items-center justify-center rounded-full px-2.5 py-1 text-sm font-medium " <> bgClass <> " " <> textClass
        ]
        [M.text (M.ms shortLabel)]

-- | Get background and text color classes for a grade
gradeColorClasses :: Grade -> (T.Text, T.Text)
gradeColorClasses g = case g of
  Grade1 -> ("bg-green-100", "text-green-700")
  Grade1_2 -> ("bg-green-100", "text-green-700")
  Grade2 -> ("bg-green-100", "text-green-700")
  Grade2_3 -> ("bg-green-100", "text-green-700")
  Grade3 -> ("bg-green-100", "text-green-700")
  Grade3_4 -> ("bg-yellow-100", "text-yellow-700")
  Grade4 -> ("bg-yellow-100", "text-yellow-700")
  Grade4_5 -> ("bg-yellow-100", "text-yellow-700")
  Grade5 -> ("bg-red-100", "text-red-700")

-- | Short label for grade (just the number part)
gradeShortLabel :: Grade -> T.Text
gradeShortLabel g = case g of
  Grade1 -> "1"
  Grade1_2 -> "1-2"
  Grade2 -> "2"
  Grade2_3 -> "2-3"
  Grade3 -> "3"
  Grade3_4 -> "3-4"
  Grade4 -> "4"
  Grade4_5 -> "4-5"
  Grade5 -> "5"
