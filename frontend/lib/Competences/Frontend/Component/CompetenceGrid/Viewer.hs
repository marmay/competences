module Competences.Frontend.Component.CompetenceGrid.Viewer
  ( viewerDetailView
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceAssessment (..)
  , CompetenceAssessmentIxs
  , CompetenceGrid (..)
  , CompetenceIxs
  , Document (..)
  , EvidenceIxs
  , Level (..)
  , LevelInfo (..)
  , allLevels
  , ordered
  )
import Competences.Document.Evidence
  ( ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  )
import Competences.Document.Competence (CompetenceId)
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..))
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Table (TableCellSpec (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
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

-- | Projection type for the viewer - contains only the data needed for this view.
-- This is grid-specific: all data is pre-filtered for this grid and focused user.
data ViewerProjection = ViewerProjection
  { competences :: !(Ix.IxSet CompetenceIxs Competence)
  -- ^ Competences for this grid only
  , userEvidences :: !(Ix.IxSet EvidenceIxs Evidence)
  -- ^ Evidences for focused user only
  , userAssessments :: !(Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment)
  -- ^ Assessments for focused user only
  , activeGridGrade :: !(Maybe CompetenceGridGrade)
  -- ^ Pre-computed: most recent grid grade for this grid and focused user
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Model for the viewer detail component
data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  }
  deriving (Eq, Generic, Show)

-- | Action for the viewer detail component
data ViewerAction
  = ViewerProjectionChanged !(ProjectedChange ViewerProjection)
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
    { M.subs = [subscribeWithProjection r viewerProjection ViewerProjectionChanged]
    }
  where
    -- Projection function captures the grid parameter
    -- Pre-computes activeGridGrade so the view doesn't need to search
    viewerProjection :: Document -> Maybe User -> ViewerProjection
    viewerProjection doc mUser = ViewerProjection
      { competences = doc.competences Ix.@= grid.id
      , userEvidences = case mUser of
          Nothing -> Ix.empty
          Just u -> doc.evidences Ix.@= u.id
      , userAssessments = case mUser of
          Nothing -> Ix.empty
          Just u -> doc.competenceAssessments Ix.@= u.id
      , activeGridGrade = case mUser of
          Nothing -> Nothing
          -- Use IxSet indexing (@=) and toDescList for efficient sorted access
          Just u -> listToMaybe $ Ix.toDescList (Proxy @Day) $
            doc.competenceGridGrades Ix.@= u.id Ix.@= grid.id
      , focusedUser = mUser
      }

    emptyProjection = ViewerProjection Ix.empty Ix.empty Ix.empty Nothing Nothing
    model = ViewerModel emptyProjection

    update (ViewerProjectionChanged change) =
      M.modify $ #projection .~ change.projection

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
        proj = m.projection

        -- Header with title on left and grade badge on right
        header _vm =
          MH.div_
            [class_ "flex items-center justify-between w-full"]
            [ Typography.h2 (M.ms grid.title)
            , case proj.activeGridGrade of
                Just gridGrade -> gradeBadgeView gridGrade.grade
                Nothing -> V.empty
            ]

        description = Typography.paragraph (M.ms grid.description)

        competencesTable vm =
          let evidences = vm.projection.userEvidences
           in V.viewTable $
                V.defTable
                  { V.columns =
                      [ViewerDescriptionColumn]
                        <> map ViewerLevelColumn allLevels
                  , V.rows = ordered vm.projection.competences
                  , V.columnSpec = \case
                      ViewerDescriptionColumn ->
                        Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                      ViewerLevelColumn l ->
                        Table.TableColumnSpec Table.EqualWidthColumn (C.translate' $ C.LblCompetenceLevelDescription l)
                  , V.rowContents = V.cellContentsWithSpec $ \competence -> \case
                      ViewerDescriptionColumn ->
                        -- Description cell: green if achieved, yellow if not achieved, white if no assessment
                        -- userAssessments is already filtered to user (empty if no focused user)
                        let mAssessment = getActiveAssessment' proj.userAssessments competence.id
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

                            -- Get active assessment (userAssessments already filtered to focused user)
                            mAssessment = getActiveAssessment' proj.userAssessments competence.id

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

-- | Get the most recent (active) assessment for a user and competence.
-- Uses IxSet indexing for efficient lookup:
-- 1. Filter by userId and competenceId using @= (index-based)
-- 2. Get descending list by Day to find most recent
getActiveAssessment'
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> Maybe CompetenceAssessment
getActiveAssessment' assessments competenceId =
  -- assessments is already filtered to user, just filter by competenceId
  listToMaybe $ Ix.toDescList (Proxy @Day) $ assessments Ix.@= competenceId
