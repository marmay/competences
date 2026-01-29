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
  , Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  , Task (..)
  , allLevels
  , ordered
  )
import Competences.Document.Evidence
  ( ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  )
import Competences.Document.Competence (CompetenceId, CompetenceLevelId)
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..))
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskIdentifier (..)
  , getTaskAttributes
  , getTaskContent
  , getTaskPrimaryCompetences
  , isResourceTask
  )
import Competences.Document.User (User (..), UserRole (..))
import Competences.Query.Competence qualified as QCompetence
import Competences.Query.Evidence qualified as QEvidence
import Competences.Query.User qualified as QUser
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ResourceModal qualified as ResourceModal
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.TaskResourceList (TaskWithSolutions (..))
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , SyncDocumentEnv (..)
  , openModal
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Table (TableCellSpec (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Mastery
  ( MasteryStatus (..)
  , getClassMasteryStats
  , getClassMasteryWithStudents
  )
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.Svg.Property qualified as MSP
import Optics.Core ((&), (.~))

import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)

-- ============================================================================
-- PROJECTION TYPES
-- ============================================================================

-- | Common data shared between user view and analytics view
data ViewerProjection = ViewerProjection
  { competences :: !(Ix.IxSet CompetenceIxs Competence)
  -- ^ Competences for this grid only
  , resourceTasks :: !(Map CompetenceLevelId [TaskWithSolutions])
  -- ^ Tasks with displayInResources=true, grouped by primary competence level
  , learningResources :: !(Map CompetenceLevelId [Resource])
  -- ^ Learning resources grouped by competence level
  , connectedUserRole :: !UserRole
  -- ^ Role of the connected user (for conditional display)
  , viewData :: !ViewData
  -- ^ Either user-specific data or class-wide analytics
  }
  deriving (Eq, Generic, Show)

-- | View-specific data: either focused on one user or showing class analytics
data ViewData
  = UserViewData !UserData
  -- ^ Focused on a specific student
  | AnalyticsViewData !AnalyticsData
  -- ^ Class-wide analytics (no focused user)
  deriving (Eq, Generic, Show)

-- | Data for viewing a specific student's progress
data UserData = UserData
  { focusedUser :: !User
  , userEvidences :: !(Ix.IxSet EvidenceIxs Evidence)
  -- ^ Evidences for focused user only
  , userAssessments :: !(Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment)
  -- ^ Assessments for focused user only
  , activeGridGrade :: !(Maybe CompetenceGridGrade)
  -- ^ Pre-computed: most recent grid grade for this grid and focused user
  }
  deriving (Eq, Generic, Show)

-- | Data for class-wide analytics view
data AnalyticsData = AnalyticsData
  { totalStudents :: !Int
  -- ^ Total number of students in the class
  , masteryStats :: !(Map CompetenceLevelId (Map MasteryStatus Int))
  -- ^ Pre-computed mastery statistics per competence level (for counts)
  , masteryStudents :: !(Map CompetenceLevelId (Map MasteryStatus [User]))
  -- ^ Pre-computed student lists per mastery status (for tooltips), sorted alphabetically
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
  | OpenResourceModal !CompetenceLevelId
  deriving (Eq, Show)

-- ============================================================================
-- COMPONENT
-- ============================================================================

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
    { M.subs = [subscribeWithProjection r (viewerProjection connectedRole) ViewerProjectionChanged]
    }
  where
    -- Capture connected user role from SyncContext
    connectedRole = (syncDocumentEnv r).connectedUser.role

    -- Projection function captures the grid parameter and connected user role
    viewerProjection :: UserRole -> Document -> Maybe User -> ViewerProjection
    viewerProjection role doc mUser =
      let gridCompetences = QCompetence.gridCompetences doc grid.id
          -- Compute view-specific data based on focused user
          vData = case mUser of
            Nothing ->
              -- No focused user: compute analytics
              let students = QUser.students doc
                  -- Pre-compute mastery stats for all competence levels in this grid
                  competenceLevels =
                    [ (c.id, level)
                    | c <- Ix.toList gridCompetences
                    , level <- allLevels
                    , let levelInfo = Map.findWithDefault (LevelInfo T.empty False) level c.levels
                    , not (T.null levelInfo.description)
                    ]
                  stats = Map.fromList
                    [ (clId, getClassMasteryStats doc clId)
                    | clId <- competenceLevels
                    ]
                  studentLists = Map.fromList
                    [ (clId, getClassMasteryWithStudents doc clId)
                    | clId <- competenceLevels
                    ]
               in AnalyticsViewData $ AnalyticsData
                    { totalStudents = length students
                    , masteryStats = stats
                    , masteryStudents = studentLists
                    }
            Just u ->
              -- Focused user: compute user-specific data
              UserViewData $ UserData
                { focusedUser = u
                , userEvidences = QEvidence.userEvidences doc u.id
                , userAssessments = doc.competenceAssessments Ix.@= u.id
                , activeGridGrade = listToMaybe $ Ix.toDescList (Proxy @Day) $
                    doc.competenceGridGrades Ix.@= u.id Ix.@= grid.id
                }
       in ViewerProjection
            { competences = gridCompetences
            , resourceTasks = computeResourceTasks doc gridCompetences
            , learningResources = computeLearningResources doc gridCompetences
            , connectedUserRole = role
            , viewData = vData
            }

    -- Compute resource tasks grouped by competence level
    computeResourceTasks :: Document -> Ix.IxSet CompetenceIxs Competence -> Map CompetenceLevelId [TaskWithSolutions]
    computeResourceTasks doc gridCompetences =
      let taskGroups = doc.taskGroups
          competenceIds = [c.id | c <- Ix.toList gridCompetences]
          resourceTasksList = filter (isResourceTask taskGroups) $ Ix.toList doc.tasks
          buildTaskWithSolutions :: Task -> TaskWithSolutions
          buildTaskWithSolutions task = TaskWithSolutions
            { task = task
            , taskContent = getTaskContent taskGroups task
            , taskPurpose = (getTaskAttributes taskGroups task).purpose
            , solutions = Ix.toList $ doc.solutions Ix.@= task.id
            }
          taskSortKey :: TaskWithSolutions -> T.Text
          taskSortKey tws = let TaskIdentifier ident = tws.task.identifier in ident
          groupByCompetenceLevel :: [TaskWithSolutions] -> Map CompetenceLevelId [TaskWithSolutions]
          groupByCompetenceLevel tasks =
            Map.map (sortOn taskSortKey) $ foldr insertTask Map.empty tasks
            where
              insertTask tws acc =
                let primaryLevels = getTaskPrimaryCompetences taskGroups tws.task
                    relevantLevels = filter (\(cid, _) -> cid `elem` competenceIds) primaryLevels
                 in foldr (\lvl -> Map.insertWith (++) lvl [tws]) acc relevantLevels
       in groupByCompetenceLevel $ map buildTaskWithSolutions resourceTasksList

    -- Compute learning resources grouped by competence level
    computeLearningResources :: Document -> Ix.IxSet CompetenceIxs Competence -> Map CompetenceLevelId [Resource]
    computeLearningResources doc gridCompetences =
      let competenceIds = [c.id | c <- Ix.toList gridCompetences]
          allResources = Ix.toList doc.resources
          resourceSortKey :: Resource -> (Int, T.Text)
          resourceSortKey res =
            let ResourceIdentifier ident = res.identifier
                typeOrder = case res.content of
                  InlineContent _ -> 0
                  VideoLink _ _ -> 1
                  WebLink _ _ -> 2
             in (typeOrder, ident)
          groupByCompetenceLevel :: [Resource] -> Map CompetenceLevelId [Resource]
          groupByCompetenceLevel resources =
            Map.map (sortOn resourceSortKey) $ foldr insertResource Map.empty resources
            where
              insertResource res acc =
                let relevantLevels = filter (\(cid, _) -> cid `elem` competenceIds) res.competenceLevels
                 in foldr (\lvl -> Map.insertWith (++) lvl [res]) acc relevantLevels
       in groupByCompetenceLevel allResources

    emptyProjection = ViewerProjection
      { competences = Ix.empty
      , resourceTasks = Map.empty
      , learningResources = Map.empty
      , connectedUserRole = connectedRole
      , viewData = AnalyticsViewData $ AnalyticsData 0 Map.empty Map.empty
      }

    model = ViewerModel emptyProjection

    update (ViewerProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    update (OpenResourceModal clId) = do
      m <- M.get
      let tasks = Map.findWithDefault [] clId m.projection.resourceTasks
          resources = Map.findWithDefault [] clId m.projection.learningResources
          showPurposeBadge = m.projection.connectedUserRole == Teacher
          cfg = ResourceModal.ResourceModalConfig tasks resources showPurposeBadge r.modalManager
      M.io_ $ openModal r.modalManager (ResourceModal.resourceModalComponent cfg)

    -- Main view: dispatch based on view data type
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
        proj = m.projection

        -- Header varies by view type
        header = case proj.viewData of
          UserViewData userData ->
            MH.div_
              [class_ "flex items-center justify-between w-full"]
              [ Typography.h2 (M.ms grid.title)
              , case userData.activeGridGrade of
                  Just gridGrade -> gradeBadgeView gridGrade.grade
                  Nothing -> V.empty
              ]
          AnalyticsViewData _ ->
            MH.div_
              [class_ "flex items-center justify-between w-full"]
              [ Typography.h2 (M.ms grid.title)
              , V.empty
              ]

        description = Typography.paragraph (M.ms grid.description)

        -- Table with cells that vary by view type
        competencesTable vm =
          V.viewTable $
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
                    renderDescriptionCell proj competence
                  ViewerLevelColumn level ->
                    renderLevelCell proj competence level
              }

    -- Render description cell (first column)
    renderDescriptionCell proj competence =
      let bgClass = case proj.viewData of
            UserViewData userData ->
              let mAssessment = getActiveAssessment' userData.userAssessments competence.id
               in case mAssessment of
                    Nothing -> ""
                    Just assessment -> case assessment.level of
                      Nothing -> "bg-yellow-100"
                      Just _ -> "bg-green-100"
            AnalyticsViewData _ -> ""
       in TableCellSpec
            { cellClasses = "px-4 py-3 " <> bgClass
            , cellStyle = []
            , cellContent = Typography.small (M.ms competence.description)
            }

    -- Render level cell (varies by view type)
    renderLevelCell proj competence level =
      let levelInfo = Map.findWithDefault (LevelInfo T.empty False) level competence.levels
          hasDescription = not (T.null levelInfo.description)
          competenceLevelId = (competence.id, level)

          -- Striped background for empty cells
          stripeStyle :: [(M.MisoString, M.MisoString)]
          stripeStyle =
            if not hasDescription
              then
                [ ("background",
                   "repeating-linear-gradient(135deg, rgb(245 245 244) 0px, rgb(245 245 244) 4px, rgb(231 229 228) 4px, rgb(231 229 228) 8px)")
                ]
              else []

       in case proj.viewData of
            UserViewData userData ->
              renderUserCell proj userData competence level levelInfo hasDescription competenceLevelId stripeStyle
            AnalyticsViewData analyticsData ->
              renderAnalyticsCell proj analyticsData levelInfo hasDescription competenceLevelId stripeStyle

    -- Render cell for user view (shows evidence icons, assessment status)
    renderUserCell proj userData competence level levelInfo hasDescription competenceLevelId stripeStyle =
      let evidences = userData.userEvidences
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

          -- Get active assessment
          mAssessment = getActiveAssessment' userData.userAssessments competence.id

          -- Determine cell assessment status
          cellStatus :: CellAssessmentStatus
          cellStatus = case mAssessment of
            Nothing -> NoAssessment
            Just assessment -> case assessment.level of
              Nothing -> NotYetAchieved
              Just assessedLevel ->
                if level <= assessedLevel
                  then Achieved
                  else NotYetAchieved

          -- Cell background color
          bgClass
            | not hasDescription = ""
            | cellStatus == Achieved = "bg-green-100"
            | levelInfo.locked = "bg-stone-200"
            | cellStatus == NotYetAchieved = "bg-yellow-100"
            | otherwise = "bg-white"

          -- Status icon
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

          -- Resource handling
          hasResourceTasks = not $ null $ Map.findWithDefault [] competenceLevelId proj.resourceTasks
          hasLearningResources' = not $ null $ Map.findWithDefault [] competenceLevelId proj.learningResources
          hasResources = hasResourceTasks || hasLearningResources'

          resourceIcon =
            if hasResources
              then
                MH.div_
                  [class_ "absolute bottom-1 right-1 text-sky-600"]
                  [icon [MP.width_ "14", MP.height_ "14"] IcnResources]
              else V.empty

          cursorClass = if hasResources then " cursor-pointer hover:bg-opacity-80" else ""
          tdClasses = "relative px-4 py-3 " <> bgClass <> cursorClass

          clickHandler =
            if hasResources
              then [MH.onClick (OpenResourceModal competenceLevelId)]
              else []

          cellContent =
            MH.div_
              (class_ "flex flex-col justify-center min-h-[44px]" : clickHandler)
              [ statusIcon
              , if hasDescription
                  then Typography.small (M.ms levelInfo.description)
                  else V.empty
              , if not (null evidenceList)
                  then
                    MH.div_
                      [class_ "flex flex-wrap gap-1 mt-1"]
                      (map showEvidence evidenceList)
                  else V.empty
              , resourceIcon
              ]
       in TableCellSpec
            { cellClasses = tdClasses
            , cellStyle = stripeStyle
            , cellContent = cellContent
            }

    -- Render cell for analytics view (shows mastery distribution bars + resources)
    -- Same structure as user view, but with mastery bars instead of evidence icons
    renderAnalyticsCell proj analyticsData levelInfo hasDescription competenceLevelId stripeStyle =
      let -- Resource handling (same as user view)
          hasResourceTasks = not $ null $ Map.findWithDefault [] competenceLevelId proj.resourceTasks
          hasLearningResources' = not $ null $ Map.findWithDefault [] competenceLevelId proj.learningResources
          hasResources = hasResourceTasks || hasLearningResources'

          resourceIcon =
            if hasResources
              then
                MH.div_
                  [class_ "absolute bottom-1 right-1 text-sky-600"]
                  [icon [MP.width_ "14", MP.height_ "14"] IcnResources]
              else V.empty

          cursorClass = if hasResources then " cursor-pointer hover:bg-opacity-80" else ""
          tdClasses = "relative px-2 py-2" <> cursorClass

          clickHandler =
            if hasResources
              then [MH.onClick (OpenResourceModal competenceLevelId)]
              else []

          cellContent =
            MH.div_
              (class_ "flex flex-col justify-center min-h-[44px]" : clickHandler)
              [ if hasDescription
                  then Typography.small (M.ms levelInfo.description)
                  else V.empty
              , if hasDescription
                  then
                    let stats = Map.findWithDefault Map.empty competenceLevelId analyticsData.masteryStats
                        students = Map.findWithDefault Map.empty competenceLevelId analyticsData.masteryStudents
                     in masteryDisplay MasteryDisplayConfig
                          { totalStudents = analyticsData.totalStudents
                          , stats = stats
                          , students = students
                          }
                  else V.empty
              , resourceIcon
              ]
       in TableCellSpec
            { cellClasses = tdClasses
            , cellStyle = stripeStyle
            , cellContent = cellContent
            }

-- ============================================================================
-- MASTERY DISPLAY COMPONENT
-- ============================================================================

-- | Configuration for the mastery display component
data MasteryDisplayConfig = MasteryDisplayConfig
  { totalStudents :: !Int
  , stats :: !(Map MasteryStatus Int)
  , students :: !(Map MasteryStatus [User])
  }

-- | Render mastery distribution as horizontal stacked bars with tooltips
-- Always shows all 5 indicators (dimmed when count is 0) for consistent navigation
masteryDisplay :: MasteryDisplayConfig -> M.View m action
masteryDisplay config =
  MH.div_
    [class_ "flex flex-col gap-1 mt-1"]
    [ -- Stacked horizontal bar (only segments with count > 0)
      MH.div_
        [class_ "flex h-3 rounded overflow-hidden bg-stone-100"]
        (map renderSegment segments)
    , -- Count labels below - always show all 5, with CSS tooltips
      MH.div_
        [class_ "flex gap-x-2 text-xs"]
        (map renderIndicator segments)
    ]
  where
    getCount status = Map.findWithDefault 0 status config.stats
    getStudents status = Map.findWithDefault [] status config.students

    segments =
      [ (StreakTwoPlus, "bg-green-700", C.translate' C.LblMasteryStreakTwoPlus)
      , (OneSuccess, "bg-green-500", C.translate' C.LblMasteryOneSuccess)
      , (OnlySillyMistakes, "bg-yellow-500", C.translate' C.LblMasteryOnlySillyMistakes)
      , (MasteryNotYet, "bg-amber-600", C.translate' C.LblMasteryNotYet)
      , (NotTried, "bg-stone-300", C.translate' C.LblMasteryNotTried)
      ]

    percentage count =
      if config.totalStudents > 0
        then (fromIntegral count * 100.0 / fromIntegral config.totalStudents) :: Double
        else 0.0

    -- Render bar segment (only if count > 0, otherwise skip to keep bar compact)
    renderSegment (status, colorClass, _label) =
      let count = getCount status
          pct = percentage count
       in if count > 0
            then
              MH.div_
                [ class_ $ colorClass <> " h-full"
                , MC.style_ [("width", M.ms $ show pct <> "%")]
                ]
                []
            else V.empty

    -- Render count indicator with CSS tooltip showing student names
    renderIndicator (status, colorClass, label) =
      let count = getCount status
          studentList = getStudents status
          isZero = count == 0
          -- Dim both the color box and text when count is 0
          opacityClass = if isZero then " opacity-30" else ""
          textClass = if isZero then "text-stone-400" else "text-stone-600"
          -- Build tooltip content: label on first line, student names on second
          studentNames = T.intercalate ", " $ map (.name) studentList
          tooltipContent = label <> "\n" <> M.ms studentNames
          -- Only show tooltip if there are students (no point showing "—")
          tooltipView =
            if isZero
              then M.text ""
              else
                MH.span_
                  [ class_
                      "absolute bottom-full left-0 mb-2 px-3 py-1.5 \
                      \bg-primary text-primary-foreground text-xs rounded-md \
                      \whitespace-pre-line min-w-48 max-w-xs text-left \
                      \opacity-0 group-hover:opacity-100 \
                      \pointer-events-none transition-opacity z-50"
                  ]
                  [M.text tooltipContent]
       in MH.div_
            [class_ $ "group relative flex items-center gap-0.5" <> opacityClass]
            [ tooltipView
            , -- Colored square
              MH.div_ [class_ $ "w-2 h-2 rounded-sm " <> colorClass] []
            , -- Count
              MH.span_ [class_ textClass] [M.text $ M.ms $ show count]
            ]

-- ============================================================================
-- HELPER TYPES AND FUNCTIONS
-- ============================================================================

data ViewerColumn
  = ViewerDescriptionColumn
  | ViewerLevelColumn !Level
  deriving (Eq, Show)

-- | Assessment status for a cell in the viewer
data CellAssessmentStatus
  = Achieved       -- ^ Cell level is at or below the assessed level
  | NotYetAchieved -- ^ Cell level is above the assessed level
  | NoAssessment   -- ^ No assessment exists for this competence
  deriving (Eq, Show)

-- | Get the most recent (active) assessment for a competence
getActiveAssessment'
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> Maybe CompetenceAssessment
getActiveAssessment' assessments competenceId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $ assessments Ix.@= competenceId
