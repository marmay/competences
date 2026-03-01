module Competences.Frontend.Component.CompetenceGrid.Viewer
  ( viewerDetailView
  , pinCompetenceGridViewer
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
  , getLevelInfo
  , hasLevelContent
  , ordered
  )
import Competences.Document.Id (idToText)
import Competences.Document.Evidence
  ( Evidence (..)
  , EvidenceId
  , Observation (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..))
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskId
  , TaskIdentifier (..)
  , getTaskAttributes
  , getTaskContent
  , getTaskPrimaryCompetences
  , isResourceTask
  )
import Competences.Document.User (User (..), UserRole (..))
import Competences.Query.Competence qualified as QCompetence
import Competences.Query.CompetenceAssessment qualified as QAssessment
import Competences.Query.Evidence qualified as QEvidence
import Competences.Query.User qualified as QUser
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ResourceLookup (findGroupedResources)
import Competences.Frontend.SyncContext.WindowManager (PinCategory (..), PinMeta (..), SortAtom (..), SortKey (..), WindowChrome (..), WindowMode, inlineComponentWith, isPinned, pinDialogWith)
import Competences.Frontend.Component.Resource.Modal qualified as ResourceModal
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.TaskResource (TaskWithSolutions (..))
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , SyncDocument (..)
  , SyncDocumentEnv (..)
  , getFocusedUserRef
  , readSyncDocument
  , subscribeWithProjection
  , syncDocumentEnv
  )
import Competences.Frontend.SyncContext.UIState (readFocusedUser)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Color.Ability (abilityPalette)
import Competences.Frontend.View.Color.Mastery (masteryPalette)
import Competences.Frontend.View.EvidenceIcon qualified as EvidenceIcon
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Table (TableCellSpec (..))
import Competences.Frontend.View.CellStyle qualified as CellStyle
import Competences.Frontend.View.MasteryBar qualified as MasteryBar
import Competences.Frontend.View.StatusIcon qualified as StatusIcon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.WindowFrame (pinButton)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Mastery
  ( MasteryStatus (..)
  , getClassMasteryStats
  , getClassMasteryWithStudents
  , getUserMastery
  , getUserMasteryWithReasoning
  )
import Competences.Query.TaskStatus (TaskCompletionStatus (..), taskCompletionStatuses)
import Control.Concurrent (threadDelay)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.DSL (jsg, (#))
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (MisoString)
import Optics.Core ((.~))

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
  , taskStatuses :: !(Map TaskId TaskCompletionStatus)
  -- ^ Per-task completion status for the focused user (empty when no user focused)
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
  , userMastery :: !(Map CompetenceLevelId MasteryStatus)
  -- ^ Pre-computed mastery status per competence level (for stripe display)
  , masteryInfluencing :: !(Map CompetenceLevelId (Set EvidenceId))
  -- ^ Evidence IDs that influenced the mastery decision per competence level
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

-- | Data for printing a single student's competence grid
data StudentPrintData = StudentPrintData
  { studentName :: !T.Text
  , mastery :: !(Map CompetenceLevelId MasteryStatus)
  , uncompletedTasks :: !(Map CompetenceLevelId [TaskIdentifier])
  }
  deriving (Eq, Generic, Show)

-- | All data needed to render the print view
data PrintData = PrintData
  { students :: ![StudentPrintData]
  }
  deriving (Eq, Generic, Show)

-- | Model for the viewer detail component
data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  , printData :: !(Maybe PrintData)
  }
  deriving (Eq, Generic, Show)

-- | Action for the viewer detail component
data ViewerAction
  = ViewerProjectionChanged !(ProjectedChange ViewerProjection)
  | OpenResourceModal !CompetenceLevelId
  | PinThis
  | TriggerPrint
  | DoPrint !PrintData
  | ClearPrint
  deriving (Eq, Show)

-- ============================================================================
-- COMPONENT
-- ============================================================================

-- | Pin the competence grid viewer as a persistent dialog.
pinCompetenceGridViewer :: SyncContext -> CompetenceGrid -> IO ()
pinCompetenceGridViewer r grid =
  let chrome = WindowChrome (M.ms grid.title) Icon.IcnCompetenceGrid
      meta = PinMeta
        { key = "grid-" <> idToText grid.id
        , category = PinCatCompetenceGrid
        , sortKey = SortKey [SortAtom grid.order, SortAtom grid.id]
        , context = Nothing
        }
   in pinDialogWith r.windowManager
        meta
        chrome
        (viewerComponent r grid)

-- | View for the viewer detail - shows competence grid with student evidence
viewerDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
viewerDetailView r grid =
  inlineComponentWith
    ("competence-grid-viewer-" <> M.ms (show grid.id))
    (viewerComponent r grid)

viewerComponent :: SyncContext -> CompetenceGrid -> WindowMode -> M.Component p ViewerModel ViewerAction
viewerComponent r grid wm =
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
          -- All competence levels in this grid that have descriptions
          competenceLevels =
            [ (c.id, level)
            | c <- Ix.toList gridCompetences
            , level <- allLevels
            , let levelInfo = getLevelInfo level c
            , not (T.null levelInfo.description)
            ]
          -- Compute view-specific data based on focused user
          vData = case mUser of
            Nothing ->
              -- No focused user: compute analytics
              let students = QUser.students doc
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
              let masteryWithReasoning = Map.fromList
                    [ (clId, getUserMasteryWithReasoning doc u.id clId)
                    | clId <- competenceLevels
                    ]
                  mastery = Map.map fst masteryWithReasoning
                  influencing = Map.map (Set.fromList . snd) masteryWithReasoning
               in UserViewData $ UserData
                    { focusedUser = u
                    , userEvidences = QEvidence.userEvidences doc u.id
                    , userAssessments = doc.competenceAssessments Ix.@= u.id
                    , activeGridGrade = listToMaybe $ Ix.toDescList (Proxy @Day) $
                        doc.competenceGridGrades Ix.@= u.id Ix.@= grid.id
                    , userMastery = mastery
                    , masteryInfluencing = influencing
                    }
          -- Pre-compute resource tasks (needed for both display and status computation)
          resTasks = computeResourceTasks doc gridCompetences
          lResources = computeLearningResources doc gridCompetences
          -- Compute task statuses for focused user (if any)
          allResTasks = concatMap (map (.task)) (Map.elems resTasks)
          tStatuses = case mUser of
            Just u -> taskCompletionStatuses doc u.id allResTasks
            Nothing -> Map.empty
       in ViewerProjection
            { competences = gridCompetences
            , resourceTasks = resTasks
            , learningResources = lResources
            , connectedUserRole = role
            , taskStatuses = tStatuses
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
      , taskStatuses = Map.empty
      , viewData = AnalyticsViewData $ AnalyticsData 0 Map.empty Map.empty
      }

    model = ViewerModel emptyProjection Nothing

    update (ViewerProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    update (OpenResourceModal clId) = do
      m <- M.get
      let tasks = Map.findWithDefault [] clId m.projection.resourceTasks
          showPurposeBadge = m.projection.connectedUserRole == Teacher
          cfg = ResourceModal.ResourceModalConfig tasks (\doc -> findGroupedResources doc [clId]) showPurposeBadge m.projection.taskStatuses
      M.io_ $ ResourceModal.openResourceModal r cfg

    update PinThis = M.io_ $ pinCompetenceGridViewer r grid

    update TriggerPrint = M.io $ do
      syncDoc <- readSyncDocument r
      let doc = syncDoc.localDocument
      mUser <- readFocusedUser (getFocusedUserRef r)
      let isTeacher = connectedRole == Teacher
          studentsToprint = case (isTeacher, mUser) of
            (True, Nothing) -> QUser.studentsSortedByName doc
            (True, Just u) -> [u]
            (False, _) -> [(syncDocumentEnv r).connectedUser]
          gridCompetences = QCompetence.gridCompetences doc grid.id
          competenceLevels =
            [ (c.id, level)
            | c <- Ix.toList gridCompetences
            , level <- allLevels
            , let li = getLevelInfo level c
            , not (T.null li.description)
            ]
          resTasks = computeResourceTasks doc gridCompetences
          allResTasks = concatMap (map (.task)) (Map.elems resTasks)
          mkStudentData u =
            let mastery = Map.fromList
                  [ (clId, getUserMastery doc u.id clId)
                  | clId <- competenceLevels
                  ]
                tStatuses = taskCompletionStatuses doc u.id allResTasks
                uncompleted = Map.mapWithKey
                  (\_clId tasks' ->
                    [ t.task.identifier
                    | t <- tasks'
                    , case Map.lookup t.task.id tStatuses of
                        Just (TaskDone _) -> False
                        _ -> True
                    ]
                  )
                  resTasks
             in StudentPrintData
                  { studentName = u.name
                  , mastery = mastery
                  , uncompletedTasks = uncompleted
                  }
          pd = PrintData { students = map mkStudentData studentsToprint }
      pure $ DoPrint pd

    update (DoPrint pd) = do
      M.modify $ #printData .~ Just pd
      M.io $ do
        threadDelay 500000 -- 500ms for DOM patch to complete
        triggerBrowserPrint
        pure ClearPrint

    update ClearPrint =
      M.modify $ #printData .~ Nothing

    -- Main view: dispatch based on view data type
    view m =
      MH.div_
        []
        [ MH.div_
            [class_ "print:hidden"]
            [ Layout.vFlow
                (Layout.gapS <> Layout.wFull <> Layout.crossCenter)
                [ header
                , description
                , competencesTable m
                ]
            ]
        , case m.printData of
            Just pd -> printView grid pd m.projection
            Nothing -> Layout.empty
        ]
      where
        proj = m.projection

        printButton' =
          Button.ghostSm (Button.ButtonConfig (Button.IconOnly Icon.IcnPrint) (Just TriggerPrint))

        -- Header varies by view type
        header = case proj.viewData of
          UserViewData userData ->
            MH.div_
              [class_ "w-full"]
              [ Layout.hFlow
                  (Layout.hFull <> Layout.crossCenter) $
                  [ Typography.h2 (M.ms grid.title)
                  , Layout.flowSpring
                  ]
                  <> [ printButton' ]
                  <> [ pinButton PinThis | not (isPinned wm) ]
                  <> [ case userData.activeGridGrade of
                         Just gridGrade -> gradeBadgeView gridGrade.grade
                         Nothing -> Layout.empty
                     ]
              ]
          AnalyticsViewData _ ->
            MH.div_
              [class_ "w-full"]
              [ Layout.hFlow
                  (Layout.hFull <> Layout.crossCenter) $
                  [ Typography.h2 (M.ms grid.title)
                  , Layout.flowSpring
                  ]
                  <> [ printButton' ]
                  <> [ pinButton PinThis | not (isPinned wm) ]
              ]

        description = Typography.paragraph (M.ms grid.description)

        -- Table with cells that vary by view type
        competencesTable vm =
          Table.viewTable $
            Table.defTable
              { Table.columns =
                  [ViewerDescriptionColumn]
                    <> map ViewerLevelColumn allLevels
              , Table.rows = ordered vm.projection.competences
              , Table.columnSpec = \case
                  ViewerDescriptionColumn ->
                    Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                  ViewerLevelColumn l ->
                    Table.TableColumnSpec Table.EqualWidthColumn (C.translate' $ C.LblCompetenceLevelDescription l)
              , Table.rowContents = Table.cellContentsWithSpec $ \competence -> \case
                  ViewerDescriptionColumn ->
                    renderDescriptionCell proj competence
                  ViewerLevelColumn level ->
                    renderLevelCell proj competence level
              }

    -- | Look up mastery status for a competence level (defaults to NotTried)
    lookupMastery :: UserData -> CompetenceLevelId -> MasteryStatus
    lookupMastery ud clId = Map.findWithDefault NotTried clId ud.userMastery

    -- Render description cell (first column)
    renderDescriptionCell proj competence =
      let (bgClass, bgStyle) = case proj.viewData of
            UserViewData userData ->
              let mAssessment = QAssessment.activeAssessment userData.userAssessments competence.id
               in case mAssessment of
                    Nothing ->
                      let firstNonEmptyLevel = listToMaybe
                            [ level | level <- allLevels, hasLevelContent level competence ]
                          ms = maybe NotTried
                            (\lvl -> lookupMastery userData (competence.id, lvl))
                            firstNonEmptyLevel
                       in ("", CellStyle.masteryStripedStyle ms)
                    Just assessment -> case assessment.level of
                      Nothing -> ("bg-yellow-200", [])
                      Just _ -> ("bg-green-200", [])
            AnalyticsViewData _ -> ("", [])
       in TableCellSpec
            { cellClasses = "px-4 py-3 " <> bgClass
            , cellStyle = bgStyle
            , cellContent = Typography.small (M.ms competence.description)
            }

    -- Render level cell (varies by view type)
    renderLevelCell proj competence level =
      let levelInfo = getLevelInfo level competence
          hasDescription = not (T.null levelInfo.description)
          competenceLevelId = (competence.id, level)

          -- Striped background for empty cells
          stripeStyle :: [(M.MisoString, M.MisoString)]
          stripeStyle = if not hasDescription then CellStyle.stripedStyle else []

       in case proj.viewData of
            UserViewData userData ->
              renderUserCell proj userData competence level levelInfo hasDescription competenceLevelId stripeStyle
            AnalyticsViewData analyticsData ->
              renderAnalyticsCell proj analyticsData levelInfo hasDescription competenceLevelId stripeStyle

    -- Render cell for user view (shows evidence icons, assessment status)
    renderUserCell proj userData competence level levelInfo hasDescription competenceLevelId stripeStyle =
      let evidences = userData.userEvidences

          -- Direct evidence badges paired with date for sorting
          directBadges =
            [ (e.date, showEvidence e)
            | e <- Ix.toAscList (Proxy @Day) (evidences Ix.@= competenceLevelId)
            ]

          -- Cross-level badges: observations at OTHER levels of the same competence
          -- that influence this level's mastery via cross-level inference.
          -- Only show badges for evidences that actually influenced the mastery decision.
          influencingIds = Map.findWithDefault Set.empty competenceLevelId userData.masteryInfluencing
          crossLevelBadges =
            [ (e.date, showCrossLevel obs)
            | lvl <- allLevels
            , lvl /= level
            , e <- Ix.toAscList (Proxy @Day) (evidences Ix.@= (competence.id, lvl))
            , Set.member e.id influencingIds
            , Ix.null (e.observations Ix.@= competenceLevelId)
            , obs <- maybeToList $ Ix.getOne (e.observations Ix.@= (competence.id, lvl))
            ]

          -- Merge by date (ascending = oldest first)
          allBadges = map snd $ sortOn fst (directBadges ++ crossLevelBadges)

          showEvidence evidence =
            case Ix.getOne (evidence.observations Ix.@= competenceLevelId) of
              Just observation ->
                showSummary evidence.activityType observation.socialForm observation.ability
              Nothing -> Layout.empty

          showSummary activityType socialForm ability =
            Badge.badge (abilityPalette ability) $
              MH.span_
                [class_ "inline-flex items-center gap-0.5"]
                [ Icon.icon [] (EvidenceIcon.activityTypeIcon activityType)
                , Icon.icon [] (EvidenceIcon.socialFormIcon socialForm)
                ]

          showCrossLevel obs =
            let fromLevel = snd obs.competenceLevelId
                arrowIcon = if fromLevel > level then Icon.IcnArrowDown else Icon.IcnArrowUp
                lvlText = levelShortLabel fromLevel
             in Badge.badge (abilityPalette obs.ability) $
                  MH.span_
                    [class_ "inline-flex items-center gap-0 opacity-60"]
                    [ Icon.icon [class_ "w-3 h-3"] arrowIcon
                    , M.text (M.ms lvlText)
                    ]

          -- Get active assessment
          mAssessment = QAssessment.activeAssessment userData.userAssessments competence.id

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

          -- Visual status for styling
          cellVisualStatus
            | not hasDescription = StatusIcon.NoStatus
            | cellStatus == Achieved = StatusIcon.Achieved
            | levelInfo.locked = StatusIcon.Locked
            | cellStatus == NotYetAchieved = StatusIcon.InProgress
            | otherwise = StatusIcon.NoStatus

          -- Cell background color
          bgClass = CellStyle.statusBgClass cellVisualStatus

          -- Status icon
          statusIcon = StatusIcon.statusIconOverlay cellVisualStatus

          -- Resource handling
          hasResourceTasks = not $ null $ Map.findWithDefault [] competenceLevelId proj.resourceTasks
          hasLearningResources' = not $ null $ Map.findWithDefault [] competenceLevelId proj.learningResources
          hasResources = hasResourceTasks || hasLearningResources'

          resourceIcon =
            if hasResources
              then
                MH.div_
                  [class_ "absolute bottom-1 right-1 text-sky-600"]
                  [Icon.icon [MP.width_ "14", MP.height_ "14"] Icon.IcnResources]
              else Layout.empty

          cursorClass = if hasResources then " cursor-pointer hover:bg-opacity-80" else ""
          tdClasses = "relative px-4 py-3 " <> bgClass <> cursorClass

          clickHandler =
            if hasResources
              then [MH.onClick (OpenResourceModal competenceLevelId)]
              else []

          -- Mastery badge row above description (only when mastery is active)
          masteryBadgeRow = case cellStatus of
            NoAssessment
              | hasDescription
              , let ms = lookupMastery userData competenceLevelId
              , Just p <- masteryPalette ms ->
                  MH.div_
                    [class_ "mb-0.5"]
                    [ Layout.hFlow
                        (Layout.gapT <> Layout.hFull <> Layout.crossCenter)
                        [ Badge.secondary (Badge.badgeLabel C.LblMasteryBadgeAuto)
                        , Layout.flowSpring
                        , Badge.badge p (Badge.badgeText $ masteryBadgeLabel ms)
                        ]
                    ]
            _ -> Layout.empty

          cellContent =
            MH.div_
              (class_ "min-h-[44px]" : clickHandler)
              [ Layout.vFlow Layout.mainCenter
                  [ statusIcon
                  , masteryBadgeRow
                  , if hasDescription
                      then Typography.small (M.ms levelInfo.description)
                      else Layout.empty
                  , if not (null allBadges)
                      then
                        MH.div_
                          [class_ "mt-1"]
                          [ Layout.hFlow
                              (Layout.gapT <> Layout.flexWrap)
                              allBadges
                          ]
                      else Layout.empty
                  , resourceIcon
                  ]
              ]

          -- Mastery stripes when no assessment exists for this competence
          masteryStyle = case cellStatus of
            NoAssessment
              | hasDescription ->
                  CellStyle.masteryStripedStyle $
                    lookupMastery userData competenceLevelId
            _ -> []
       in TableCellSpec
            { cellClasses = tdClasses
            , cellStyle = stripeStyle <> masteryStyle
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
                  [Icon.icon [MP.width_ "14", MP.height_ "14"] Icon.IcnResources]
              else Layout.empty

          cursorClass = if hasResources then " cursor-pointer hover:bg-opacity-80" else ""
          tdClasses = "relative px-2 py-2" <> cursorClass

          clickHandler =
            if hasResources
              then [MH.onClick (OpenResourceModal competenceLevelId)]
              else []

          cellContent =
            MH.div_
              (class_ "min-h-[44px]" : clickHandler)
              [ Layout.vFlow Layout.mainCenter
                  [ if hasDescription
                      then Typography.small (M.ms levelInfo.description)
                      else Layout.empty
                  , if hasDescription
                      then
                        let stats = Map.findWithDefault Map.empty competenceLevelId analyticsData.masteryStats
                            students = Map.findWithDefault Map.empty competenceLevelId analyticsData.masteryStudents
                         in MasteryBar.masteryDisplay MasteryBar.MasteryDisplayConfig
                              { totalStudents = analyticsData.totalStudents
                              , stats = stats
                              , students = students
                              }
                      else Layout.empty
                  , resourceIcon
                  ]
              ]
       in TableCellSpec
            { cellClasses = tdClasses
            , cellStyle = stripeStyle
            , cellContent = cellContent
            }

-- ============================================================================
-- PRINT VIEW
-- ============================================================================

-- | Render the print-only view, hidden on screen but visible when printing.
printView :: CompetenceGrid -> PrintData -> ViewerProjection -> M.View ViewerModel ViewerAction
printView grid pd proj =
  MH.div_
    [class_ "hidden print:block"]
    ( case pd.students of
        [] -> []
        (first' : rest) ->
          renderStudent "" first' <> concatMap (renderStudent "break-before-page") rest
    )
  where
    comps = ordered proj.competences

    renderStudent :: M.MisoString -> StudentPrintData -> [M.View ViewerModel ViewerAction]
    renderStudent breakClass spd =
      [ MH.div_
          [ MP.class_ breakClass ]
          [ MH.div_
              [class_ "flex justify-between items-baseline mb-1"]
              [ MH.h2_ [class_ "text-xl font-bold"] [M.text (M.ms grid.title)]
              , MH.span_ [class_ "text-lg font-semibold"] [M.text (M.ms spd.studentName)]
              ]
          , MH.p_ [class_ "text-sm text-stone-600 mb-2"] [M.text (M.ms grid.description)]
          , MH.table_
              [ class_ "w-full border-collapse text-xs grid-print-table"
              , MC.style_ [("print-color-adjust", "exact"), ("-webkit-print-color-adjust", "exact")]
              ]
              [ MH.thead_
                  []
                  [ MH.tr_
                      []
                      ( MH.th_ [class_ "border border-stone-300 px-2 py-1 text-left bg-stone-100"]
                          [M.text (C.translate' C.LblCompetenceDescription)]
                          : [ MH.th_ [class_ "border border-stone-300 px-2 py-1 text-left bg-stone-100"]
                                [M.text (C.translate' $ C.LblCompetenceLevelDescription l)]
                            | l <- allLevels
                            ]
                      )
                  ]
              , MH.tbody_
                  []
                  (map (renderPrintRow spd) comps)
              ]
          ]
      ]

    renderPrintRow :: StudentPrintData -> Competence -> M.View ViewerModel ViewerAction
    renderPrintRow spd competence =
      MH.tr_
        []
        ( MH.td_ [class_ "border border-stone-300 px-2 py-1 align-top"]
            [MH.span_ [] [M.text (M.ms competence.description)]]
            : [ renderPrintLevelCell spd competence level | level <- allLevels ]
        )

    renderPrintLevelCell :: StudentPrintData -> Competence -> Level -> M.View ViewerModel ViewerAction
    renderPrintLevelCell spd competence level =
      let levelInfo = getLevelInfo level competence
          clId = (competence.id, level)
          hasDesc = not (T.null levelInfo.description)
       in if not hasDesc
            then
              MH.td_
                [ class_ "border border-stone-300 px-2 py-1"
                , MC.style_ CellStyle.stripedStyle
                ]
                []
            else
              let ms' = Map.findWithDefault NotTried clId spd.mastery
                  tasks = Map.findWithDefault [] clId spd.uncompletedTasks
                  masteryLine = printMasteryIndicator ms'
                  taskLines =
                    if null tasks
                      then []
                      else
                        [ MH.div_
                            [class_ "mt-0.5 text-stone-600"]
                            [ M.text $ M.ms $ T.intercalate ", " [ident | TaskIdentifier ident <- tasks] ]
                        ]
               in MH.td_
                    [class_ "border border-stone-300 px-2 py-1 align-top"]
                    (masteryLine : taskLines)

-- | Render mastery indicator for print: symbol + German text
printMasteryIndicator :: MasteryStatus -> M.View m a
printMasteryIndicator StreakTwoAssessed =
  MH.span_ [class_ "font-semibold text-green-700"] [M.text "✓✓ Überprüft"]
printMasteryIndicator StreakTwoPlus =
  MH.span_ [class_ "font-semibold text-green-600"] [M.text "✓✓ Serie"]
printMasteryIndicator OneSuccess =
  MH.span_ [class_ "text-green-600"] [M.text "✓ Erste Erfolge"]
printMasteryIndicator OnlySillyMistakes =
  MH.span_ [class_ "text-yellow-600"] [M.text "~ Flüchtigkeitsfehler"]
printMasteryIndicator MasteryNotYet =
  MH.span_ [class_ "text-red-600"] [M.text "✗ Noch nicht"]
printMasteryIndicator NotTried =
  MH.span_ [] []

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

-- | Trigger browser print dialog via JSaddle FFI.
triggerBrowserPrint :: IO ()
triggerBrowserPrint = do
  window <- jsg ("window" :: MisoString)
  _ <- window # ("print" :: MisoString) $ ([] :: [MisoString])
  pure ()

-- | Short numeric label for a level, used in cross-level badges.
levelShortLabel :: Level -> T.Text
levelShortLabel BasicLevel = "1"
levelShortLabel IntermediateLevel = "2"
levelShortLabel AdvancedLevel = "3"

-- | Short badge label for a mastery status in grid cells.
masteryBadgeLabel :: MasteryStatus -> M.MisoString
masteryBadgeLabel StreakTwoAssessed = C.translate' C.LblMasteryBadgeChecked
masteryBadgeLabel StreakTwoPlus = C.translate' C.LblMasteryBadgeStreak
masteryBadgeLabel OneSuccess = C.translate' C.LblMasteryBadgeFirstSuccess
masteryBadgeLabel OnlySillyMistakes = C.translate' C.LblMasteryBadgeSillyMistakes
masteryBadgeLabel MasteryNotYet = C.translate' C.LblMasteryBadgeNotYet
masteryBadgeLabel NotTried = ""


