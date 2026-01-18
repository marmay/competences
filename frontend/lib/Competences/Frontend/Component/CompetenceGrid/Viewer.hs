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
import Competences.Document.Resource (ResourceId)
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..))
import Competences.Document.Task
  ( TaskAttributes (..)
  , TaskIdentifier (..)
  , getTaskAttributes
  , getTaskContent
  , getTaskPrimaryCompetences
  , isResourceTask
  )
import Competences.Frontend.Component.TaskResourceList
  ( TaskResourceList
  , TaskWithSolutions (..)
  , DisplayMode (..)
  , initialState
  , taskResourceListView
  , updateTaskResourceList
  )
import Competences.Frontend.Component.TaskResourceList qualified as TRL
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Modal qualified as Modal
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Table (TableCellSpec (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Event.Types (stopPropagation)
import Miso.Html qualified as MH
import Miso.Html.Event (onClickWithOptions)
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
  , resourceTasks :: !(Map.Map CompetenceLevelId [TaskWithSolutions])
  -- ^ Tasks with displayInResources=true, grouped by primary competence level
  , learningResources :: !(Map.Map CompetenceLevelId [Resource])
  -- ^ Learning resources grouped by competence level
  }
  deriving (Eq, Generic, Show)

-- | Model for the viewer detail component
data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  , modalState :: !(Maybe ResourceModalState)
  -- ^ Nothing = modal closed, Just = modal open for a specific competence level
  }
  deriving (Eq, Generic, Show)

-- | View mode for the resource modal
data ResourceViewMode
  = ViewTasks           -- ^ Show tasks
  | ViewLearningResources  -- ^ Show learning resources (placeholder for future)
  deriving (Eq, Generic, Show)

-- | State for the resource modal when open
data ResourceModalState = ResourceModalState
  { competenceLevelId :: !CompetenceLevelId
  , taskListState :: !TaskResourceList
  , viewMode :: !ResourceViewMode
  , expandedResources :: !(Set.Set ResourceId)
  -- ^ Set of resource IDs with expanded inline content (collapsed by default)
  }
  deriving (Eq, Generic, Show)

-- | Action for the viewer detail component
data ViewerAction
  = ViewerProjectionChanged !(ProjectedChange ViewerProjection)
  | OpenResourceModal !CompetenceLevelId
  | CloseResourceModal
  | ResourceModalAction !TRL.Action
  | SwitchResourceViewMode !ResourceViewMode
  | ToggleResourceExpanded !ResourceId
  -- ^ Toggle collapsed/expanded state for inline content of a resource
  | NoOp  -- ^ Used for stopping event propagation
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
      { competences = gridCompetences
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
      , resourceTasks = computeResourceTasks doc gridCompetences
      , learningResources = computeLearningResources doc gridCompetences
      }
      where
        gridCompetences = doc.competences Ix.@= grid.id

    -- Compute resource tasks grouped by competence level
    -- Only includes tasks with displayInResources=true for competences in this grid
    -- Tasks are sorted alphabetically by identifier within each level
    computeResourceTasks :: Document -> Ix.IxSet CompetenceIxs Competence -> Map.Map CompetenceLevelId [TaskWithSolutions]
    computeResourceTasks doc gridCompetences =
      let taskGroups = doc.taskGroups
          -- Get all competence IDs in this grid
          competenceIds = [c.id | c <- Ix.toList gridCompetences]
          -- Get all resource tasks (displayInResources=true)
          resourceTasksList = filter (isResourceTask taskGroups) $ Ix.toList doc.tasks
          -- Build TaskWithSolutions for each task
          buildTaskWithSolutions :: Task -> TaskWithSolutions
          buildTaskWithSolutions task = TaskWithSolutions
            { task = task
            , taskContent = getTaskContent taskGroups task
            , taskPurpose = (getTaskAttributes taskGroups task).purpose
            , solutions = Ix.toList $ doc.solutions Ix.@= task.id
            }
          -- Sort key for tasks: by identifier
          taskSortKey :: TaskWithSolutions -> T.Text
          taskSortKey tws = let TaskIdentifier ident = tws.task.identifier in ident
          -- Group tasks by their primary competence levels (filtering to this grid)
          -- Then sort each group by identifier
          groupByCompetenceLevel :: [TaskWithSolutions] -> Map.Map CompetenceLevelId [TaskWithSolutions]
          groupByCompetenceLevel tasks =
            Map.map (sortOn taskSortKey) $ foldr insertTask Map.empty tasks
            where
              insertTask tws acc =
                let primaryLevels = getTaskPrimaryCompetences taskGroups tws.task
                    -- Only include levels for competences in this grid
                    relevantLevels = filter (\(cid, _) -> cid `elem` competenceIds) primaryLevels
                 in foldr (\lvl -> Map.insertWith (++) lvl [tws]) acc relevantLevels
       in groupByCompetenceLevel $ map buildTaskWithSolutions resourceTasksList

    -- Compute learning resources grouped by competence level
    -- Only includes resources for competences in this grid
    -- Resources are sorted by type (Inline, Video, Web) then alphabetically by identifier
    computeLearningResources :: Document -> Ix.IxSet CompetenceIxs Competence -> Map.Map CompetenceLevelId [Resource]
    computeLearningResources doc gridCompetences =
      let -- Get all competence IDs in this grid
          competenceIds = [c.id | c <- Ix.toList gridCompetences]
          -- Get all resources
          allResources = Ix.toList doc.resources
          -- Sort key for resources: (content type order, identifier)
          -- InlineContent = 0, VideoLink = 1, WebLink = 2
          resourceSortKey :: Resource -> (Int, T.Text)
          resourceSortKey res =
            let ResourceIdentifier ident = res.identifier
                typeOrder = case res.content of
                  InlineContent _ -> 0
                  VideoLink _ _ -> 1
                  WebLink _ _ -> 2
             in (typeOrder, ident)
          -- Group resources by their competence levels (filtering to this grid)
          -- Then sort each group by type and identifier
          groupByCompetenceLevel :: [Resource] -> Map.Map CompetenceLevelId [Resource]
          groupByCompetenceLevel resources =
            Map.map (sortOn resourceSortKey) $ foldr insertResource Map.empty resources
            where
              insertResource res acc =
                let -- Only include levels for competences in this grid
                    relevantLevels = filter (\(cid, _) -> cid `elem` competenceIds) res.competenceLevels
                 in foldr (\lvl -> Map.insertWith (++) lvl [res]) acc relevantLevels
       in groupByCompetenceLevel allResources

    emptyProjection = ViewerProjection Ix.empty Ix.empty Ix.empty Nothing Nothing Map.empty Map.empty
    model = ViewerModel emptyProjection Nothing

    update (ViewerProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    update (OpenResourceModal clId) =
      M.modify $ \m ->
        let tasks = Map.findWithDefault [] clId m.projection.resourceTasks
            resources = Map.findWithDefault [] clId m.projection.learningResources
            taskListState = initialState TasksCollapsed tasks
            -- Default to resources tab if no tasks but have resources
            defaultMode = if null tasks && not (null resources)
                            then ViewLearningResources
                            else ViewTasks
         in m & #modalState .~ Just (ResourceModalState clId taskListState defaultMode Set.empty)

    update CloseResourceModal =
      M.modify $ #modalState .~ Nothing

    update (ResourceModalAction action) =
      M.modify $ \m -> case m.modalState of
        Nothing -> m
        Just ms ->
          m & #modalState .~ Just (ms & #taskListState .~ updateTaskResourceList action ms.taskListState)

    update (SwitchResourceViewMode newMode) =
      M.modify $ \m -> case m.modalState of
        Nothing -> m
        Just ms -> m & #modalState .~ Just (ms & #viewMode .~ newMode)

    update (ToggleResourceExpanded resId) =
      M.modify $ \m -> case m.modalState of
        Nothing -> m
        Just ms ->
          let newExpanded = if Set.member resId ms.expandedResources
                              then Set.delete resId ms.expandedResources
                              else Set.insert resId ms.expandedResources
           in m & #modalState .~ Just (ms & #expandedResources .~ newExpanded)

    update NoOp = pure ()

    view m =
      MH.div_
        []
        [ V.viewFlow
            ( V.vFlow
                & (#expandDirection .~ V.Expand V.Start)
                & (#expandOrthogonal .~ V.Expand V.Center)
                & (#gap .~ V.SmallSpace)
            )
            [ header m
            , description
            , competencesTable m
            ]
        , resourceModal m
        ]
      where
        proj = m.projection

        -- Resource modal
        resourceModal vm = case vm.modalState of
          Nothing -> V.empty
          Just ms ->
            let tasks = Map.findWithDefault [] ms.competenceLevelId vm.projection.resourceTasks
                resources = Map.findWithDefault [] ms.competenceLevelId vm.projection.learningResources
             in Modal.modalHost
                  [MH.onClick CloseResourceModal]
                  [ Modal.modalDialog
                      [ onClickWithOptions stopPropagation NoOp
                      -- Use !important to override modalDialog's default max-w-96 and w-full
                      -- flex-shrink-0 prevents the flex container from shrinking this element
                      , class_ "!w-[66vw] !min-w-[66vw] !max-w-none !h-[90vh] flex flex-col flex-shrink-0"
                      ]
                      [ -- Header with title, mode switch, and close button
                        MH.div_
                          [class_ "flex items-center justify-between border-b px-8 py-6 shrink-0"]
                          [ Typography.h3 $ C.translate' C.LblMaterials
                          , MH.div_
                              [class_ "flex items-center gap-4"]
                              [ -- Mode switch (always shown)
                                modeSwitcher ms.viewMode (not $ null tasks) (not $ null resources)
                              , -- Close button
                                MH.button_
                                  [ class_ "text-muted-foreground hover:text-foreground transition-colors"
                                  , MH.onClick CloseResourceModal
                                  ]
                                  [icon [MP.width_ "20", MP.height_ "20"] IcnCancel]
                              ]
                          ]
                      , -- Scrollable content area
                        MH.div_
                          [class_ "flex-1 overflow-y-auto px-8 py-6"]
                          [ case ms.viewMode of
                              ViewTasks ->
                                taskResourceListView tasks ms.taskListState ResourceModalAction
                              ViewLearningResources ->
                                resourcesListView resources ms.expandedResources
                          ]
                      ]
                  ]

        -- View for displaying learning resources
        resourcesListView :: [Resource] -> Set.Set ResourceId -> M.View ViewerModel ViewerAction
        resourcesListView resources expandedSet =
          if null resources
            then Typography.muted $ C.translate' C.LblNoResources
            else MH.div_ [class_ "space-y-2"] (map resourceCard resources)
          where
            resourceCard res =
              let ResourceIdentifier ident = res.identifier
                  displayName = if T.null ident then "(Unbenannt)" else ident
               in case res.content of
                    -- Inline content: expandable card
                    InlineContent txt ->
                      let isExpanded = Set.member res.id expandedSet
                          hasContent = not (T.null txt)
                          headerClasses = if hasContent
                            then "flex items-center gap-2 px-4 py-3 cursor-pointer hover:bg-muted/50 transition-colors"
                            else "flex items-center gap-2 px-4 py-3"
                          headerAttrs = if hasContent
                            then [class_ headerClasses, MH.onClick (ToggleResourceExpanded res.id)]
                            else [class_ headerClasses]
                       in MH.div_
                            [class_ "border rounded-lg overflow-hidden"]
                            [ -- Header (clickable if has content)
                              MH.div_
                                headerAttrs
                                [ -- Expand/collapse icon (only if has content)
                                  if hasContent
                                    then icon [] (if isExpanded then IcnArrowDown else IcnExpandShrinkArrowRight)
                                    else V.empty
                                , icon [class_ "text-sky-600"] IcnResources
                                , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
                                ]
                            , -- Content (shown when expanded)
                              if isExpanded && hasContent
                                then MH.div_
                                       [class_ "px-4 py-3 border-t"]
                                       [MH.div_ [class_ "prose prose-stone prose-sm max-w-none whitespace-pre-wrap"] [M.text (M.ms txt)]]
                                else V.empty
                            ]
                    -- Web link: direct link card
                    WebLink url title ->
                      MH.a_
                        [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
                        , MP.href_ (M.ms url)
                        , MP.target_ "_blank"
                        , MP.rel_ "noopener noreferrer"
                        ]
                        [ icon [class_ "text-sky-600"] IcnLink
                        , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
                        , if T.null title || title == ident
                            then V.empty
                            else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
                        ]
                    -- Video link: direct link card
                    VideoLink url title ->
                      MH.a_
                        [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
                        , MP.href_ (M.ms url)
                        , MP.target_ "_blank"
                        , MP.rel_ "noopener noreferrer"
                        ]
                        [ icon [class_ "text-sky-600"] IcnVideo
                        , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
                        , if T.null title || title == ident
                            then V.empty
                            else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
                        ]

        -- Mode switcher using button group (same style as competence grid)
        modeSwitcher :: ResourceViewMode -> Bool -> Bool -> M.View ViewerModel ViewerAction
        modeSwitcher currentMode hasTasks hasResources =
          Button.buttonGroup
            [ modeButton ViewTasks (C.translate' C.LblTasks) hasTasks
            , modeButton ViewLearningResources (C.translate' C.LblLearningResources) hasResources
            ]
          where
            modeButton mode label hasContent =
              let variant = if mode == currentMode then Button.Primary else Button.Outline
               in Button.button variant label
                    & Button.withSize Button.Small
                    & Button.withDisabled (not hasContent)
                    & Button.withClick (SwitchResourceViewMode mode)
                    & Button.renderButton

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

                            -- Check if there are resources (tasks or learning resources) for this cell
                            hasResourceTasks = not $ null $ Map.findWithDefault [] competenceLevelId proj.resourceTasks
                            hasLearningResources' = not $ null $ Map.findWithDefault [] competenceLevelId proj.learningResources
                            hasResources = hasResourceTasks || hasLearningResources'

                            -- Resource icon in bottom-right corner
                            resourceIcon =
                              if hasResources
                                then
                                  MH.div_
                                    [class_ "absolute bottom-1 right-1 text-sky-600"]
                                    [icon [MP.width_ "14", MP.height_ "14"] IcnResources]
                                else V.empty

                            -- Cell classes: relative for icon positioning, padding, and vertical centering
                            -- Add cursor-pointer when clickable (has resources)
                            cursorClass = if hasResources then " cursor-pointer hover:bg-opacity-80" else ""
                            tdClasses = "relative px-4 py-3 " <> bgClass <> cursorClass

                            -- Cell click handler (only when there are resources)
                            clickHandler =
                              if hasResources
                                then [MH.onClick (OpenResourceModal competenceLevelId)]
                                else []

                            -- Cell content wrapper for vertical centering
                            cellContent =
                              MH.div_
                                (class_ "flex flex-col justify-center min-h-[44px]" : clickHandler)
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
                                , -- Resource icon
                                  resourceIcon
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
