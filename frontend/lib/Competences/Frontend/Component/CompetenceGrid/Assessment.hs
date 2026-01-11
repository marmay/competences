module Competences.Frontend.Component.CompetenceGrid.Assessment
  ( assessmentDetailView
  )
where

import Competences.Command (Command (..), CompetenceAssessmentsCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceAssessment (..)
  , CompetenceAssessmentIxs
  , CompetenceGrid (..)
  , CompetenceGridId
  , CompetenceId
  , CompetenceIxs
  , Document (..)
  , EvidenceIxs
  , Level (..)
  , LevelInfo (..)
  , ordered
  )
import Competences.Document.Evidence
  ( Ability (..)
  , ActivityType (..)
  , Evidence (..)
  , Observation (..)
  , SocialForm (..)
  )
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..), CompetenceGridGradeIxs)
import Data.Proxy (Proxy (..))
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Colors qualified as Colors
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.List (nub, sortOn)
import Data.Map qualified as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Text qualified as T
import Data.Time (Day, getCurrentTime, utctDay)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Svg.Property qualified as MSP
import Optics.Core ((&), (.~))
import System.IO.Unsafe (unsafePerformIO)

import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)

-- ============================================================================
-- ASSESSMENT MODE DETAIL
-- ============================================================================

-- | Projection type for the assessment view - contains only the data needed for this view.
-- This is grid-specific: competences are filtered to the grid, user data filtered to focused user.
data AssessmentProjection = AssessmentProjection
  { competences :: !(Ix.IxSet CompetenceIxs Competence)
  -- ^ Competences for this grid only
  , userEvidences :: !(Ix.IxSet EvidenceIxs Evidence)
  -- ^ Evidences for focused user only
  , userAssessments :: !(Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment)
  -- ^ Assessments for focused user only
  , userGridGrades :: !(Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade)
  -- ^ Grid grades for focused user only
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Model for the assessment detail component
data AssessmentModel = AssessmentModel
  { projection :: !AssessmentProjection
  , today :: !(Maybe Day) -- Current date for creating/deleting assessments
  }
  deriving (Eq, Generic, Show)

-- | Action for the assessment detail component
data AssessmentAction
  = AssessmentProjectionChanged !(ProjectedChange AssessmentProjection)
  | SetAssessmentLevel !Competence !(Maybe Level) -- Set level for competence (Nothing = not achieved, creates/updates today's assessment)
  | ClearAssessment !Competence -- Remove today's assessment
  | InitToday !Day -- Initialize today's date
  deriving (Eq, Show)

-- | View for the assessment detail - allows assessing student competences
assessmentDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
assessmentDetailView r grid =
  V.component
    ("competence-grid-assessment-" <> M.ms (show grid.id))
    (assessmentComponent r grid)

assessmentComponent :: SyncContext -> CompetenceGrid -> M.Component p AssessmentModel AssessmentAction
assessmentComponent r grid =
  (M.component model update view)
    { M.subs = [subscribeWithProjection r assessmentProjection AssessmentProjectionChanged]
    , M.initialAction = Just initTodayAction
    }
  where
    -- Projection function captures the grid parameter
    assessmentProjection :: Document -> Maybe User -> AssessmentProjection
    assessmentProjection doc mUser = AssessmentProjection
      { competences = doc.competences Ix.@= grid.id
      , userEvidences = case mUser of
          Nothing -> Ix.empty
          Just u -> doc.evidences Ix.@= u.id
      , userAssessments = case mUser of
          Nothing -> Ix.empty
          Just u -> doc.competenceAssessments Ix.@= u.id
      , userGridGrades = case mUser of
          Nothing -> Ix.empty
          Just u -> doc.competenceGridGrades Ix.@= u.id
      , focusedUser = mUser
      }

    emptyProjection = AssessmentProjection Ix.empty Ix.empty Ix.empty Ix.empty Nothing
    model = AssessmentModel emptyProjection Nothing

    -- Initialize today's date on mount
    initTodayAction :: AssessmentAction
    initTodayAction = InitToday $ unsafePerformIO $ utctDay <$> getCurrentTime

    update (AssessmentProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    update (InitToday day) =
      M.modify $ #today .~ Just day

    update (SetAssessmentLevel competence level) = do
      m <- M.get
      case (m.projection.focusedUser, m.today) of
        (Just user, Just day) -> M.io_ $ do
          assessmentId <- nextId r
          let assessment =
                CompetenceAssessment
                  { id = assessmentId
                  , userId = user.id
                  , competenceId = competence.id
                  , level = level
                  , date = day
                  , comment = Nothing
                  }
          -- Always Create - command handler handles one-per-day constraint
          modifySyncDocument r $ CompetenceAssessments $ OnCompetenceAssessments $ Create assessment
        _ -> pure ()

    update (ClearAssessment competence) = do
      m <- M.get
      case (m.projection.focusedUser, m.today) of
        (Just _, Just day) -> do
          -- Find today's assessment to delete
          let existingToday = findAssessmentForDay' m.projection.userAssessments competence.id day
          case existingToday of
            Just assessment ->
              M.io_ $ modifySyncDocument r $ CompetenceAssessments $ OnCompetenceAssessments $ Delete assessment.id
            Nothing -> pure () -- No-op
        _ -> pure ()

    view m = case m.projection.focusedUser of
      Nothing -> Typography.muted (C.translate' C.LblNoStudentSelected)
      Just _ ->
        V.viewFlow
          ( V.vFlow
              & (#expandDirection .~ V.Expand V.Start)
              & (#expandOrthogonal .~ V.Expand V.Center)
              & (#gap .~ V.SmallSpace)
          )
          [ header
          , description
          , competenceAssessmentList m
          ]
      where
        proj = m.projection

        -- Header with title on left and grade badge on right
        header =
          MH.div_
            [class_ "flex items-center justify-between w-full"]
            [ Typography.h2 (M.ms grid.title)
            , case getActiveGridGrade' proj.userGridGrades grid.id of
                Just gridGrade -> gradeBadgeView gridGrade.grade
                Nothing -> V.empty
            ]
        description = Typography.paragraph (M.ms grid.description)

        competenceAssessmentList am =
          V.viewFlow
            (V.vFlow & (#gap .~ V.MediumSpace))
            [ competenceAssessmentCard am c
            | c <- ordered proj.competences
            ]

        competenceAssessmentCard am competence =
          let evidences = proj.userEvidences
              -- Get all assessments for this competence (historical)
              assessments = getAssessmentHistory' proj.userAssessments competence.id
              currentAssessment = listToMaybe assessments
              -- currentLevel is Maybe (Maybe Level): Nothing = no assessment, Just Nothing = not achieved, Just (Just lvl) = achieved at level
              currentLevel = fmap (.level) currentAssessment
              todayAssessment = case am.today of
                Just day -> findAssessmentForDay' proj.userAssessments competence.id day
                Nothing -> Nothing
           in Card.card
                [ competenceHeaderWithButtons competence currentLevel todayAssessment
                , timelineForCompetence competence evidences assessments
                ]

        competenceHeaderWithButtons competence currentLevel todayAssessment =
          V.viewFlow
            (V.hFlow & (#gap .~ V.SmallSpace) & (#expandOrthogonal .~ V.Expand V.Center))
            [ MH.span_ [class_ "flex-1"] [Typography.paragraph (M.ms competence.description)]
            , assessmentButtons competence currentLevel todayAssessment
            ]

        -- | Assessment buttons - levels without descriptions are disabled/grayed out
        -- Locked levels show a lock icon indicator but remain clickable
        -- currentLevel: Nothing = no assessment, Just Nothing = not achieved, Just (Just level) = achieved at level
        assessmentButtons competence currentLevel todayAssessment =
          let -- Get level info
              getLevelInfo lvl = Map.findWithDefault (LevelInfo T.empty False) lvl competence.levels

              -- Check if level has a description (empty = not achievable)
              hasDescription lvl = not $ T.null (getLevelInfo lvl).description

              -- Check if level is locked
              isLocked lvl = (getLevelInfo lvl).locked

              -- "Not Achieved" button
              notAchievedBtn =
                (if currentLevel == Just Nothing then Button.buttonPrimary else Button.buttonOutline)
                  (C.translate' C.LblNotAchieved)
                  & Button.withClick (SetAssessmentLevel competence Nothing)
                  & Button.renderButton

              -- Level buttons - disabled only if no description
              -- Locked levels show a lock icon indicator but remain clickable
              levelBtn lvl =
                let isActive = currentLevel == Just (Just lvl)
                    isEnabled = hasDescription lvl
                    locked = isLocked lvl
                    baseBtn = if isActive then Button.buttonPrimary else Button.buttonOutline
                 in baseBtn (C.translate' $ C.LblCompetenceLevelDescription lvl)
                      & Button.withClick (SetAssessmentLevel competence (Just lvl))
                      & Button.withDisabled (not isEnabled)
                      & (if locked then Button.withIconRight IcnLock else id)
                      & Button.renderButton

              -- Delete button (red trash icon)
              deleteBtn =
                Button.buttonDestructive ""
                  & Button.withIcon IcnDelete
                  & Button.withClick (ClearAssessment competence)
                  & Button.withDisabled (isNothing todayAssessment)
                  & Button.renderButton
           in V.viewFlow
                (V.hFlow & (#gap .~ V.TinySpace))
                [ notAchievedBtn
                , levelBtn BasicLevel
                , levelBtn IntermediateLevel
                , levelBtn AdvancedLevel
                , deleteBtn
                ]

        -- | Timeline with horizontal lanes for each level
        -- Each level gets its own row with label on left and cells for each column
        -- Columns are either evidence columns (Day, ActivityType) or assessment columns (Day)
        -- For each date: evidence columns come first, then assessment column
        -- Dates are shown in a separate lane at the bottom
        timelineForCompetence competence evidences assessments =
          let -- Collect ALL observations for this competence, grouped by level
              evidencesByLevel :: Level -> [(Day, ActivityType, SocialForm, Ability)]
              evidencesByLevel lvl =
                [ (evidence.date, evidence.activityType, obs.socialForm, obs.ability)
                | evidence <- Ix.toList evidences
                , Just obs <- [Ix.getOne (evidence.observations Ix.@= (competence.id, lvl))]
                ]

              -- Merge same (date, activityType) keeping worst ability
              mergeForLevel :: Level -> Map.Map (Day, ActivityType) (SocialForm, Ability)
              mergeForLevel lvl = Map.fromListWith mergeWorst
                [ ((date, actType), (socialForm, ability))
                | (date, actType, socialForm, ability) <- evidencesByLevel lvl
                ]

              mergeWorst (sf1, ab1) (_sf2, ab2) = (sf1, max ab1 ab2)

              -- All evidence maps for each level
              advancedMap = mergeForLevel AdvancedLevel
              intermediateMap = mergeForLevel IntermediateLevel
              basicMap = mergeForLevel BasicLevel

              -- All evidence keys across all levels
              allEvidenceKeys = nub $ Map.keys advancedMap ++ Map.keys intermediateMap ++ Map.keys basicMap

              -- Assessments grouped by level (including Nothing for "Not Achieved")
              assessmentsByLevel :: Maybe Level -> [Day]
              assessmentsByLevel lvl =
                [ a.date
                | a <- assessments
                , a.level == lvl
                ]

              -- All assessment dates (including "Not Achieved")
              allAssessmentDates = nub [ a.date | a <- assessments ]

              -- Column type: either an evidence column or an assessment column
              -- Left (Day, ActivityType) = evidence column
              -- Right Day = assessment column
              -- Sorted by date, then evidence before assessment
              allColumns :: [Either (Day, ActivityType) Day]
              allColumns =
                let evidenceCols = map Left allEvidenceKeys
                    assessmentCols = map Right allAssessmentDates
                    -- Sort: by date first, then Left (evidence) before Right (assessment)
                    colDate (Left (d, _)) = d
                    colDate (Right d) = d
                    colOrder (Left _) = 0 :: Int
                    colOrder (Right _) = 1
                 in sortOn (\c -> (colDate c, colOrder c)) (evidenceCols ++ assessmentCols)

              -- For evidence columns, compute min/max levels for range placeholders
              levelRangeForEvidence :: (Day, ActivityType) -> (Maybe Level, Maybe Level)
              levelRangeForEvidence key =
                let hasAdvanced = Map.member key advancedMap
                    hasIntermediate = Map.member key intermediateMap
                    hasBasic = Map.member key basicMap
                    presentLevels = [lvl | (lvl, present) <- [(BasicLevel, hasBasic), (IntermediateLevel, hasIntermediate), (AdvancedLevel, hasAdvanced)], present]
                 in case presentLevels of
                      [] -> (Nothing, Nothing)
                      lvls -> (Just (minimum lvls), Just (maximum lvls))

              -- Check if a level is in the range for an evidence column
              isInRangeEvidence :: (Day, ActivityType) -> Level -> Bool
              isInRangeEvidence key lvl =
                case levelRangeForEvidence key of
                  (Just minLvl, Just maxLvl) -> lvl >= minLvl && lvl <= maxLvl
                  _ -> False

              -- "Not Achieved" assessment dates
              notAchievedDates = assessmentsByLevel Nothing

           in MH.div_ [class_ "flex flex-col"]
                ( [ levelLane AdvancedLevel advancedMap (assessmentsByLevel (Just AdvancedLevel)) isInRangeEvidence allColumns
                  , levelLane IntermediateLevel intermediateMap (assessmentsByLevel (Just IntermediateLevel)) isInRangeEvidence allColumns
                  , levelLane BasicLevel basicMap (assessmentsByLevel (Just BasicLevel)) isInRangeEvidence allColumns
                  ]
                  ++ [notAchievedLane notAchievedDates allColumns | not (null notAchievedDates)]
                  ++ [dateLane allColumns]
                )
          where
            -- | One horizontal lane for a level
            -- Iterates through ALL columns and shows appropriate content
            levelLane lvl evidenceMap assessmentDates isInRangeEvidence allColumns =
              let levelInfo = Map.findWithDefault (LevelInfo T.empty False) lvl competence.levels
                  hasDesc = not (T.null levelInfo.description)
                  levelName = C.translate' (C.LblCompetenceLevelDescription lvl)
                  -- Gray out if no description or locked
                  textClass = if hasDesc && not levelInfo.locked then "text-stone-600" else "text-stone-400"

                  -- Label with info icon and CSS-based tooltip (group-hover pattern)
                  -- If locked, show lock icon instead of info icon
                  labelWithTooltip =
                    MH.div_ [class_ "w-28 flex items-center gap-1 shrink-0"]
                      [ MH.span_ [class_ $ "text-xs font-medium " <> textClass] [M.text levelName]
                      , if levelInfo.locked
                          then MH.span_ [class_ "text-stone-400"] [V.icon [] IcnLock]
                          else if hasDesc
                            then MH.span_ [class_ "group relative cursor-help"]
                                   [ V.icon [class_ "text-stone-400"] IcnInfo
                                   , -- Tooltip shown on hover via CSS
                                     MH.span_
                                       [ class_
                                           "absolute bottom-full left-0 mb-1 px-2 py-1 \
                                           \bg-stone-800 text-white text-xs rounded \
                                           \whitespace-pre-line min-w-48 max-w-64 \
                                           \opacity-0 group-hover:opacity-100 \
                                           \pointer-events-none transition-opacity z-50"
                                       ]
                                       [M.text (M.ms levelInfo.description)]
                                   ]
                            else V.empty
                      ]

                  -- Build cells for each column
                  columnCells = map (cellForColumn hasDesc evidenceMap assessmentDates isInRangeEvidence lvl) allColumns

                  -- Lane background: muted for locked levels
                  laneClass = "flex items-center min-h-[28px] border-b border-stone-100"
                    <> if levelInfo.locked then " bg-stone-50" else ""

               in MH.div_ [class_ laneClass]
                    (labelWithTooltip : columnCells)

            -- | Cell for a specific column in a level lane
            cellForColumn hasDesc evidenceMap assessmentDates isInRangeEvidence lvl col =
              case col of
                Left evKey@(_, actType) ->
                  -- Evidence column
                  case Map.lookup evKey evidenceMap of
                    Just (socialForm, ability) ->
                      -- Has evidence at this level
                      showEvidenceIcon hasDesc actType socialForm ability
                    Nothing ->
                      -- No evidence at this level - show placeholder if in range
                      if isInRangeEvidence evKey lvl
                        then showRangePlaceholder
                        else showEmptyCell
                Right day ->
                  -- Assessment column
                  if day `elem` assessmentDates
                    then showAssessmentIcon
                    else showEmptyCell

            -- | "Not Achieved" lane - only shows assessment icons, no evidence
            notAchievedLane notAchievedDates allColumns =
              let levelName = C.translate' C.LblNotAchieved
                  -- Label (no tooltip for "Not Achieved")
                  label =
                    MH.div_ [class_ "w-28 flex items-center gap-1 shrink-0"]
                      [MH.span_ [class_ "text-xs font-medium text-stone-400"] [M.text levelName]]

                  -- Build cells for each column
                  columnCells = map cellForNotAchieved allColumns

                  cellForNotAchieved col =
                    case col of
                      Left _ ->
                        -- Evidence column - always empty for "Not Achieved"
                        showEmptyCell
                      Right day ->
                        -- Assessment column - show X icon if not achieved on this day
                        if day `elem` notAchievedDates
                          then showNotAchievedIcon
                          else showEmptyCell

               in MH.div_ [class_ "flex items-center min-h-[28px] border-b border-stone-100"]
                    (label : columnCells)

            -- | Empty cell placeholder (maintains column alignment)
            showEmptyCell =
              MH.div_ [class_ "w-12"] []

            -- | Placeholder showing this level is in the min-max range
            -- Uses a subtle background color to indicate the range
            showRangePlaceholder =
              MH.div_ [class_ "w-12 h-5 bg-stone-100 rounded mx-0.5"] []

            -- | Date lane at the bottom
            -- Shows date for each column (evidence or assessment)
            dateLane columns =
              let columnLabels = map showColumnLabel columns
               in MH.div_ [class_ "flex items-center min-h-[20px] pt-1"]
                    (MH.div_ [class_ "w-28 shrink-0"] [] : columnLabels)

            -- | Show a label for a column (date for both evidence and assessment columns)
            showColumnLabel col =
              let date = case col of
                    Left (d, _) -> d
                    Right d -> d
                  dateStr = M.ms $ take 5 (M.fromMisoString (C.formatDay date) :: String)
               in MH.div_ [class_ "text-xs text-stone-500 w-12 text-center"] [M.text dateStr]

            -- | Show evidence icon for a single evidence entry
            showEvidenceIcon hasLevelDesc actType socialForm ability =
              let abilityClass = if hasLevelDesc then Colors.abilityTextClass ability else "text-stone-400"
                  activityTypeIcn = case actType of
                    Conversation -> IcnActivityTypeConversation
                    Exam -> IcnActivityTypeExam
                    SchoolExercise -> IcnActivityTypeSchoolExercise
                    HomeExercise -> IcnActivityTypeHomeExercise
                  socialFormIcn = case socialForm of
                    Group -> IcnSocialFormGroup
                    Individual -> IcnSocialFormIndividual
                  coloredIcon icn = MH.span_ [class_ abilityClass] [V.icon [MSP.stroke_ "currentColor"] icn]

               in MH.div_ [class_ "flex items-center justify-center w-12"]
                    [coloredIcon i | i <- [activityTypeIcn, socialFormIcn]]

            -- | Show assessment icon (checkmark)
            showAssessmentIcon =
              MH.div_ [class_ "flex items-center justify-center w-12"]
                [MH.span_ [class_ "text-sky-600"] [V.icon [MSP.stroke_ "currentColor"] IcnApply]]

            -- | Show "Not Achieved" icon (X mark in red)
            showNotAchievedIcon =
              MH.div_ [class_ "flex items-center justify-center w-12"]
                [MH.span_ [class_ "text-red-500"] [V.icon [MSP.stroke_ "currentColor"] IcnCancel]]

-- | Find assessment for specific day.
-- Input IxSet must be pre-filtered to the focused user.
-- Uses IxSet indexing for efficient lookup.
findAssessmentForDay'
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> Day
  -> Maybe CompetenceAssessment
findAssessmentForDay' assessments competenceId day =
  Ix.getOne $ assessments Ix.@= competenceId Ix.@= day

-- | Get the most recent (active) grid grade for a competence grid.
-- Input IxSet must be pre-filtered to the focused user.
-- Uses IxSet indexing for efficient lookup.
getActiveGridGrade'
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> CompetenceGridId
  -> Maybe CompetenceGridGrade
getActiveGridGrade' gridGrades gridId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $ gridGrades Ix.@= gridId

-- | Get the assessment history for a competence (sorted by date descending).
-- Input IxSet must be pre-filtered to the focused user.
-- Uses IxSet indexing for efficient lookup.
getAssessmentHistory'
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> [CompetenceAssessment]
getAssessmentHistory' assessments competenceId =
  Ix.toDescList (Proxy @Day) $ assessments Ix.@= competenceId
