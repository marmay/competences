module Competences.Frontend.Component.CompetenceGrid.Grading
  ( gradingDetailView
  )
where

import Competences.Command (Command (..), CompetenceGridGradesCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceAssessment (..)
  , CompetenceAssessmentIxs
  , CompetenceGrid (..)
  , CompetenceGridId
  , CompetenceIxs
  , Document (..)
  , Level (..)
  , LevelInfo (..)
  , allLevels
  , ordered
  )
import Competences.Document.Competence (CompetenceId)
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..), CompetenceGridGradeId, CompetenceGridGradeIxs)
import Competences.Document.Grade (Grade (..), grades, gradeToText)
import Data.Proxy (Proxy (..))
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)
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
import Competences.Frontend.View.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Time (Day, getCurrentTime, utctDay)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~))
import System.IO.Unsafe (unsafePerformIO)

-- ============================================================================
-- GRADING MODE DETAIL
-- ============================================================================

-- | Projection type for the grading view - contains only the data needed for this view.
-- This is grid-specific: competences are filtered to the grid, user data filtered to focused user.
data GradingProjection = GradingProjection
  { competences :: !(Ix.IxSet CompetenceIxs Competence)
  -- ^ Competences for this grid only
  , userAssessments :: !(Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment)
  -- ^ Assessments for focused user only
  , userGridGrades :: !(Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade)
  -- ^ Grid grades for focused user only (for history)
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Model for the grading detail component
data GradingModel = GradingModel
  { projection :: !GradingProjection
  , selectedGrade :: !(Maybe Grade) -- Currently selected grade for entry
  , gradeComment :: !T.Text -- Comment for new grade
  , today :: !(Maybe Day) -- Current date for creating grades
  }
  deriving (Eq, Generic, Show)

-- | Action for the grading detail component
data GradingAction
  = GradingProjectionChanged !(ProjectedChange GradingProjection)
  | SelectGrade !(Maybe Grade)
  | SetGradeComment !M.MisoString
  | SubmitGrade
  | DeleteGrade !CompetenceGridGradeId
  | InitToday !Day
  deriving (Eq, Show)

-- | View for the grading detail - shows condensed competence grid with grade entry
gradingDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
gradingDetailView r grid =
  V.component
    ("competence-grid-grading-" <> M.ms (show grid.id))
    (gradingComponent r grid)

gradingComponent :: SyncContext -> CompetenceGrid -> M.Component p GradingModel GradingAction
gradingComponent r grid =
  (M.component model update view)
    { M.subs = [subscribeWithProjection r gradingProjection GradingProjectionChanged]
    , M.initialAction = Just initTodayAction
    }
  where
    -- Projection function captures the grid parameter
    gradingProjection :: Document -> Maybe User -> GradingProjection
    gradingProjection doc mUser = GradingProjection
      { competences = doc.competences Ix.@= grid.id
      , userAssessments = case mUser of
          Nothing -> Ix.empty
          Just u -> doc.competenceAssessments Ix.@= u.id
      , userGridGrades = case mUser of
          Nothing -> Ix.empty
          Just u -> doc.competenceGridGrades Ix.@= u.id
      , focusedUser = mUser
      }

    emptyProjection = GradingProjection Ix.empty Ix.empty Ix.empty Nothing
    model = GradingModel emptyProjection Nothing T.empty Nothing

    initTodayAction :: GradingAction
    initTodayAction = InitToday $ unsafePerformIO $ utctDay <$> getCurrentTime

    update (GradingProjectionChanged change) =
      M.modify $ #projection .~ change.projection

    update (InitToday day) =
      M.modify $ #today .~ Just day

    update (SelectGrade grade) =
      M.modify $ #selectedGrade .~ grade

    update (SetGradeComment txt) =
      M.modify $ #gradeComment .~ M.fromMisoString txt

    update SubmitGrade = do
      m <- M.get
      case (m.projection.focusedUser, m.selectedGrade, m.today) of
        (Just user, Just grade, Just day) -> do
          M.io_ $ do
            gradeId <- nextId r
            let gridGrade =
                  CompetenceGridGrade
                    { id = gradeId
                    , userId = user.id
                    , competenceGridId = grid.id
                    , grade = grade
                    , date = day
                    , comment = if T.null m.gradeComment then Nothing else Just m.gradeComment
                    }
            modifySyncDocument r $ CompetenceGridGrades $ OnCompetenceGridGrades $ Create gridGrade
          -- Clear selection after submit
          M.modify $ \s -> s & #selectedGrade .~ Nothing & #gradeComment .~ T.empty
        _ -> pure ()

    update (DeleteGrade gradeId) =
      M.io_ $ modifySyncDocument r $ CompetenceGridGrades $ OnCompetenceGridGrades $ Delete gradeId

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
          , competencesTable
          , gradeEntrySection m
          , gradeHistorySection
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

        -- Condensed competence table showing assessment status
        competencesTable =
          V.viewTable $
            V.defTable
              { V.columns =
                  [GradingDescriptionColumn]
                    <> map GradingLevelColumn allLevels
              , V.rows = ordered proj.competences
              , V.columnSpec = \case
                  GradingDescriptionColumn ->
                    Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                  GradingLevelColumn l ->
                    Table.TableColumnSpec Table.EqualWidthColumn (C.translate' $ C.LblCompetenceLevelDescription l)
              , V.rowContents = V.cellContentsWithSpec $ \competence -> \case
                  GradingDescriptionColumn ->
                    -- Description cell: shows overall competence status
                    let mAssessment = getActiveAssessment' proj.userAssessments competence.id
                        bgClass = case mAssessment of
                          Nothing -> "" -- No assessment: white
                          Just assessment -> case assessment.level of
                            Nothing -> "bg-yellow-100" -- Not achieved at all
                            Just _ -> "bg-green-100" -- Achieved at some level
                     in Table.TableCellSpec
                          { Table.cellClasses = "px-4 py-2 " <> bgClass
                          , Table.cellStyle = []
                          , Table.cellContent = Typography.small (M.ms competence.description)
                          }
                  GradingLevelColumn level ->
                    let levelInfo = Map.findWithDefault (LevelInfo T.empty False) level competence.levels
                        hasDescription = not (T.null levelInfo.description)

                        -- Get active assessment for focused user + competence
                        mAssessment = getActiveAssessment' proj.userAssessments competence.id

                        -- Determine cell assessment status
                        cellStatus = case mAssessment of
                          Nothing -> NoAssessment
                          Just assessment -> case assessment.level of
                            Nothing -> NotYetAchieved
                            Just assessedLevel ->
                              if level <= assessedLevel
                                then Achieved
                                else NotYetAchieved

                        -- Cell background color based on status
                        bgClass
                          | not hasDescription = ""
                          | cellStatus == Achieved = "bg-green-100"
                          | levelInfo.locked = "bg-stone-200"
                          | cellStatus == NotYetAchieved = "bg-yellow-100"
                          | otherwise = "bg-white"

                        -- Striped background for empty cells
                        stripeStyle =
                          if not hasDescription
                            then
                              [ ( "background"
                                , "repeating-linear-gradient(135deg, rgb(245 245 244) 0px, rgb(245 245 244) 4px, rgb(231 229 228) 4px, rgb(231 229 228) 8px)"
                                )
                              ]
                            else []

                        -- Status icon in cell center
                        statusIcon
                          | not hasDescription = V.empty
                          | cellStatus == Achieved =
                              MH.div_
                                [class_ "text-green-600 flex justify-center"]
                                [V.icon [MP.width_ "16", MP.height_ "16"] IcnApply]
                          | levelInfo.locked =
                              MH.div_
                                [class_ "text-stone-500 flex justify-center"]
                                [V.icon [MP.width_ "16", MP.height_ "16"] IcnLock]
                          | cellStatus == NotYetAchieved =
                              MH.div_
                                [class_ "text-yellow-600 flex justify-center"]
                                [V.icon [MP.width_ "16", MP.height_ "16"] IcnProgress]
                          | otherwise = V.empty
                     in Table.TableCellSpec
                          { Table.cellClasses = "px-2 py-2 " <> bgClass
                          , Table.cellStyle = stripeStyle
                          , Table.cellContent = statusIcon
                          }
              }

        -- Grade entry section with grade buttons and comment input
        gradeEntrySection gm =
          Card.cardWithHeader (C.translate' C.LblEnterGrade) Nothing
            [ V.viewFlow
                (V.vFlow & (#gap .~ V.SmallSpace))
                [ -- Grade buttons row
                  V.viewFlow
                    (V.hFlow & (#gap .~ V.TinySpace))
                    [ gradeButton gm g | g <- grades ]
                , -- Comment input and submit button row
                  V.viewFlow
                    (V.hFlow & (#gap .~ V.SmallSpace) & (#expandOrthogonal .~ V.Expand V.Center))
                    [ MH.div_
                        [class_ "flex-1"]
                        [ Input.textInput'
                            (C.translate' C.LblGradeComment)
                            (M.ms gm.gradeComment)
                            SetGradeComment
                        ]
                    , Button.buttonPrimary (C.translate' C.LblApply)
                        & Button.withClick SubmitGrade
                        & Button.withDisabled (gm.selectedGrade == Nothing)
                        & Button.renderButton
                    ]
                ]
            ]

        gradeButton gm g =
          let isSelected = gm.selectedGrade == Just g
              -- Short label for button (just the number part)
              shortLabel = case g of
                Grade1 -> "1"
                Grade1_2 -> "1-2"
                Grade2 -> "2"
                Grade2_3 -> "2-3"
                Grade3 -> "3"
                Grade3_4 -> "3-4"
                Grade4 -> "4"
                Grade4_5 -> "4-5"
                Grade5 -> "5"
           in (if isSelected then Button.buttonPrimary else Button.buttonOutline) shortLabel
                & Button.withClick (SelectGrade (Just g))
                & Button.renderButton

        -- Grade history section
        gradeHistorySection =
          let history = getGridGradeHistory' proj.userGridGrades grid.id
           in if null history
                then V.empty
                else
                  Card.cardWithHeader (C.translate' C.LblGradeHistory) Nothing
                    [ MH.div_
                        [class_ "flex flex-col gap-2"]
                        [ gradeHistoryItem g | g <- history ]
                    ]

        gradeHistoryItem g =
          MH.div_
            [class_ "flex items-center justify-between py-2 border-b border-stone-100 last:border-0"]
            [ MH.div_
                [class_ "flex items-center gap-3"]
                [ -- Date
                  MH.span_
                    [class_ "text-sm text-stone-500"]
                    [M.text (C.formatDay g.date)]
                , -- Grade
                  MH.span_
                    [class_ "text-sm font-medium"]
                    [M.text (M.ms $ gradeToText g.grade)]
                , -- Comment (if any)
                  case g.comment of
                    Just c ->
                      MH.span_
                        [class_ "text-sm text-stone-600 italic"]
                        [M.text (M.ms c)]
                    Nothing -> V.empty
                ]
            , -- Delete button
              Button.buttonDestructive ""
                & Button.withIcon IcnDelete
                & Button.withClick (DeleteGrade g.id)
                & Button.renderButton
            ]

-- | Column type for grading table
data GradingColumn
  = GradingDescriptionColumn
  | GradingLevelColumn !Level
  deriving (Eq, Show)

-- | Assessment status for a cell in the grading view
data CellAssessmentStatus
  = Achieved
  | NotYetAchieved
  | NoAssessment
  deriving (Eq, Show)

-- | Get the most recent (active) grid grade for a competence grid.
-- Input IxSet must be pre-filtered to the focused user.
-- Uses IxSet indexing for efficient lookup.
getActiveGridGrade'
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> CompetenceGridId
  -> Maybe CompetenceGridGrade
getActiveGridGrade' gridGrades gridId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $ gridGrades Ix.@= gridId

-- | Get the grade history for a competence grid (sorted by date descending).
-- Input IxSet must be pre-filtered to the focused user.
-- Uses IxSet indexing for efficient lookup.
getGridGradeHistory'
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> CompetenceGridId
  -> [CompetenceGridGrade]
getGridGradeHistory' gridGrades gridId =
  Ix.toDescList (Proxy @Day) $ gridGrades Ix.@= gridId

-- | Get the most recent (active) assessment for a competence.
-- Input IxSet must be pre-filtered to the focused user.
-- Uses IxSet indexing for efficient lookup.
getActiveAssessment'
  :: Ix.IxSet CompetenceAssessmentIxs CompetenceAssessment
  -> CompetenceId
  -> Maybe CompetenceAssessment
getActiveAssessment' assessments competenceId =
  listToMaybe $ Ix.toDescList (Proxy @Day) $ assessments Ix.@= competenceId
