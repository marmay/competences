module Competences.Frontend.Component.CompetenceGrid.Grading
  ( pinCompetenceGridGrading
  )
where

import Competences.Command (Command (..), CompetenceGridGradesCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceAssessment (..)
  , CompetenceAssessmentIxs
  , CompetenceGrid (..)
  , CompetenceIxs
  , Document (..)
  , Level (..)
  , LevelInfo (..)
  , allLevels
  , ordered
  )
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..), CompetenceGridGradeId, CompetenceGridGradeIxs)
import Competences.Document.Grade (Grade (..), grades, gradeToText)
import Competences.Query.Competence qualified as QCompetence
import Competences.Query.CompetenceAssessment qualified as QAssessment
import Competences.Query.CompetenceGridGrade qualified as QGridGrade
import Competences.Document.User (User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Document.Id (idToText)
import Competences.Frontend.SyncContext.WindowManager
  ( PinCategory (..)
  , PinMeta (..)
  , SortAtom (..)
  , SortKey (..)
  , WindowChrome (..)
  , pinDialogWith
  )
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.Fragment.GradeBadge (gradeBadgeView)
import Competences.Frontend.View.Input qualified as Input
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Table qualified as Table
import Competences.Frontend.View.CellStyle qualified as CellStyle
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.StatusIcon qualified as StatusIcon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (Day, getCurrentTime, utctDay)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (?~), (.~))
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (isJust)

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

-- | Pin the competence-grid grading view as a persistent dialog.
pinCompetenceGridGrading :: SyncContext -> CompetenceGrid -> IO ()
pinCompetenceGridGrading r grid =
  let chrome = WindowChrome (M.ms grid.title) Icon.IcnEvidence Nothing
      meta = PinMeta
        { key = "grid-grade-" <> idToText grid.id
        , category = PinCatCompetenceGrid
        , sortKey = SortKey [SortAtom grid.order, SortAtom grid.id]
        , context = Nothing
        , isEditor = False
        , followUp = True
        }
   in pinDialogWith r.windowManager
        meta
        chrome
        (\_ (_ :: Maybe ()) -> gradingComponent r grid)

gradingComponent :: SyncContext -> CompetenceGrid -> M.Component p GradingModel GradingAction
gradingComponent r grid =
  (M.component model update view)
    { M.subs = [subscribeWithProjection r gradingProjection GradingProjectionChanged]
    , M.mount = Just initTodayAction
    }
  where
    -- Projection function captures the grid parameter
    gradingProjection :: Document -> Maybe User -> GradingProjection
    gradingProjection doc mUser = GradingProjection
      { competences = QCompetence.gridCompetences doc grid.id
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
      M.modify $ (#today ?~ day)

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
        Layout.vFlow
          (Layout.gapS <> Layout.wFull <> Layout.crossCenter)
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
            [class_ "w-full"]
            [ Layout.hFlow
                (Layout.hFull <> Layout.crossCenter)
                [ Typography.h2 (M.ms grid.title)
                , Layout.flowSpring
                , case QGridGrade.activeGridGrade proj.userGridGrades grid.id of
                    Just gridGrade -> gradeBadgeView gridGrade.grade
                    Nothing -> Layout.empty
                ]
            ]
        description = Typography.paragraph (M.ms grid.description)

        -- Condensed competence table showing assessment status
        competencesTable =
          Table.viewTable $
            Table.defTable
              { Table.columns =
                  [GradingDescriptionColumn]
                    <> map GradingLevelColumn allLevels
              , Table.rows = ordered proj.competences
              , Table.columnSpec = \case
                  GradingDescriptionColumn ->
                    Table.TableColumnSpec Table.AutoSizedColumn (C.translate' C.LblCompetenceDescription)
                  GradingLevelColumn l ->
                    Table.TableColumnSpec Table.EqualWidthColumn (C.translate' $ C.LblCompetenceLevelDescription l)
              , Table.rowContents = Table.cellContentsWithSpec $ \competence -> \case
                  GradingDescriptionColumn ->
                    -- Description cell: shows overall competence status
                    let mAssessment = QAssessment.activeAssessment proj.userAssessments competence.id
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
                        mAssessment = QAssessment.activeAssessment proj.userAssessments competence.id

                        -- Determine cell assessment status
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

                        -- Striped background for empty cells
                        stripeStyle = if not hasDescription then CellStyle.stripedStyle else []

                        -- Status icon in cell center
                        statusIcon = StatusIcon.statusIcon cellVisualStatus
                     in Table.TableCellSpec
                          { Table.cellClasses = "px-2 py-2 " <> bgClass
                          , Table.cellStyle = stripeStyle
                          , Table.cellContent = statusIcon
                          }
              }

        -- Grade entry section with grade buttons and comment input
        gradeEntrySection gm =
          Card.cardWithHeader (C.translate' C.LblEnterGrade) Nothing
            [ Layout.vFlow Layout.gapS
                [ -- Grade buttons row
                  Layout.hFlow Layout.gapT
                    [ gradeButton gm g | g <- grades ]
                , -- Comment input and submit button row
                  Layout.hFlow
                    (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
                    [ MH.div_
                        [class_ "flex-1"]
                        [ Input.textInput'
                            (C.translate' C.LblGradeComment)
                            (M.ms gm.gradeComment)
                            SetGradeComment
                        ]
                    , Button.primary (Button.button C.LblApply (isJust gm.selectedGrade, SubmitGrade))
                    ]
                ]
            ]

        gradeButton gm g =
          let isSelected = gm.selectedGrade == Just g
              -- Short label for button (just the number part)
              shortLabel :: M.MisoString
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
           in Button.toggle isSelected (Button.button shortLabel (SelectGrade (Just g)))

        -- Grade history section
        gradeHistorySection =
          let history = QGridGrade.gridGradeHistory proj.userGridGrades grid.id
           in if null history
                then Layout.empty
                else
                  Card.cardWithHeader (C.translate' C.LblGradeHistory) Nothing
                    [ Layout.vFlow Layout.gapS
                        [ gradeHistoryItem g | g <- history ]
                    ]

        gradeHistoryItem g =
          MH.div_
            [class_ "py-2 border-b border-stone-100 last:border-0"]
            [ Layout.hFlow
                (Layout.hFull <> Layout.crossCenter)
                [ Layout.hFlow
                    (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
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
                        Nothing -> Layout.empty
                    ]
                , Layout.flowSpring
                , -- Delete button
                  Button.deleteButton (DeleteGrade g.id)
                ]
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

