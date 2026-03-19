module Competences.Frontend.Component.ParticipationTimeline
  ( participationTimelineComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Document.Absence (Absence (..))
import Competences.Document.Lesson (Lesson (..), LessonId)
import Competences.Document.ParticipationRecord (ParticipationLevel (..), ParticipationRecord (..), ParticipationType (..), allParticipationTypes)
import Competences.Document.User (UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View.Color (PaletteColor (..), textClass)
import Competences.Frontend.View.Color.Participation qualified as Color
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.User qualified as QUser
import Data.List (partition, sortOn)
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)

-- ---------------------------------------------------------------------------
-- Model
-- ---------------------------------------------------------------------------

-- | A column in the participation table, representing one lesson.
data LessonColumn = LessonColumn
  { lessonId :: !LessonId
  , date :: !(Maybe Day)
  , title :: !Text
  }
  deriving (Eq, Show)

-- | A single cell entry (level + optional remark).
data CellEntry = CellEntry
  { level :: !ParticipationLevel
  , remark :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | The participation timeline model: students x lessons table.
data Model = Model
  { students :: ![User]
  , lessons :: ![LessonColumn]
  , records :: !(Map.Map (UserId, LessonId, ParticipationType) CellEntry)
  , absences :: !(Set.Set (UserId, LessonId))
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  deriving (Eq, Show)

emptyModel :: Model
emptyModel = Model {students = [], lessons = [], records = Map.empty, absences = Set.empty}

-- ---------------------------------------------------------------------------
-- Component
-- ---------------------------------------------------------------------------

participationTimelineComponent :: SyncContext -> M.Component p Model Action
participationTimelineComponent docRef =
  (M.component model update view)
    { M.subs = [subscribeDocument docRef UpdateDocument]
    , M.initialAction = Nothing
    }
  where
    model = emptyModel

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange doc _)) =
      M.modify $ const $ computeTimeline doc

    view :: Model -> M.View Model Action
    view m =
      MH.div_
        [class_ "h-full min-h-0 overflow-y-auto"]
        [ MH.div_
            [class_ "space-y-4 p-4"]
            [ Typography.h2 (C.translate' C.LblParticipationTimeline)
            , if null m.lessons
                then MH.div_ [class_ "text-muted-foreground text-sm"] [M.text "—"]
                else tableView m
            ]
        ]

-- ---------------------------------------------------------------------------
-- Compute timeline
-- ---------------------------------------------------------------------------

computeTimeline :: Document -> Model
computeTimeline doc =
  let -- Build triple-key map of all participation records
      allRecords = Ix.toList doc.participationRecords
      recordMap =
        Map.fromList
          [ ((pr.userId, pr.lessonId, pr.participationType), CellEntry pr.level pr.remark)
          | pr <- allRecords
          ]

      -- Build absence set
      allAbsences = Ix.toList doc.absences
      absenceSet = Set.fromList [(a.userId, a.lessonId) | a <- allAbsences]

      -- Determine which lessons are referenced (by records or absences)
      referencedIds =
        Set.fromList [pr.lessonId | pr <- allRecords]
          <> Set.fromList [a.lessonId | a <- allAbsences]

      -- Build lesson columns: only lessons with records or absences (IxSet @+ for indexed lookup)
      lessonsWithRecords =
        Ix.toList $ doc.lessons Ix.@+ Set.toList referencedIds

      -- Sort: dated ascending by date, then undated by Order
      (dated, undated) = partition (isJust . (.date)) lessonsWithRecords

      datedSorted = sortOn (.date) dated
      undatedSorted = sortOn (.order) undated

      lessonCols =
        map toLessonColumn datedSorted
          <> map toLessonColumn undatedSorted

      students = QUser.studentsSortedByName doc
   in Model {students, lessons = lessonCols, records = recordMap, absences = absenceSet}
  where
    toLessonColumn l = LessonColumn {lessonId = l.id, date = l.date, title = l.title}

-- ---------------------------------------------------------------------------
-- Table view
-- ---------------------------------------------------------------------------

tableView :: Model -> M.View Model Action
tableView m =
  MH.div_
    [class_ "overflow-x-auto rounded-lg border"]
    [ MH.table_
        [class_ "border-collapse text-sm"]
        [ headerRow m
        , MH.tbody_ [] (map (studentRow m) m.students)
        ]
    ]

-- | Header row: sticky student column + one th per lesson
headerRow :: Model -> M.View Model Action
headerRow m =
  MH.thead_
    []
    [ MH.tr_
        []
        ( stickyHeaderTh
            : map lessonHeaderTh m.lessons
        )
    ]
  where
    stickyHeaderTh =
      MH.th_
        [ class_ "sticky left-0 z-10 bg-muted/80 border-r border-b px-3 py-2 text-left font-medium text-muted-foreground"
        ]
        [M.text ""]

    lessonHeaderTh lc =
      MH.th_
        [ class_ "border-b px-2 py-2 text-center font-medium text-muted-foreground whitespace-nowrap"
        , MP.title_ (ms lc.title)
        ]
        [M.text $ dateLabel lc.date]

    dateLabel :: Maybe Day -> MisoString
    dateLabel (Just d) = C.formatDayShort d
    dateLabel Nothing = "?"

-- | Row for a single student: one <tr> with the student name cell + per-lesson cells.
-- Each cell contains 3 vertically stacked slots (one per ParticipationType).
studentRow :: Model -> User -> M.View Model Action
studentRow m user =
  MH.tr_
    [class_ "border-b last:border-b-0 hover:bg-muted/30"]
    ( stickyStudentTd
        : map (lessonCell m user) m.lessons
    )
  where
    stickyStudentTd =
      MH.td_
        [class_ "sticky left-0 z-10 bg-card border-r px-3 py-1 whitespace-nowrap"]
        [ MH.div_
            [class_ "flex items-center justify-between gap-2"]
            [ MH.span_ [class_ "font-medium text-foreground"] [M.text $ ms user.name]
            , MH.div_
                [class_ "flex flex-col items-end text-muted-foreground"]
                (map legendIcon allParticipationTypes)
            ]
        ]

    legendIcon pt =
      MH.span_ [class_ "h-4 w-4 flex items-center justify-center"]
        [Icon.iconS Icon.Small (Color.participationTypeIcon pt)]

-- | A single cell: if absent, show sick icon spanning all 3 slots;
-- otherwise show 3 participation type slots.
lessonCell :: Model -> User -> LessonColumn -> M.View Model Action
lessonCell m user lc
  | Set.member (user.id, lc.lessonId) m.absences =
      MH.td_
        [class_ "px-1 py-0.5 align-top"]
        [ MH.div_
            [class_ "flex flex-col items-center justify-center h-12 text-muted-foreground"]
            [Icon.iconS Icon.Small Icon.IcnSick]
        ]
  | otherwise =
      MH.td_
        [class_ "px-1 py-0.5 align-top"]
        [ MH.div_
            [class_ "flex flex-col items-center gap-0"]
            (map (slotView m user lc) allParticipationTypes)
        ]

-- | A single slot within a cell: either a colored level icon or an empty spacer
slotView :: Model -> User -> LessonColumn -> ParticipationType -> M.View Model Action
slotView m user lc pt =
  case Map.lookup (user.id, lc.lessonId, pt) m.records of
    Just ce ->
      MH.span_
        [ class_ $ "h-4 w-4 flex items-center justify-center " <> entryColorClass pt ce.level
        , MP.title_ (cellTooltipText lc pt ce)
        ]
        [Icon.iconS Icon.Small (Color.participationLevelIcon pt ce.level)]
    Nothing ->
      MH.span_ [class_ "h-4 w-4"] []

-- ---------------------------------------------------------------------------
-- Color palette mapping
-- ---------------------------------------------------------------------------

entryColorClass :: ParticipationType -> ParticipationLevel -> Text
entryColorClass pType ParticipationLevel1 = textClass Base (Color.participationPalette pType)
entryColorClass pType ParticipationLevel2 = textClass Accent (Color.participationPalette pType)

-- ---------------------------------------------------------------------------
-- Tooltips
-- ---------------------------------------------------------------------------

-- | Build tooltip text for a cell entry: "DD.MM. Lesson: Type Level (remark)"
cellTooltipText :: LessonColumn -> ParticipationType -> CellEntry -> MisoString
cellTooltipText lc pt ce =
  let dayPart = case lc.date of
        Just d -> C.formatDayShort d <> " "
        Nothing -> ""
      typePart = C.translate' (C.LblParticipationType pt)
      levelPart = C.translate' (C.LblParticipationLevel pt ce.level)
      remarkPart = case ce.remark of
        Just r | r /= "" -> " (" <> ms r <> ")"
        _ -> ""
   in dayPart <> ms lc.title <> ": " <> typePart <> " " <> levelPart <> remarkPart
