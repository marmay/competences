module Competences.Frontend.Component.StatisticsOverview
  ( statisticsOverviewComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Document (Document (..), User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View.Color (bgClass')
import Competences.Frontend.View.Color.AssignmentCompletion (assignmentCompletionPalette)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.StackedBar (BarSegment (..), StackedBarConfig (..), stackedBar)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..))
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Assignment (AssignmentCompletionCategory (..), userAssignmentCompletionStats)
import Competences.Query.User qualified as QUser
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH

-- | Statistics Overview Component Model
newtype Model = Model
  { byUserStats :: Map.Map User (Map.Map AssignmentCompletionCategory Int)
  }
  deriving (Eq, Generic, Show)

-- | Statistics Overview Component Actions
data Action
  = UpdateDocument !DocumentChange
  deriving (Eq, Show)

-- | Empty model for statistics overview
emptyModel :: Model
emptyModel = Model {byUserStats = Map.empty}

-- | Statistics Overview Component
statisticsOverviewComponent :: SyncContext -> M.Component p Model Action
statisticsOverviewComponent docRef =
  (M.component model update view)
    { M.subs = [subscribeDocument docRef UpdateDocument]
    , M.initialAction = Nothing
    }
  where
    model = emptyModel

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange doc _)) =
      M.modify $ const $ computeStats doc

    view :: Model -> M.View Model Action
    view m =
      MH.div_
        [class_ "h-full min-h-0 overflow-y-auto"]
        [ Layout.vFlow'
            [ Typography.h2 (C.translate' C.LblStatisticsOverview)
            , Layout.vFlow
                Layout.gapS
                (map (studentRow m) sortedStudents)
            ]
        ]
      where
        sortedStudents = sortBy (comparing (.name)) $ Map.keys m.byUserStats

-- | Derive max assignment count across all students (for bar scaling)
maxAssignments :: Model -> Int
maxAssignments m =
  let totals = map (sum . Map.elems) (Map.elems m.byUserStats)
   in if null totals then 0 else maximum totals

-- | Render a single student row with name + stacked bar
studentRow :: Model -> User -> M.View Model Action
studentRow m user =
  MH.div_
    [class_ "flex gap-3 py-1"]
    [ MH.div_
        [class_ "w-32 shrink-0 truncate text-sm font-medium text-foreground"]
        [M.text $ M.ms user.name]
    , MH.div_
        [class_ "flex-1 min-w-0"]
        [ case m.byUserStats Map.!? user of
            Just stats -> renderBar (maxAssignments m) stats
            Nothing -> M.text ""
        ]
    ]

-- | Render a stacked bar for a user's assignment completion stats
renderBar :: Int -> Map.Map AssignmentCompletionCategory Int -> M.View Model Action
renderBar maxTotal stats =
  stackedBar $
    StackedBarConfig
      { total = maxTotal
      , segments = map toSegment categories
      }
  where
    categories =
      [ (AsgCompleted, C.translate' C.LblAsgCompleted)
      , (AsgCorrectedNotDone, C.translate' C.LblAsgCorrectedNotDone)
      , (AsgSubmittedNotCorrected, C.translate' C.LblAsgSubmittedNotCorrected)
      , (AsgVoid, C.translate' C.LblAsgVoid)
      , (AsgNotSubmitted, C.translate' C.LblAsgNotSubmitted)
      ]

    toSegment (cat, lbl) =
      let count = Map.findWithDefault 0 cat stats
       in BarSegment
            { count = count
            , colorClass = bgClass' (assignmentCompletionPalette cat)
            , tooltip = if count == 0 then NoTooltip else PlainTooltip lbl
            }

computeStats :: Document -> Model
computeStats document =
  let students = QUser.students document
      byUserStats =
        Map.fromList $
          map (\user -> (user, userAssignmentCompletionStats document user.id)) students
   in Model {byUserStats}
