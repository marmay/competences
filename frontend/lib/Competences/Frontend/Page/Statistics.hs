module Competences.Frontend.Page.Statistics
  ( statisticsPage
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Document (Document (..), User (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, SyncDocumentEnv (..), subscribeDocument, syncDocumentEnv)
import Competences.Frontend.View.Color (bgClass')
import Competences.Frontend.View.Color.AssignmentCompletion (assignmentCompletionPalette)
import Competences.Frontend.View.HoverMenu qualified as HoverMenu
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.StackedBar (BarSegment (..), StackedBarConfig (..), stackedBarOnly)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..))
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.Assignment (AssignmentCompletionCategory (..), userAssignmentCompletionStats)
import Competences.Query.User qualified as QUser
import Data.List (sortBy)
import Data.Time (Day)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString, ms)

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
statisticsPage :: SyncContext -> M.Component p Model Action
statisticsPage docRef =
  (M.component model update view)
    { M.subs = [subscribeDocument docRef UpdateDocument]
    , M.initialAction = Nothing
    }
  where
    model = emptyModel

    today = (syncDocumentEnv docRef).currentDay

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange doc _)) =
      M.modify $ const $ computeStats today doc

    view :: Model -> M.View Model Action
    view m =
      MH.div_
        [class_ "h-full min-h-0 overflow-y-auto"]
        [ Layout.vFlow'
            [ Typography.h2 (C.translate' C.LblStatisticsOverview)
            , Layout.vFlow
                Layout.gapS
                (map (studentRow m) sortedStudents)
            , categoryLegend
            ]
        ]
      where
        sortedStudents = sortBy (comparing (.name)) $ Map.keys m.byUserStats

-- | Derive max assignment count across all students (for bar scaling)
maxAssignments :: Model -> Int
maxAssignments m =
  let totals = map (sum . Map.elems) (Map.elems m.byUserStats)
   in if null totals then 0 else maximum totals

-- | Category definitions shared by bars, hover details, and legend
categories :: [(AssignmentCompletionCategory, MisoString)]
categories =
  [ (AsgCompleted, C.translate' C.LblAsgCompleted)
  , (AsgCorrectedNotDone, C.translate' C.LblAsgCorrectedNotDone)
  , (AsgSubmittedNotCorrected, C.translate' C.LblAsgSubmittedNotCorrected)
  , (AsgVoid, C.translate' C.LblAsgVoid)
  , (AsgNotSubmitted, C.translate' C.LblAsgNotSubmitted)
  , (AsgOverdue, C.translate' C.LblAsgOverdue)
  ]

-- | Render a single student row: name + bar + hover details trigger
studentRow :: Model -> User -> M.View Model Action
studentRow m user =
  MH.div_
    [class_ "flex gap-3 py-1 items-center"]
    [ MH.div_
        [class_ "w-32 shrink-0 truncate text-sm font-medium text-foreground"]
        [M.text $ ms user.name]
    , MH.div_
        [class_ "flex-1 min-w-0"]
        [ case m.byUserStats Map.!? user of
            Just stats -> renderBar (maxAssignments m) stats
            Nothing -> M.text ""
        ]
    , case m.byUserStats Map.!? user of
        Just stats -> detailsHover stats
        Nothing -> M.text ""
    ]

-- | Render just the stacked bar (no legend)
renderBar :: Int -> Map.Map AssignmentCompletionCategory Int -> M.View Model Action
renderBar maxTotal stats =
  stackedBarOnly $
    StackedBarConfig
      { total = maxTotal
      , segments = map toSegment categories
      }
  where
    toSegment (cat, _lbl) =
      let count = Map.findWithDefault 0 cat stats
       in BarSegment
            { count = count
            , colorClass = bgClass' (assignmentCompletionPalette cat)
            , tooltip = NoTooltip
            }

-- | Hover menu showing per-category counts for one student
detailsHover :: Map.Map AssignmentCompletionCategory Int -> M.View Model Action
detailsHover stats =
  let total = sum (Map.elems stats)
      trigger =
        MH.div_
          [class_ "text-xs text-muted-foreground tabular-nums cursor-default"]
          [M.text $ ms (show total)]
   in HoverMenu.hoverMenuRight trigger $
        map detailEntry categories
  where
    detailEntry (cat, lbl) =
      let count = Map.findWithDefault 0 cat stats
       in MH.div_
            [class_ "flex items-center gap-2 px-3 py-1 text-sm"]
            [ MH.div_ [class_ $ "w-2.5 h-2.5 rounded-sm " <> bgClass' (assignmentCompletionPalette cat)] []
            , MH.span_ [class_ "flex-1 text-popover-foreground"] [M.text lbl]
            , MH.span_
                [class_ $ "tabular-nums font-medium " <> if count > 0 then "text-popover-foreground" else "text-muted-foreground"]
                [M.text $ ms $ show count]
            ]

-- | Shared legend at the bottom explaining all category colors
categoryLegend :: M.View Model Action
categoryLegend =
  MH.div_
    [class_ "pt-4 border-t border-border"]
    [ MH.div_
        [class_ "flex flex-wrap gap-x-4 gap-y-1"]
        (map legendItem categories)
    ]
  where
    legendItem (cat, lbl) =
      MH.div_
        [class_ "flex items-center gap-1.5 text-xs text-muted-foreground"]
        [ MH.div_ [class_ $ "w-2.5 h-2.5 rounded-sm " <> bgClass' (assignmentCompletionPalette cat)] []
        , MH.span_ [] [M.text lbl]
        ]

computeStats :: Day -> Document -> Model
computeStats today document =
  let students = QUser.students document
      byUserStats =
        Map.fromList $
          map (\user -> (user, userAssignmentCompletionStats today document user.id)) students
   in Model {byUserStats}
