module Competences.Frontend.Component.ParticipationTimeline
  ( participationTimelineComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User (..))
import Competences.Document.Lesson (Lesson (..), LessonId)
import Competences.Document.ParticipationRecord (ParticipationLevel (..), ParticipationRecord (..), ParticipationType (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, subscribeDocument)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Query.User qualified as QUser
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString, ms)

-- | A single timeline entry for one participation record
data TimelineEntry = TimelineEntry
  { day :: !(Maybe Day)
  , participationType :: !ParticipationType
  , level :: !ParticipationLevel
  , remark :: !(Maybe Text)
  , lessonTitle :: !Text
  }
  deriving (Eq, Show)

-- | Model: per-student list of timeline entries sorted by day
newtype Model = Model
  { byUser :: Map.Map User [TimelineEntry]
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  deriving (Eq, Show)

emptyModel :: Model
emptyModel = Model {byUser = Map.empty}

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
        [ Layout.vFlow'
            [ Typography.h2 (C.translate' C.LblParticipationTimeline)
            , Layout.vFlow
                Layout.gapS
                (map (studentRow m) sortedStudents)
            ]
        ]
      where
        sortedStudents = sortBy (comparing (.name)) $ Map.keys m.byUser

-- | Build the timeline model from a document
computeTimeline :: Document -> Model
computeTimeline doc =
  let lessonInfo :: Map.Map LessonId (Maybe Day, Text)
      lessonInfo =
        Map.fromList $
          map (\l -> (l.id, (l.date, l.title))) $
            Ix.toList doc.lessons

      students = QUser.students doc

      buildEntries :: User -> [TimelineEntry]
      buildEntries user =
        let records = Ix.toList $ doc.participationRecords Ix.@= user.id
            toEntry pr = case Map.lookup pr.lessonId lessonInfo of
              Just (mDay, title) ->
                Just $
                  TimelineEntry
                    { day = mDay
                    , participationType = pr.participationType
                    , level = pr.level
                    , remark = pr.remark
                    , lessonTitle = title
                    }
              Nothing -> Nothing
            entries = mapMaybe toEntry records
         in sortBy (comparing dayKey) entries

      dayKey :: TimelineEntry -> (Down Bool, Maybe Day)
      dayKey e = case e.day of
        Just d -> (Down True, Just d)
        Nothing -> (Down False, Nothing)

      byUser =
        Map.fromList $
          map (\user -> (user, buildEntries user)) students
   in Model {byUser}

-- | Render a single student row
studentRow :: Model -> User -> M.View Model Action
studentRow m user =
  MH.div_
    [class_ "flex gap-3 py-1 items-center"]
    [ MH.div_
        [class_ "w-32 shrink-0 truncate text-sm font-medium text-foreground"]
        [M.text $ ms user.name]
    , MH.div_
        [class_ "flex flex-wrap gap-1"]
        (case m.byUser Map.!? user of
          Just entries -> map entryView entries
          Nothing -> [])
    ]

-- | Render a single icon-pair for a timeline entry
entryView :: TimelineEntry -> M.View Model Action
entryView e =
  withTooltip (PlainTooltip (entryTooltipText e)) $
    MH.span_
      [class_ $ "inline-flex items-center" <> destructiveClass e.participationType]
      [ Icon.iconS Icon.Small (typeIcon e.participationType)
      , Icon.iconS Icon.Small (levelIcon e.participationType e.level)
      ]

-- | CSS class for destructive (PoorWorkEthic) entries
destructiveClass :: ParticipationType -> Text
destructiveClass PoorWorkEthic = " text-destructive"
destructiveClass _ = ""

-- | Icon for the participation type
typeIcon :: ParticipationType -> Icon.Icon
typeIcon Participation = Icon.IcnSocialFormIndividual
typeIcon Collaboration = Icon.IcnSocialFormGroup
typeIcon PoorWorkEthic = Icon.IcnTask

-- | Icon for the level within a participation type
levelIcon :: ParticipationType -> ParticipationLevel -> Icon.Icon
levelIcon PoorWorkEthic ParticipationLevel1 = Icon.IcnMinus
levelIcon PoorWorkEthic ParticipationLevel2 = Icon.IcnMinusMinus
levelIcon _ ParticipationLevel1 = Icon.IcnPlus
levelIcon _ ParticipationLevel2 = Icon.IcnPlusPlus

-- | Build tooltip text: "DD.MM. Lesson: Type Level (remark)"
entryTooltipText :: TimelineEntry -> MisoString
entryTooltipText e =
  let dayPart = case e.day of
        Just d -> C.formatDayShort d <> " "
        Nothing -> ""
      typePart = C.translate' (C.LblParticipationType e.participationType)
      levelPart = C.translate' (C.LblParticipationLevel e.participationType e.level)
      remarkPart = case e.remark of
        Just r | r /= "" -> " (" <> ms r <> ")"
        _ -> ""
   in dayPart <> ms e.lessonTitle <> ": " <> typePart <> " " <> levelPart <> remarkPart
