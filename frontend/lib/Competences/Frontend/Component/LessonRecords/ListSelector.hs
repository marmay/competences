-- | Lesson records (Schulübung) list selector — config wrapper
-- around 'listSelectorComponent'.
--
-- The "entity" rendered here is a synthetic 'LessonRow' (LessonId +
-- derived title + date), built by the projection from lessons that
-- have a date and at least one published item. The page binds to a
-- 'Maybe LessonRow' and extracts the 'LessonId' as needed (URL push,
-- detail view).
--
-- URL-bound via the @ManageLessonRecords@ route. On first projection
-- snapshot the deep-link (if any) is preserved when it resolves
-- against the current rows; otherwise the most recent row is picked.
module Competences.Frontend.Component.LessonRecords.ListSelector
  ( LessonRow (..)
  , lessonRecordsListSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Lesson (Lesson (..), LessonId, LessonItem (..), LessonPhase (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Lesson.Detailed (lessonDerivedTitle)
import Competences.Frontend.Component.Selector.List
  ( Action (..)
  , ItemRenderer (..)
  , ListSelectorConfig (..)
  , Model
  , listSelectorComponent
  )
import Competences.Frontend.Component.Selector.UriBinding (pageBinding)
import Competences.Frontend.Fragment.SelectorFilter (searchOnlyFilter)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core (Lens')

-- | Synthetic row rendered by the selector — 'Lesson's flat surface
-- area for browsing.
data LessonRow = LessonRow
  { lessonId :: !LessonId
  , title :: !Text
  , date :: !Day
  }
  deriving (Eq, Show, Generic, Ord)

type Selected = LessonRow

-- | Indices for the synthetic row IxSet.
type LessonRowIxs = '[LessonId, Day]

instance Ix.Indexable LessonRowIxs LessonRow where
  indices =
    Ix.ixList
      (Ix.ixFun $ \r -> [r.lessonId])
      (Ix.ixFun $ \r -> [r.date])

type Projection = Ix.IxSet LessonRowIxs LessonRow

lessonRecordsListSelectorComponent
  :: SyncContext
  -> Maybe LessonId
  -- ^ Deep-link selection (typically from URL). Preserved on the
  -- first projection snapshot if it resolves against current rows.
  -> Lens' p (Maybe Selected)
  -> M.Component p (Model Selected Projection Text) (Action Selected Projection Text)
lessonRecordsListSelectorComponent r mDeepLink parentLens =
  listSelectorComponent r (config mDeepLink parentLens)

config
  :: Maybe LessonId
  -> Lens' p (Maybe Selected)
  -> ListSelectorConfig p Selected Projection LessonRowIxs LessonId Text Text
config mDeepLink parentLens =
  ListSelectorConfig
    { title = C.translate' C.LblLessonRecords
    , project = \doc _user -> projectRows doc
    , emptyProjection = Ix.empty
    , entitiesOf = id
    , itemsInOrder = sortOn (Down . (.date)) . Ix.toList
    , idOf = (.lessonId)
    , itemView = ItemRenderer renderItem
    , createActions = []
    , uriBinding =
        Just $ pageBinding (LessonRecords . Just) $ \case
          LessonRecords (Just lid) -> Just lid
          _ -> Nothing
    , initialPick = Just $ \xs ->
        case mDeepLink of
          Just lid -> case Ix.getOne (xs Ix.@= lid) of
            Just hit -> Just hit
            Nothing -> mostRecent xs
          Nothing -> mostRecent xs
    , filter = searchOnlyFilter (C.translate' C.LblFilterLessonRecords) (.title)
    , parentLens = parentLens
    }
  where
    mostRecent xs = listToMaybe (Ix.toDescList (Proxy @Day) xs)

renderItem
  :: Selected
  -> Projection
  -> Bool
  -> M.View m (Action Selected Projection Text)
renderItem row _proj isSel =
  SL.selectorItemMultiLine
    isSel
    [ MH.span_ [class_ "text-sm font-medium truncate"] [M.text (ms row.title)]
    , MH.span_
        [class_ "text-xs text-muted-foreground"]
        [M.text (C.formatDay row.date)]
    ]
    (Pick row)

projectRows :: Document -> Projection
projectRows doc =
  Ix.fromList
    [ LessonRow{lessonId = l.id, title = lessonDerivedTitle l, date = d}
    | l <- Ix.toAscList (Proxy @LessonId) doc.lessons
    , hasPublished l
    , Just d <- [l.date]
    ]

hasPublished :: Lesson -> Bool
hasPublished l =
  any (\p -> any (.publish) p.items) l.phases
    || any (.publish) l.supplementalItems
