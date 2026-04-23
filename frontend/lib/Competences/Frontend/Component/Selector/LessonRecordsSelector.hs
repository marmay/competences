-- | Lesson records selector (Schulübung): subscribes to the document, derives a flat row
-- per Lesson that has a date and at least one published item, and binds
-- the chosen 'LessonId' back to the parent for URL routing.
--
-- On the first projection snapshot the selector preserves the deep-link
-- selection if it resolves against the current rows; otherwise it falls
-- back to the most recent row.
module Competences.Frontend.Component.Selector.LessonRecordsSelector
  ( lessonRecordsSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), User)
import Competences.Document.Lesson (Lesson (..), LessonId, LessonItem (..), LessonPhase (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Lesson.Detailed (lessonDerivedTitle)
import Competences.Frontend.SyncContext
  ( ChangeInfo (..)
  , ProjectedChange (..)
  , SyncContext (..)
  , subscribeWithProjection
  )
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

data LessonRow = LessonRow
  { lessonId :: !LessonId
  , title :: !Text
  , date :: !Day
  }
  deriving (Eq, Generic, Show)

newtype Projection = Projection {rows :: [LessonRow]}
  deriving (Eq, Generic, Show)

data Model = Model
  { rows :: ![LessonRow]
  , selectedItem :: !(Maybe LessonId)
  , searchQuery :: !Text
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectItem !LessonId
  | SetSearchQuery !Text
  | ProjectionChanged !(ProjectedChange Projection)
  deriving (Eq, Show)

lessonRecordsSelectorComponent
  :: SyncContext
  -> Maybe LessonId
  -- ^ Deep-link selection (typically from URL). Preserved on the first
  -- projection snapshot if it resolves against current rows.
  -> Maybe (LessonId -> IO ())
  -- ^ Side effect on selection (typically URL push).
  -> Lens' p (Maybe LessonId)
  -- ^ Parent lens for the bound selection.
  -> M.Component p Model Action
lessonRecordsSelectorComponent r mDeepLink onSelect parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedItem]
    , M.subs = [subscribeWithProjection r listProjection ProjectionChanged]
    }
  where
    model = Model [] mDeepLink ""

    update (SelectItem lid) = do
      M.modify $ #selectedItem ?~ lid
      case onSelect of
        Just f -> M.io_ (f lid)
        Nothing -> pure ()

    update (SetSearchQuery q) = M.modify $ #searchQuery .~ q

    update (ProjectionChanged pc) = M.modify $ \m ->
      let rows' = pc.projection.rows
          resolvesTo lid = any (\row -> row.lessonId == lid) rows'
          validated = case m.selectedItem of
            Just lid | resolvesTo lid -> Just lid
            _ -> Nothing
          m' = m & #rows .~ rows' & #selectedItem .~ validated
       in case (pc.changeInfo, validated) of
            (InitialSnapshot, Nothing) ->
              m' & #selectedItem .~ fmap (.lessonId) (listToMaybe rows')
            _ -> m'

    view' m =
      MH.div_
        [class_ "h-full"]
        [ Layout.vFlow
            (Layout.gapS <> Layout.hFull)
            [ SL.selectorHeader (C.translate' C.LblLessonRecords) Nothing
            , SL.selectorSearchField
                (ms m.searchQuery)
                (C.translate' C.LblFilterLessonRecords)
                (SetSearchQuery . M.fromMisoString)
            , viewItems m
            ]
        ]

    viewItems m =
      let query = T.toLower m.searchQuery
          filtered =
            if T.null query
              then m.rows
              else filter (\row -> query `T.isInfixOf` T.toLower row.title) m.rows
       in SL.selectorList (map (viewRow m) filtered)

    viewRow m row =
      let isSel = Just row.lessonId == m.selectedItem
       in SL.selectorItemMultiLine
            isSel
            [ MH.span_ [class_ "text-sm font-medium truncate"] [M.text (ms row.title)]
            , MH.span_
                [class_ "text-xs text-muted-foreground"]
                [M.text (C.formatDay row.date)]
            ]
            (SelectItem row.lessonId)

listProjection :: Document -> Maybe User -> Projection
listProjection doc _mUser =
  let rows =
        [ LessonRow {lessonId = l.id, title = lessonDerivedTitle l, date = d}
        | l <- Ix.toAscList (Proxy @LessonId) doc.lessons
        , hasPublished l
        , Just d <- [l.date]
        ]
   in Projection {rows = sortOn (Down . (.date)) rows}

hasPublished :: Lesson -> Bool
hasPublished l =
  any (\p -> any (.publish) p.items) l.phases
    || any (.publish) l.supplementalItems
