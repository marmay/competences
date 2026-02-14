module Competences.Frontend.Component.Selector.MultiSelectLessonNotesSelector
  ( multiSelectLessonNotesSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), LessonNotes (..))
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , mkSelectorBinding
  )
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Combobox qualified as Combobox
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortOn)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- Projection
-- ============================================================================

data SelectorProjection = SelectorProjection
  { allLessonNotes :: ![LessonNotes]
  , eligibleLessonNotes :: ![LessonNotes]
  }
  deriving (Eq, Generic, Show)

selectorProjection :: (LessonNotes -> Bool) -> Document -> Maybe user -> SelectorProjection
selectorProjection eligible doc _ =
  let allSorted = sortOn (.date) $ Ix.toDescList (Proxy @Day) doc.lessonNotes
   in SelectorProjection
        { allLessonNotes = allSorted
        , eligibleLessonNotes = filter eligible allSorted
        }

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { projection :: !SelectorProjection
  , selectedResults :: ![LessonNotesId]
  , searchQuery :: !Text
  , isOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = ProjectionChanged !(ProjectedChange SelectorProjection)
  | SetSearchQuery !Text
  | ToggleLessonNotes !LessonNotesId
  | SetOpen !Bool
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

multiSelectLessonNotesSelectorComponent
  :: SyncContext
  -> (LessonNotes -> Bool)
  -> [LessonNotesId]
  -> SelectorTransformedLens p [] LessonNotesId f' a'
  -> M.Component p Model Action
multiSelectLessonNotesSelectorComponent r eligible initResults lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding #selectedResults]
    , M.subs = [subscribeWithProjection r (selectorProjection eligible) ProjectionChanged]
    }
  where
    model =
      Model
        { projection = SelectorProjection [] []
        , selectedResults = initResults
        , searchQuery = ""
        , isOpen = False
        }

    update (ProjectionChanged change) =
      M.modify $ \m ->
        m & #projection .~ change.projection

    update (SetSearchQuery q) =
      M.modify $ #searchQuery .~ q

    update (ToggleLessonNotes lnId) =
      M.modify $ \m ->
        let current = m.selectedResults
            new =
              if lnId `elem` current
                then filter (/= lnId) current
                else current <> [lnId]
         in m & #selectedResults .~ new

    update (SetOpen open) =
      M.modify $ #isOpen .~ open

    view m =
      MH.div_
        [class_ "space-y-2"]
        [ let filtered = filterLessonNotes m.searchQuery m.projection.eligibleLessonNotes
              options =
                [ Combobox.ComboboxOption ln.id (formatLessonNotes ln)
                | ln <- filtered
                ]
              selectedSet = Set.fromList m.selectedResults
           in Combobox.multiSelectCombobox SetSearchQuery ToggleLessonNotes SetOpen
                & Combobox.withPlaceholder (M.fromMisoString $ C.translate' C.LblSelectLessonNotes)
                & Combobox.withOptions options
                & Combobox.withSelected selectedSet
                & Combobox.withSearchQuery m.searchQuery
                & Combobox.withIsOpen m.isOpen
                & Combobox.renderCombobox
        , if null m.selectedResults
            then M.text ""
            else
              Layout.hFlow
                (Layout.gapS <> Layout.flexWrap)
                [ viewLessonNotesTag ln
                | lnId <- m.selectedResults
                , Just ln <- [lookupLessonNotes lnId m.projection.allLessonNotes]
                ]
        ]

    filterLessonNotes query notes =
      let q = T.toLower query
       in if T.null q
            then notes
            else filter (\ln -> q `T.isInfixOf` T.toLower (formatLessonNotes ln)) notes

    formatLessonNotes ln =
      let title = if T.null ln.title then "(Ohne Titel)" else ln.title
       in title <> " (" <> T.pack (show $ C.formatDay ln.date) <> ")"

    lookupLessonNotes lnId notes =
      case filter (\ln -> ln.id == lnId) notes of
        (ln : _) -> Just ln
        [] -> Nothing

    viewLessonNotesTag :: LessonNotes -> M.View Model Action
    viewLessonNotesTag ln =
      let title = if T.null ln.title then "(Ohne Titel)" else ln.title
       in Badge.interactive
            Badge.Secondary
            (Just (Icon.IcnCancel, ToggleLessonNotes ln.id))
            (Badge.badgeIconText Icon.IcnMesoPlan (M.ms title))
