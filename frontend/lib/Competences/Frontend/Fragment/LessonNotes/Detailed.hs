-- | Detailed lesson-notes view: pure view primitives and pure state machine.
--
-- Effects for the state machine (item resolution, sub-component mounting)
-- live in 'Component.LessonNotes.Detailed.Embed'.
module Competences.Frontend.Fragment.LessonNotes.Detailed
  ( -- * State machine
    LessonNotesDetailedState (..)
  , LessonNotesDetailedAction (..)
  , initialLessonNotesDetailedState
  , updateLessonNotesDetailedPure
    -- * View primitives
  , lessonNotesHeader
  , linkedLessonLink
  , itemsSection
    -- * Composites
  , lessonNotesCardView
  , lessonNotesDisclosureView
  )
where

import Competences.Common.Set (toggle)
import Competences.Document (LessonNotes)
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)
import Optics.Core ((%~), (.~))

-- ============================================================================
-- State machine
-- ============================================================================

data LessonNotesDetailedState = LessonNotesDetailedState
  { expandedLessonNotes :: !(Set LessonNotesId)
  , holdDeleteEntity :: !(HoldButton.HoldState LessonNotesId)
  , menuOpen :: !Bool
  }
  deriving (Eq, Generic, Show)

data LessonNotesDetailedAction
  = ToggleLessonNotes !LessonNotesId
  | MenuEdit !LessonNotesId
  | MenuPin !LessonNotes
  | MenuGoTo !LessonNotesId
  | MenuDelete !LessonNotesId
  | HoldDeleteEntity !(HoldButton.HoldAction LessonNotesId)
  | MenuToggle
  | MenuClose
  deriving (Eq, Show)

initialLessonNotesDetailedState :: [LessonNotesId] -> LessonNotesDetailedState
initialLessonNotesDetailedState expanded =
  LessonNotesDetailedState {expandedLessonNotes = Set.fromList expanded, holdDeleteEntity = HoldButton.emptyHoldState, menuOpen = False}

updateLessonNotesDetailedPure
  :: LessonNotesDetailedAction
  -> LessonNotesDetailedState
  -> LessonNotesDetailedState
updateLessonNotesDetailedPure (ToggleLessonNotes lnid) =
  #expandedLessonNotes %~ toggle lnid
updateLessonNotesDetailedPure MenuToggle = #menuOpen %~ not
updateLessonNotesDetailedPure MenuClose = #menuOpen .~ False
updateLessonNotesDetailedPure _ = id


-- ============================================================================
-- Header
-- ============================================================================

-- | Title + date block with annotation slot.
lessonNotesHeader
  :: MisoString
  -- ^ Title
  -> Day
  -- ^ Date
  -> [M.View m a]
  -- ^ Annotations (right side of title row)
  -> M.View m a
lessonNotesHeader title date annotations =
  MH.div_
    [class_ "space-y-1"]
    [ Layout.hFlow
        (Layout.hFull <> Layout.crossCenter)
        ( [ Typography.h2 title
          , Layout.flowSpring
          ]
            <> annotations
        )
    , MH.span_
        [class_ "text-sm text-muted-foreground"]
        [M.text $ C.formatDay date]
    ]

-- | Small "Lesson: <title>" chip shown when a LessonNotes is linked to a lesson.
linkedLessonLink :: MisoString -> M.View m a
linkedLessonLink lessonTitle =
  MH.div_
    [class_ "text-sm"]
    [ Layout.hFlow
        Layout.gapS
        [ MH.span_
            [class_ "text-muted-foreground"]
            [M.text $ C.translate' C.LblLesson <> ":"]
        , MH.span_ [] [M.text lessonTitle]
        ]
    ]

-- | Item list frame; empty list renders as empty.
itemsSection :: [M.View m a] -> M.View m a
itemsSection [] = M.text ""
itemsSection items = MH.div_ [class_ "space-y-2"] items

-- ============================================================================
-- Composites
-- ============================================================================

-- | Always-expanded card. For page detail / modal body.
-- Body is rendered directly inside the card (no extra wrapper).
lessonNotesCardView :: [M.View m a] -> M.View m a
lessonNotesCardView = Card.card

-- | Collapsible group frame.
lessonNotesDisclosureView
  :: a
  -> MisoString
  -> [M.View m a]
  -> Bool
  -> M.View m a
  -> M.View m a
lessonNotesDisclosureView toggleAction title annotations isExpanded body =
  Disclosure.innerDisclosure toggleAction $
    Disclosure.contents (mkTitle title annotations) isExpanded body []

mkTitle :: MisoString -> [M.View m a] -> M.View m a
mkTitle title [] = Disclosure.titleIconText Icon.IcnLessonNotes title
mkTitle title annotations =
  Disclosure.titleWithAnnotation
    (Disclosure.titleIconText Icon.IcnLessonNotes title)
    (Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter) annotations)
