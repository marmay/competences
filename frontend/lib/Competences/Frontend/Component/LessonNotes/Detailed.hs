-- | Detailed lesson-notes view: state machine, view primitives, effectful
-- update, and full Miso component.
module Competences.Frontend.Component.LessonNotes.Detailed
  ( -- * State machine
    LessonNotesDetailedState (..)
  , LessonNotesDetailedAction (..)
  , initialLessonNotesDetailedState
  , updateLessonNotesDetailedPure
    -- * Embeddable update
  , updateLessonNotesDetailed
    -- * Lesson-notes group rendering
  , renderLessonNotesGroup
    -- * View primitives
  , lessonNotesHeader
  , linkedLessonLink
  , itemsSection
    -- * Composites
  , lessonNotesCardView
  , lessonNotesDisclosureView
    -- * Full component
  , LessonNotesDetailedConfig (..)
  , LessonNotesDetailedSettings (..)
  , defaultLessonNotesDetailedSettings
  , lessonNotesDetailedComponent
  )
where

import Competences.Command (EntityCommand (..), ModifyCommand (..))
import Competences.Command qualified as Cmd
import Competences.Command.LessonNotes (LessonNotesCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Common.Set (toggle)
import Competences.Document (Document (..), Lesson (..), LessonNoteItem (..), LessonNotes (..), Task (..), User)
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.EntityMenu qualified as EM
import Competences.Frontend.Component.ResourceLookup (ResolvedItem (..))
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , PinViewerRequest (..)
  , SyncContext (..)
  , isTeacher
  , modifySyncDocument
  , requestViewerPin
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Router qualified as M
import Miso.String (MisoString, ms)
import Competences.Frontend.Common.Effect (liftEffect_)
import Optics.Core (Lens', (%), (%~), (.~))

-- ============================================================================
-- State machine
-- ============================================================================

data LessonNotesDetailedState = LessonNotesDetailedState
  { expandedLessonNotes :: !(Set LessonNotesId)
  , holdDeleteEntity :: !(HoldButton.HoldState LessonNotesId)
  , menuOpen :: !(Maybe LessonNotesId)
  }
  deriving (Eq, Generic, Show)

data LessonNotesDetailedAction
  = ToggleLessonNotes !LessonNotesId
  | MenuEdit !LessonNotesId
  | MenuPin !LessonNotes
  | MenuGoTo !LessonNotesId
  | MenuDelete !LessonNotesId
  | HoldDeleteEntity !(HoldButton.HoldAction LessonNotesId)
  | MenuToggle !LessonNotesId
  | MenuClose
  deriving (Eq, Show)

initialLessonNotesDetailedState :: [LessonNotesId] -> LessonNotesDetailedState
initialLessonNotesDetailedState expanded =
  LessonNotesDetailedState {expandedLessonNotes = Set.fromList expanded, holdDeleteEntity = HoldButton.emptyHoldState, menuOpen = Nothing}

updateLessonNotesDetailedPure
  :: LessonNotesDetailedAction
  -> LessonNotesDetailedState
  -> LessonNotesDetailedState
updateLessonNotesDetailedPure (ToggleLessonNotes lnid) =
  #expandedLessonNotes %~ toggle lnid
updateLessonNotesDetailedPure (MenuToggle lnid) = #menuOpen %~ \cur -> if cur == Just lnid then Nothing else Just lnid
updateLessonNotesDetailedPure MenuClose = #menuOpen .~ Nothing
updateLessonNotesDetailedPure _ = id

-- ============================================================================
-- Embeddable update
-- ============================================================================

-- | Embeddable update: pass a lens at the parent's 'LessonNotesDetailedState'.
updateLessonNotesDetailed
  :: Lens' model LessonNotesDetailedState
  -> SyncContext
  -> (LessonNotesDetailedAction -> action)
  -> LessonNotesDetailedAction
  -> M.Effect parent model action
updateLessonNotesDetailed stateLens r lift = go
  where
    go (MenuEdit lnid) = do
      dismiss
      M.io_ $ modifySyncDocument r $ Cmd.LessonNotes (OnLessonNotes (Modify lnid Lock))
    go (MenuPin ln) = do
      dismiss
      M.io_ $ requestViewerPin r (PinLessonNotesViewer ln)
    go (MenuGoTo lnid) = do
      dismiss
      M.io_ $ M.pushURI (M.toURI (ManageLessonNotes (Just lnid)))
    go (MenuDelete lnid) = do
      dismiss
      M.io_ $ modifySyncDocument r $ Cmd.LessonNotes (OnLessonNotes (Delete lnid))
    go (HoldDeleteEntity ha) =
      liftEffect_ (stateLens % #holdDeleteEntity) (lift . HoldDeleteEntity) $
        HoldButton.updateHold (\lnid -> modifySyncDocument r $ Cmd.LessonNotes (OnLessonNotes (Delete lnid))) ha
    go action = M.modify (stateLens %~ updateLessonNotesDetailedPure action)

    dismiss = M.modify (stateLens % #menuOpen .~ Nothing)

-- ============================================================================
-- Lesson-notes group rendering
-- ============================================================================

-- | Render a lesson-notes group as a collapsible disclosure.
-- Body is caller-supplied (e.g. items with relevance annotations).
renderLessonNotesGroup
  :: LessonNotesDetailedState
  -> [M.View m a]
  -> M.View m a
  -> (LessonNotesDetailedAction -> a)
  -> LessonNotes
  -> M.View m a
renderLessonNotesGroup state annotations body liftAction ln =
  lessonNotesDisclosureView
    (liftAction (ToggleLessonNotes ln.id))
    (ms ln.title)
    annotations
    (not $ Set.member ln.id state.expandedLessonNotes)
    body

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

-- ============================================================================
-- Full component
-- ============================================================================

data LessonNotesDetailedConfig = LessonNotesDetailedConfig
  { lessonNotesId :: !LessonNotesId
  , settings :: !LessonNotesDetailedSettings
  }

data LessonNotesDetailedSettings = LessonNotesDetailedSettings
  { startExpanded :: !Bool
  , showAnnotations :: !Bool
  , enableGoTo :: !Bool
  , enableDelete :: !Bool
  }
  deriving (Eq, Show)

defaultLessonNotesDetailedSettings :: LessonNotesDetailedSettings
defaultLessonNotesDetailedSettings =
  LessonNotesDetailedSettings
    { startExpanded = True
    , showAnnotations = True
    , enableGoTo = True
    , enableDelete = False
    }

newtype LessonNotesProjection = LessonNotesProjection
  { lessonNotes :: Maybe (LessonNotes, Maybe Lesson, [ResolvedItem])
  }
  deriving (Eq, Generic, Show)

data ComponentModel = ComponentModel
  { projection :: !LessonNotesProjection
  , viewState :: !LessonNotesDetailedState
  }
  deriving (Eq, Generic, Show)

data ComponentAction
  = ProjectionChanged !(ProjectedChange LessonNotesProjection)
  | ViewAction !LessonNotesDetailedAction
  deriving (Eq, Show)

-- | Build the lesson-notes detailed component.
--
-- Takes a renderer for resolved items (tasks\/resources) to avoid circular
-- module dependencies -- the caller wires in the concrete renderers from
-- @Task.Detailed@ and @Resource.Detailed@.
lessonNotesDetailedComponent
  :: (SyncContext -> ResolvedItem -> M.View ComponentModel ComponentAction)
  -> SyncContext
  -> LessonNotesDetailedConfig
  -> M.Component p ComponentModel ComponentAction
lessonNotesDetailedComponent renderItem r cfg =
  (M.component model update' view')
    { M.subs = [subscribeWithProjection r (lessonNotesProjection cfg) ProjectionChanged]
    }
  where
    model = ComponentModel
      { projection = LessonNotesProjection Nothing
      , viewState = initialLessonNotesDetailedState
          [cfg.lessonNotesId | cfg.settings.startExpanded]
      }

    update' (ProjectionChanged change) = M.modify $ #projection .~ change.projection
    update' (ViewAction a) = updateLessonNotesDetailed #viewState r ViewAction a

    view' m = case m.projection.lessonNotes of
      Nothing -> Layout.empty
      Just (ln, mLesson, items) ->
        lessonNotesCardView $
          [lessonNotesHeader (ms ln.title) ln.date (annotations m ln)]
            <> [linkedLessonLink (ms lesson.title) | Just lesson <- [mLesson]]
            <> [itemsSection (map (renderItem r) items)]

    annotations _m ln
      | cfg.settings.showAnnotations, isTeacher r =
          [ inlineComponent ("entity-menu-" <> ms (show ln.id))
              (EM.entityMenuComponent r EM.EntityMenuConfig
                { edit = Just (EM.lessonNotesEdit ln.id)
                , pin = Just (PinLessonNotesViewer ln)
                , goTo = if cfg.settings.enableGoTo then Just (ManageLessonNotes (Just ln.id)) else Nothing
                , delete = if cfg.settings.enableDelete then Just (EM.lessonNotesDelete ln.id) else Nothing
                , extraEntries = []
                })
          ]
      | otherwise = []

lessonNotesProjection :: LessonNotesDetailedConfig -> Document -> Maybe User -> LessonNotesProjection
lessonNotesProjection cfg doc _mUser =
  LessonNotesProjection $ do
    ln <- Ix.getOne (doc.lessonNotes Ix.@= cfg.lessonNotesId)
    let mLesson = ln.lessonId >>= \lid -> Ix.getOne (doc.lessons Ix.@= lid)
        items = mapMaybe (resolveItem doc) ln.items
    pure (ln, mLesson, items)

resolveItem :: Document -> LessonNoteItem -> Maybe ResolvedItem
resolveItem doc (LessonResource rid) = ResolvedResource <$> Ix.getOne (doc.resources Ix.@= rid)
resolveItem doc (LessonTask tid) = do
  task <- Ix.getOne (doc.tasks Ix.@= tid)
  let sols = Ix.toList (doc.solutions Ix.@= tid)
  pure $ ResolvedTask $ TaskWithSolutions task task.content task.purpose sols
