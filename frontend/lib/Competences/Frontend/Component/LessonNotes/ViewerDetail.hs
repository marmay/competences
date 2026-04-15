module Competences.Frontend.Component.LessonNotes.ViewerDetail
  ( viewerDetailView
  , viewerComponent
  , pinLessonNotesViewer
  , openLessonNotesModal
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Lesson (..)
  , LessonNoteItem (..)
  , LessonNotes (..)
  , Resource (..)
  )
import Competences.Document.Id (idToText)
import Competences.Document.Task (Task (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Resource.Detailed qualified as ResComp
import Competences.Frontend.Component.ResourceLookup (ResolvedItem (..))
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.Task.Detailed qualified as TaskComp
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalId (..), ModalHeight (..), ModalWidth (..), PinCategory (..), PinMeta (..), SortAtom (..), SortKey (..), WindowChrome (..), WindowMode, inlineComponent, inlineComponentWith, isPinned, openFramedModalWith, pinDialogWith)
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.WindowFrame (pinButton)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- Projection
-- ============================================================================

data ViewerProjection = ViewerProjection
  { currentLessonNotes :: !LessonNotes
  , resolvedItems :: ![ResolvedItem]
  , linkedLesson :: !(Maybe Lesson)
  }
  deriving (Eq, Generic, Show)

viewerProjection :: LessonNotes -> Document -> Maybe user -> ViewerProjection
viewerProjection ln doc _ =
  ViewerProjection
    { currentLessonNotes = maybe ln id $ Ix.getOne (doc.lessonNotes Ix.@= ln.id)
    , resolvedItems = mapMaybe (resolveItem doc) ln.items
    , linkedLesson = ln.lessonId >>= \lid -> Ix.getOne (doc.lessons Ix.@= lid)
    }

resolveItem :: Document -> LessonNoteItem -> Maybe ResolvedItem
resolveItem doc (LessonResource rid) = ResolvedResource <$> Ix.getOne (doc.resources Ix.@= rid)
resolveItem doc (LessonTask tid) = do
  task <- Ix.getOne (doc.tasks Ix.@= tid)
  let sols = Ix.toList (doc.solutions Ix.@= tid)
  Just $ ResolvedTask $ TaskWithSolutions task task.content task.purpose sols

-- ============================================================================
-- Model & Action
-- ============================================================================

data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  | PinThis
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

-- | Open the lesson notes viewer as a pinnable framed modal.
openLessonNotesModal :: SyncContext -> LessonNotes -> IO ()
openLessonNotesModal r ln =
  openFramedModalWith
    r.windowManager
    ( ModalConfig
        { chrome = WindowChrome (M.ms ln.title) Icon.IcnLessonNotes Nothing
        , modalId = ModalId ("lesson-notes-" <> idToText ln.id)
        , width = ModalWide
        , height = ModalFull
        , pinnable = Just ()
        }
    )
    (viewerComponent r ln)

-- | Pin the lesson notes viewer as a persistent dialog.
pinLessonNotesViewer :: SyncContext -> LessonNotes -> IO ()
pinLessonNotesViewer r ln =
  let chrome = WindowChrome (M.ms ln.title) Icon.IcnLessonNotes Nothing
      meta = PinMeta
        { key = "lesson-notes-" <> idToText ln.id
        , category = PinCatLessonNotes
        , sortKey = SortKey [SortAtom ln.date, SortAtom ln.title, SortAtom ln.id]
        , context = Just (C.formatDayShort ln.date)
        }
   in pinDialogWith r.windowManager
        meta
        chrome
        (\mode (_savedState :: Maybe ()) -> viewerComponent r ln mode)

viewerDetailView
  :: SyncContext
  -> LessonNotes
  -> M.View (SD.Model LessonNotes mode) (SD.Action mode)
viewerDetailView r ln =
  inlineComponentWith
    ("lesson-notes-viewer-" <> M.ms (show ln.id))
    (viewerComponent r ln)

viewerComponent :: SyncContext -> LessonNotes -> WindowMode -> M.Component p ViewerModel ViewerAction
viewerComponent r ln wm =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (viewerProjection ln) ProjectionChanged]
    }
  where
    model = ViewerModel
      { projection = ViewerProjection ln [] Nothing
      }

    update (ProjectionChanged change) =
      M.modify $ \m -> m & #projection .~ change.projection

    update PinThis = M.io_ $ pinLessonNotesViewer r ln

    view' m =
      let proj = m.projection
          ln' = proj.currentLessonNotes
       in Card.card
            [ MH.div_
                [class_ "space-y-4"]
                [ -- Header: title + pin button + date
                  MH.div_
                    [class_ "space-y-1"]
                    [ Layout.hFlow (Layout.hFull <> Layout.crossCenter) $
                        [ Typography.h2 (M.ms ln'.title)
                        , Layout.flowSpring
                        ]
                        <> [ pinButton PinThis | not (isPinned wm) ]
                    , MH.span_ [class_ "text-sm text-muted-foreground"] [M.text $ C.formatDay ln'.date]
                    ]
                , -- Linked lesson
                  case proj.linkedLesson of
                    Nothing -> M.text ""
                    Just lesson ->
                      MH.div_
                        [class_ "text-sm"]
                        [ Layout.hFlow Layout.gapS
                            [ MH.span_ [class_ "text-muted-foreground"] [M.text $ C.translate' C.LblLesson <> ":"]
                            , MH.span_ [] [M.text $ M.ms lesson.title]
                            ]
                        ]
                , -- Items (resources and tasks)
                  if null proj.resolvedItems
                    then M.text ""
                    else MH.div_ [class_ "space-y-2"] (map (viewResolvedItem r) proj.resolvedItems)
                ]
            ]

-- | Render a resolved item in the viewer
viewResolvedItem :: SyncContext -> ResolvedItem -> M.View model action
viewResolvedItem r (ResolvedResource res) =
  inlineComponent
    ("lesson-notes-resource-" <> M.ms (show res.id))
    (ResComp.resourceDetailedComponent r (ResComp.ResourceDetailedConfig res.id ResComp.defaultResourceDetailedSettings))
viewResolvedItem r (ResolvedTask tws) =
  inlineComponent
    ("lesson-notes-task-" <> M.ms (show tws.task.id))
    (TaskComp.taskDetailedComponent r (TaskComp.TaskDetailedConfig tws.task.id Published TaskComp.defaultTaskDetailedSettings))
