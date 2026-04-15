module Competences.Frontend.Component.LessonNotes.ViewerDetail
  ( viewerDetailView
  , viewerComponent
  , pinLessonNotesViewer
  , openLessonNotesModal
    -- * Shared renderers (reused by ResourceLookup.View)
  , viewResourceCard
  , viewLinkCard
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Lesson (..)
  , LessonNoteItem (..)
  , LessonNotes (..)
  , Resource (..)
  , FileRef (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  )
import Competences.Document.Id (idToText)
import Competences.Document.Task (Task (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.FileUpload (showFileSize)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
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
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
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
viewResolvedItem r (ResolvedResource res) = viewResourceCard r res
viewResolvedItem r (ResolvedTask tws) =
  inlineComponent
    ("lesson-notes-task-" <> M.ms (show tws.task.id))
    (TaskComp.taskDetailedComponent r (TaskComp.TaskDetailedConfig tws.task.id Published TaskComp.defaultTaskDetailedSettings))

-- ============================================================================
-- Shared renderers
-- ============================================================================

-- | Render a resource card always-expanded (no disclosure state).
--
-- Used by views that show resources inline without collapse/expand controls
-- (e.g. lesson notes viewer detail, resource lookup view).
viewResourceCard :: SyncContext -> Resource -> M.View model action
viewResourceCard r res =
  let ResourceIdentifier ident = res.identifier
      displayName = if T.null ident then "(Unbenannt)" else ident
      fc = r.formulaCache
   in case res.content of
        InlineContent rc ->
          Card.contentCard Icon.IcnResources (M.ms displayName)
            [ if rc /= mempty
                then
                  MH.div_
                    [class_ "px-3 pb-3 prose prose-stone prose-sm max-w-none"]
                    [renderRichTextWithFiles fc r res.attachments rc]
                else Layout.empty
            ]
        WebLink url title -> viewLinkCard Icon.IcnLink ident displayName url title
        VideoLink url title -> viewLinkCard Icon.IcnVideo ident displayName url title
        FileContent fileRef ->
          Card.contentCard Icon.IcnResources (M.ms displayName)
            [ MH.div_ [class_ "px-3 pb-3 text-sm text-muted-foreground"]
                [M.text $ M.ms $ fileRef.fileName <> " (" <> showFileSize fileRef.fileSize <> ")"]
            ]

-- | Render a link card (web or video) with icon, name, and optional title.
viewLinkCard :: Icon.Icon -> T.Text -> T.Text -> T.Text -> T.Text -> M.View model action
viewLinkCard icon ident displayName url title =
  MH.a_
    [ class_ "flex items-center gap-2 px-4 py-3 rounded-lg hover:bg-muted/50 transition-colors"
    , MP.href_ (M.ms url)
    , MP.target_ "_blank"
    , MP.rel_ "noopener noreferrer"
    ]
    [ Icon.icon [class_ "text-sky-600"] icon
    , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
    , if T.null title || title == ident
        then Layout.empty
        else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
    ]
