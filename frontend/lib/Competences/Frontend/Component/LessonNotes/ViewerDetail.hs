module Competences.Frontend.Component.LessonNotes.ViewerDetail
  ( viewerDetailView
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Lesson (..)
  , LessonNoteItem (..)
  , LessonNotes (..)
  , Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  , Task (..)
  )
import Competences.Document.Task (TaskIdentifier (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~))

-- ============================================================================
-- Resolved items
-- ============================================================================

data ResolvedItem
  = ResolvedResource !Resource
  | ResolvedTask !Task
  deriving (Eq, Show)

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
resolveItem doc (LessonTask tid) = ResolvedTask <$> Ix.getOne (doc.tasks Ix.@= tid)

-- ============================================================================
-- Model & Action
-- ============================================================================

data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

viewerDetailView
  :: SyncContext
  -> LessonNotes
  -> M.View (SD.Model LessonNotes mode) (SD.Action mode)
viewerDetailView r ln =
  component
    ("lesson-notes-viewer-" <> M.ms (show ln.id))
    (viewerComponent r ln)

viewerComponent :: SyncContext -> LessonNotes -> M.Component p ViewerModel ViewerAction
viewerComponent r ln =
  (M.component model update view')
    { M.subs = [subscribeWithProjection r (viewerProjection ln) ProjectionChanged]
    }
  where
    model = ViewerModel
      { projection = ViewerProjection ln [] Nothing
      }

    update (ProjectionChanged change) =
      M.modify $ \m -> m & #projection .~ change.projection

    view' m =
      let proj = m.projection
          ln' = proj.currentLessonNotes
       in Card.card
            [ MH.div_
                [class_ "space-y-4"]
                [ -- Header: title + date
                  MH.div_
                    [class_ "space-y-1"]
                    [ Typography.h2 (M.ms ln'.title)
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
                    else MH.div_ [class_ "space-y-2"] (map viewResolvedItem proj.resolvedItems)
                ]
            ]

-- | Render a resolved item in the viewer
viewResolvedItem :: ResolvedItem -> M.View model action
viewResolvedItem (ResolvedResource res) = viewResourceCard res
viewResolvedItem (ResolvedTask task) = viewTaskCard task

-- | Render a resource card (same pattern as ResourceList.resourcesExpandedListView)
viewResourceCard :: Resource -> M.View model action
viewResourceCard res =
  let ResourceIdentifier ident = res.identifier
      displayName = if T.null ident then "(Unbenannt)" else ident
      nameView =
        Layout.hFlow
          (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
          [ Icon.icon [class_ "text-sky-600"] Icon.IcnResources
          , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
          ]
   in case res.content of
        InlineContent rc ->
          MH.div_
            [class_ "border rounded-lg overflow-hidden"]
            [ MH.div_
                [class_ "px-3 py-2"]
                [nameView]
            , if rc /= mempty
                then
                  MH.div_
                    [class_ "px-3 pb-3 prose prose-stone prose-sm max-w-none"]
                    [renderRichText rc]
                else Layout.empty
            ]
        WebLink url title ->
          MH.a_
            [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
            , MP.href_ (M.ms url)
            , MP.target_ "_blank"
            , MP.rel_ "noopener noreferrer"
            ]
            [ Icon.icon [class_ "text-sky-600"] Icon.IcnLink
            , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
            , if T.null title || title == ident
                then Layout.empty
                else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
            ]
        VideoLink url title ->
          MH.a_
            [ class_ "flex items-center gap-2 px-4 py-3 border rounded-lg hover:bg-muted/50 transition-colors"
            , MP.href_ (M.ms url)
            , MP.target_ "_blank"
            , MP.rel_ "noopener noreferrer"
            ]
            [ Icon.icon [class_ "text-sky-600"] Icon.IcnVideo
            , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
            , if T.null title || title == ident
                then Layout.empty
                else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (M.ms $ "— " <> title)]
            ]

-- | Render a task card (simplified inline view)
viewTaskCard :: Task -> M.View model action
viewTaskCard task =
  let TaskIdentifier ident = task.identifier
      displayName = if T.null ident then "(Unbenannt)" else ident
   in MH.div_
        [class_ "flex items-center gap-2 px-4 py-3 border rounded-lg"]
        [ Icon.icon [class_ "text-sky-600"] Icon.IcnTask
        , MH.span_ [class_ "font-medium"] [M.text (M.ms displayName)]
        ]
