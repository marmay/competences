module Competences.Frontend.Component.LessonNotes.ViewerDetail
  ( viewerDetailView
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Document (..)
  , Lesson (..)
  , LessonNotes (..)
  , Resource (..)
  )
import Competences.Document.Resource (ResourceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , subscribeWithProjection
  )
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.ResourceList qualified as ResourceList
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- Projection
-- ============================================================================

data ViewerProjection = ViewerProjection
  { currentLessonNotes :: !LessonNotes
  , resolvedResources :: ![Resource]
  , linkedLesson :: !(Maybe Lesson)
  }
  deriving (Eq, Generic, Show)

viewerProjection :: LessonNotes -> Document -> Maybe user -> ViewerProjection
viewerProjection ln doc _ =
  ViewerProjection
    { currentLessonNotes = maybe ln id $ Ix.getOne (doc.lessonNotes Ix.@= ln.id)
    , resolvedResources = mapMaybe (\rId -> Ix.getOne (doc.resources Ix.@= rId)) ln.resources
    , linkedLesson = ln.lessonId >>= \lid -> Ix.getOne (doc.lessons Ix.@= lid)
    }

-- ============================================================================
-- Model & Action
-- ============================================================================

data ViewerModel = ViewerModel
  { projection :: !ViewerProjection
  , expandedResources :: !(Set.Set ResourceId)
  }
  deriving (Eq, Generic, Show)

data ViewerAction
  = ProjectionChanged !(ProjectedChange ViewerProjection)
  | ToggleResourceExpanded !ResourceId
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
      , expandedResources = Set.empty
      }

    update (ProjectionChanged change) =
      M.modify $ \m -> m & #projection .~ change.projection

    update (ToggleResourceExpanded resId) =
      M.modify $ \m ->
        let newExpanded =
              if Set.member resId m.expandedResources
                then Set.delete resId m.expandedResources
                else Set.insert resId m.expandedResources
         in m & #expandedResources .~ newExpanded

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
                , -- Resources
                  if null proj.resolvedResources
                    then M.text ""
                    else ResourceList.resourcesListView proj.resolvedResources m.expandedResources ToggleResourceExpanded
                ]
            ]
