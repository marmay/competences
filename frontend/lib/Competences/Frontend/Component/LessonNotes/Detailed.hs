-- | Full Miso component wrapping the lesson-notes Fragment.
-- Parents embedding inline should use 'LessonNotes.Detailed.Embed'.
module Competences.Frontend.Component.LessonNotes.Detailed
  ( LessonNotesDetailedConfig (..)
  , LessonNotesDetailedSettings (..)
  , defaultLessonNotesDetailedSettings
  , lessonNotesDetailedComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lesson (..), LessonNoteItem (..), LessonNotes (..), Resource (..), Task (..), User)
import Competences.Document.LessonNotes (LessonNotesId)
import Competences.Frontend.Component.Draft (EntityOrigin (..))
import Competences.Frontend.Component.LessonNotes.Detailed.Embed (updateLessonNotesDetailed)
import Competences.Frontend.Component.Resource.Detailed qualified as ResComp
import Competences.Frontend.Component.ResourceLookup (ResolvedItem (..))
import Competences.Frontend.Component.Task.Detailed qualified as TaskComp
import Competences.Frontend.Fragment.LessonNotes.Detailed qualified as V
import Competences.Frontend.Fragment.Task.Projection (TaskWithSolutions (..))
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext (..)
  , isTeacher
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.EntityMenu (entityMenu, menuEdit, menuPin, menuGoTo, menuDelete)
import Competences.Frontend.View.Layout qualified as Layout
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core ((.~))

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

data Model = Model
  { projection :: !LessonNotesProjection
  , viewState :: !V.LessonNotesDetailedState
  }
  deriving (Eq, Generic, Show)

data Action
  = ProjectionChanged !(ProjectedChange LessonNotesProjection)
  | ViewAction !V.LessonNotesDetailedAction
  deriving (Eq, Show)

lessonNotesDetailedComponent :: SyncContext -> LessonNotesDetailedConfig -> M.Component p Model Action
lessonNotesDetailedComponent r cfg =
  (M.component model update' view')
    { M.subs = [subscribeWithProjection r (lessonNotesProjection cfg) ProjectionChanged]
    }
  where
    model = Model
      { projection = LessonNotesProjection Nothing
      , viewState = V.initialLessonNotesDetailedState
          [cfg.lessonNotesId | cfg.settings.startExpanded]
      }

    update' (ProjectionChanged change) = M.modify $ #projection .~ change.projection
    update' (ViewAction a) = updateLessonNotesDetailed #viewState r ViewAction a

    view' m = case m.projection.lessonNotes of
      Nothing -> Layout.empty
      Just (ln, mLesson, items) ->
        V.lessonNotesCardView $
          [V.lessonNotesHeader (ms ln.title) ln.date (annotations ln)]
            <> [V.linkedLessonLink (ms lesson.title) | Just lesson <- [mLesson]]
            <> [V.itemsSection (map (viewResolvedItem r) items)]

    annotations ln
      | cfg.settings.showAnnotations, isTeacher r =
          [entityMenu $
            [ menuEdit (ViewAction (V.MenuEdit ln.id))
            , menuPin (ViewAction (V.MenuPin ln))
            ]
            ++ [menuGoTo (ViewAction (V.MenuGoTo ln.id)) | cfg.settings.enableGoTo]
            ++ [menuDelete (ViewAction (V.MenuDelete ln.id)) | cfg.settings.enableDelete]
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

viewResolvedItem :: SyncContext -> ResolvedItem -> M.View m a
viewResolvedItem r (ResolvedResource res) =
  inlineComponent
    ("ln-resource-" <> ms (show res.id))
    (ResComp.resourceDetailedComponent r (ResComp.ResourceDetailedConfig res.id ResComp.defaultResourceDetailedSettings))
viewResolvedItem r (ResolvedTask tws) =
  inlineComponent
    ("ln-task-" <> ms (show tws.task.id))
    (TaskComp.taskDetailedComponent r (TaskComp.TaskDetailedConfig tws.task.id Published TaskComp.defaultTaskDetailedSettings))
