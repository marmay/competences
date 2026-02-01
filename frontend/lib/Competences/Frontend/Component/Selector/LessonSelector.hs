module Competences.Frontend.Component.Selector.LessonSelector
  ( lessonEditorField
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Lesson (..))
import Competences.Document.Id (idToText)
import Competences.Document.Lesson (LessonId)
import Competences.Frontend.Common.Translate qualified as C
import Competences.Frontend.Component.Editor.EditorField (EditorField, selectorEditorFieldWithViewer)
import Competences.Frontend.Component.Selector.Common
  ( EntityPatchTransformedLens (..)
  , SelectorTransformedLens (..)
  , mkSelectorBinding
  )
import Competences.Frontend.SyncContext (DocumentChange (..), SyncContext, isInitialUpdate, subscribeDocument)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Default (Default)
import Data.List (find, sortOn)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~), (^.))
import Optics.Core qualified as O

-- ============================================================================
-- Editor field
-- ============================================================================

-- | Editor field for optionally selecting a lesson in the Assignment editor.
-- Uses a viewer (read-only lesson title) and editor (select dropdown).
lessonEditorField
  :: (Default patch, Ord entity)
  => SyncContext
  -> M.MisoString
  -> EntityPatchTransformedLens entity patch Maybe LessonId Maybe LessonId
  -> EditorField entity patch f'
lessonEditorField r k eptl =
  let config e = e ^. eptl.viewLens
   in selectorEditorFieldWithViewer
        k
        (lessonIdToLessonLens eptl)
        (\e -> lessonViewerComponent r (config e))
        (\e -> lessonEditorComponent r (config e))

-- | Transform a LessonId lens to a Lesson lens for use with selector binding.
-- The selector components work with @Maybe Lesson@ internally while the entity
-- stores @Maybe LessonId@.
lessonIdToLessonLens
  :: EntityPatchTransformedLens entity patch Maybe LessonId Maybe LessonId
  -> EntityPatchTransformedLens entity patch Maybe Lesson Maybe LessonId
lessonIdToLessonLens eptl =
  EntityPatchTransformedLens
    { viewLens = eptl.viewLens
    , patchLens = eptl.patchLens
    , transform = (.id)
    , embed = id
    }

-- ============================================================================
-- Viewer component (read-only display of selected lesson)
-- ============================================================================

data ViewerModel = ViewerModel
  { allLessons :: ![Lesson]
  , selectedLesson :: !(Maybe Lesson)
  }
  deriving (Eq, Generic, Show)

newtype ViewerAction = ViewerDocChange DocumentChange
  deriving (Eq, Show)

lessonViewerComponent
  :: SyncContext
  -> Maybe LessonId
  -> SelectorTransformedLens p Maybe Lesson Maybe LessonId
  -> M.Component p ViewerModel ViewerAction
lessonViewerComponent r initialLessonId lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding (O.castOptic #selectedLesson)]
    , M.subs = [subscribeDocument r ViewerDocChange]
    }
  where
    model = ViewerModel {allLessons = [], selectedLesson = Nothing}

    update (ViewerDocChange (DocumentChange d info)) =
      M.modify $ \m ->
        let lessons = listAllLessons d
            selected =
              if isInitialUpdate info
                then initialLessonId >>= \lid -> find (\l -> l.id == lid) lessons
                else m.selectedLesson >>= \sel -> find (\l -> l.id == sel.id) lessons
         in m & #allLessons .~ lessons & #selectedLesson .~ selected

    view m = case m.selectedLesson of
      Nothing -> Typography.muted (C.translate' C.LblNoLesson)
      Just l -> MH.span_ [] [M.text $ M.ms l.title]

-- ============================================================================
-- Editor component (select dropdown)
-- ============================================================================

data EditorModel = EditorModel
  { allLessons :: ![Lesson]
  , selectedLesson :: !(Maybe Lesson)
  }
  deriving (Eq, Generic, Show)

data EditorAction
  = EditorDocChange !DocumentChange
  | SelectLesson !(Maybe Lesson)
  deriving (Eq, Show)

lessonEditorComponent
  :: SyncContext
  -> Maybe LessonId
  -> SelectorTransformedLens p Maybe Lesson Maybe LessonId
  -> M.Component p EditorModel EditorAction
lessonEditorComponent r initialLessonId lensBinding =
  (M.component model update view)
    { M.bindings = [mkSelectorBinding lensBinding (O.castOptic #selectedLesson)]
    , M.subs = [subscribeDocument r EditorDocChange]
    }
  where
    model = EditorModel {allLessons = [], selectedLesson = Nothing}

    update (EditorDocChange (DocumentChange d info)) =
      M.modify $ \m ->
        let lessons = listAllLessons d
            selected =
              if isInitialUpdate info
                then initialLessonId >>= \lid -> find (\l -> l.id == lid) lessons
                else m.selectedLesson >>= \sel -> find (\l -> l.id == sel.id) lessons
         in m & #allLessons .~ lessons & #selectedLesson .~ selected

    update (SelectLesson mLesson) =
      M.modify $ #selectedLesson .~ mLesson

    view m =
      MH.select_
        [ class_ "w-full px-3 py-2 border border-input rounded-md bg-background text-sm"
        , MH.onChange $ \v ->
            let t = M.fromMisoString v
             in SelectLesson $
                  if Text.null t
                    then Nothing
                    else find (\l -> idToText l.id == t) m.allLessons
        ]
        ( MH.option_
            [MP.value_ "", MP.selected_ (m.selectedLesson == Nothing)]
            [M.text $ C.translate' C.LblNoLesson]
            : map (lessonOption m.selectedLesson) m.allLessons
        )

    lessonOption current l =
      MH.option_
        [ MP.value_ (M.ms $ idToText l.id)
        , MP.selected_ (fmap (.id) current == Just l.id)
        ]
        [M.text $ M.ms $ if Text.null l.title then "(Untitled)" else l.title]

-- ============================================================================
-- Helpers
-- ============================================================================

-- | All lessons sorted by title
listAllLessons :: Document -> [Lesson]
listAllLessons d = sortOn (.title) $ Ix.toList d.lessons
