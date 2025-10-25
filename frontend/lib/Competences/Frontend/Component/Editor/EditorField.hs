module Competences.Frontend.Component.Editor.EditorField
  ( EditorField (..)
  , textEditorField
  , dayEditorField
  )
where

import Competences.Frontend.Component.Editor.Types (Action (..), Model)
import Competences.Frontend.View qualified as V
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Optics.Core (Lens', (&), (.~), (^.))
import Optics.Core qualified as O

data EditorField a f = EditorField
  { viewer :: !(a -> M.View (Model a f) (Action a))
  , editor :: !(a -> a -> M.View (Model a f) (Action a))
  }
  deriving (Generic)

textEditorField :: Lens' a M.MisoString -> EditorField a f
textEditorField l =
  EditorField
    { viewer = textViewer (O.castOptic l)
    , editor = textEditor l
    }

dayEditorField :: Lens' a Day -> EditorField a f
dayEditorField l =
  let parseTime :: M.MisoString -> Maybe Day
      parseTime dateStr = parseTimeM False defaultTimeLocale "%Y-%m-%d" (M.fromMisoString dateStr)

      showTime :: Day -> M.MisoString
      showTime day = M.toMisoString $ show day
   in EditorField
        { viewer = \a -> M.input_ [M.type_ "date", M.value_ (showTime $ a ^. l), M.disabled_]
        , editor = \original patched ->
            M.input_
              [ M.type_ "date"
              , M.value_ (showTime $ patched ^. l)
              , M.onChange
                  ( \v -> case parseTime v of
                      Just v' -> UpdatePatch original (patched & (l .~ v'))
                      Nothing -> UpdatePatch original patched
                  )
              ]
        }

textViewer :: Lens' a M.MisoString -> a -> M.View (Model a f) (Action a)
textViewer l a =
  let s = a ^. l
   in V.text_ s

textEditor :: Lens' a M.MisoString -> a -> a -> M.View (Model a f) (Action a)
textEditor l original patched =
  M.input_ [M.onChange (\v -> UpdatePatch original (patched & (l .~ v))), M.value_ (O.view l patched)]
