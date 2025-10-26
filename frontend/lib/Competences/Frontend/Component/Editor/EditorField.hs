module Competences.Frontend.Component.Editor.EditorField
  ( EditorField (..)
  , textEditorField
  , dayEditorField
  , enumEditorField
  , enumEditorField'
  , msIso
  )
where

import Competences.Frontend.Component.Editor.Types (Action (..), Model)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.Map qualified as Map
import Data.Text (Text)
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

msIso :: O.Iso' Text M.MisoString
msIso = O.iso M.ms M.fromMisoString

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

enumParseMap :: (Show e, Bounded e, Enum e) => Map.Map M.MisoString e
enumParseMap = Map.fromList $ map (\e -> (M.ms $ show e, e)) [minBound .. maxBound]

enumEditorField
  :: forall a e f. (Show e, Bounded e, Enum e, Eq e) => (e -> M.MisoString) -> Lens' a e -> EditorField a f
enumEditorField toText l =
  EditorField
    { viewer = V.text_ . toText . O.view l
    , editor = \original patched ->
        M.select_
          [ M.onChange
              ( \v -> case enumParseMap Map.!? v of
                  Just v' -> UpdatePatch original (patched & (l .~ v'))
                  Nothing -> UpdatePatch original patched
              )
          , M.value_ (toText $ patched ^. l)
          , T.tailwind [T.WFull]
          ]
          $ map
            ( \e ->
                M.option_
                  [ M.value_ (M.ms $ show e)
                  , M.selected_ (e == patched ^. l)
                  ]
                  [M.text_ [toText e]]
            )
            [minBound .. maxBound]
    }

enumEditorField'
  :: forall a e f. (Show e, Bounded e, Enum e, Eq e) => Lens' a e -> EditorField a f
enumEditorField' = enumEditorField (M.ms . show)

textViewer :: Lens' a M.MisoString -> a -> M.View (Model a f) (Action a)
textViewer l a =
  let s = a ^. l
   in V.text_ s

textEditor :: Lens' a M.MisoString -> a -> a -> M.View (Model a f) (Action a)
textEditor l original patched =
  M.input_
    [ T.tailwind [T.WFull]
    , M.onChange (\v -> UpdatePatch original (patched & (l .~ v)))
    , M.value_ (O.view l patched)
    ]
