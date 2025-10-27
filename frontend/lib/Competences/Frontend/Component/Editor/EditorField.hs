module Competences.Frontend.Component.Editor.EditorField
  ( EditorField (..)
  , textEditorField
  , dayEditorField
  , enumEditorField
  , enumEditorField'
  , msIso
  , hostEditorField
  )
where

import Competences.Frontend.Component.Editor.Types (Action (..), Model (..))
import Competences.Frontend.Component.Editor.View (refocusTargetString)
import Competences.Frontend.Component.Selector.Common
  ( SelectorTransformedLens (..)
  , selectorTransformedLens
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
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
  , editor :: !(Bool -> a -> a -> M.View (Model a f) (Action a))
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

hostEditorField
  :: forall a b b' f cm ca
   . (Eq cm, Ord a)
  => SelectorTransformedLens a b b'
  -> (b' -> M.View (Model a f) (Action a))
  -> (SelectorTransformedLens (Model a f) b b' -> M.Component (Model a f) cm ca)
  -> EditorField a f
hostEditorField l viewer mkEditorComponent =
  EditorField
    { viewer = \a -> viewer (a ^. l.lens)
    , editor = \refocusTarget original _ ->
        let l' =
              selectorTransformedLens l.transformer (#patches O.% O.at original O.% maybeLens original O.% l.lens)
            editorComponent = mkEditorComponent l'
         in M.div_ (refocusTargetAttr refocusTarget) M.+> editorComponent
    }
  where
    maybeLens :: (Eq a) => a -> Lens' (Maybe a) a
    maybeLens v = O.lens (fromMaybe v) (\_ v' -> if v == v' then Nothing else Just v')

dayEditorField :: Lens' a Day -> EditorField a f
dayEditorField l =
  let parseTime :: M.MisoString -> Maybe Day
      parseTime dateStr = parseTimeM False defaultTimeLocale "%Y-%m-%d" (M.fromMisoString dateStr)

      showTime :: Day -> M.MisoString
      showTime day = M.toMisoString $ show day
   in EditorField
        { viewer = \a -> M.input_ [M.type_ "date", M.value_ (showTime $ a ^. l), M.disabled_]
        , editor = \refocusTarget original patched ->
            M.input_ $
              [ M.type_ "date"
              , M.value_ (showTime $ patched ^. l)
              , M.onChange
                  ( \v -> case parseTime v of
                      Just v' -> UpdatePatch original (patched & (l .~ v'))
                      Nothing -> UpdatePatch original patched
                  )
              ]
                <> refocusTargetAttr refocusTarget
        }

enumParseMap :: (Show e, Bounded e, Enum e) => Map.Map M.MisoString e
enumParseMap = Map.fromList $ map (\e -> (M.ms $ show e, e)) [minBound .. maxBound]

enumEditorField
  :: forall a e f
   . (Show e, Bounded e, Enum e, Eq e) => (e -> M.MisoString) -> Lens' a e -> EditorField a f
enumEditorField toText l =
  EditorField
    { viewer = V.text_ . toText . O.view l
    , editor = \refocusTarget original patched ->
        M.select_
          ( [ M.onChange
                ( \v -> case enumParseMap Map.!? v of
                    Just v' -> UpdatePatch original (patched & (l .~ v'))
                    Nothing -> UpdatePatch original patched
                )
            , M.value_ (toText $ patched ^. l)
            , T.tailwind [T.WFull]
            ]
              <> refocusTargetAttr refocusTarget
          )
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

textEditor :: Lens' a M.MisoString -> Bool -> a -> a -> M.View (Model a f) (Action a)
textEditor l refocusTarget original patched =
  M.input_ $
    [ T.tailwind [T.WFull]
    , M.onChange (\v -> UpdatePatch original (patched & (l .~ v)))
    , M.value_ (O.view l patched)
    ]
      <> refocusTargetAttr refocusTarget

refocusTargetAttr :: Bool -> [M.Attribute action]
refocusTargetAttr True = [M.id_ refocusTargetString]
refocusTargetAttr False = []
