module Competences.Frontend.Component.Editor.EditorField
  ( EditorField (..)
  , textEditorField
  , dayEditorField
  , enumEditorField
  , enumEditorField'
  , msIso
  , selectorEditorField
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

selectorEditorField
  :: forall a f b f' b' ef cm ca s
   . (Eq cm, Ord a)
  => M.MisoString
  -> SelectorTransformedLens a f b f' b'
  -> (a -> s -> SelectorTransformedLens (Model a ef) f b f' b' -> M.Component (Model a ef) cm ca)
  -> (s, s)
  -> EditorField a ef
selectorEditorField k l mkEditorComponent (viewerStyle, editorStyle) =
  let
    l' p = selectorTransformedLens l.transform l.embed (#patches O.% O.at p O.% maybeLens p O.% l.lens)
  in EditorField
    { viewer = \a -> V.component (k <> "-viewer") (mkEditorComponent a viewerStyle (l' a))
    , editor = \refocusTarget a _ ->
         M.div_ (M.key_ (k <> "-editor") : refocusTargetAttr refocusTarget)
           M.+> mkEditorComponent a editorStyle (l' a)
    }
  where
    maybeLens :: (Eq a) => a -> Lens' (Maybe a) a
    maybeLens v = O.lens (fromMaybe v) (\_ v' -> Just v')

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
