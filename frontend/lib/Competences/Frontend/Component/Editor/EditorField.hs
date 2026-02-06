module Competences.Frontend.Component.Editor.EditorField
  ( EditorField (..)
  , readOnlyField
  , textEditorField
  , richTextEditorField
  , boolEditorField
  , dayEditorField
  , optionalDayEditorField
  , optionalIntEditorField
  , enumEditorField
  , enumEditorField'
  , msIso
  , mkFieldLens
  , selectorEditorField
  , selectorEditorFieldNoStyle
  , selectorEditorFieldWithViewer
  )
where

import Competences.Command.Common (Change)
import Competences.Frontend.Component.Editor.Types (Action (..), Model (..))
import Competences.Frontend.Component.Editor.View (refocusTargetString)
import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.TaskContent.RichContent (RichContent, toRawText, fromTrustedInput)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Frontend.Component.Selector.Common
  ( EntityPatchTransformedLens (..)
  , SelectorTransformedLens
  , selectorTransformedLens
  )
import Competences.Frontend.View qualified as V
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Text.Read (readMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Optics.Core (Lens', at, lens, (%), (&), (?~), (^.))
import Optics.Core qualified as O
import Competences.Frontend.View.Component (componentA)

data EditorField a patch f = EditorField
  { viewer :: !(a -> M.View (Model a patch f) (Action a patch))
  , editor :: !(Bool -> a -> patch -> M.View (Model a patch f) (Action a patch))
  }
  deriving (Generic)

-- | Read-only field that shows the viewer in both view and edit modes
readOnlyField :: (a -> M.View (Model a patch f) (Action a patch)) -> EditorField a patch f
readOnlyField viewer =
  EditorField
    { viewer = viewer
    , editor = \_ a _ -> viewer a
    }

-- | Extract the current value from a patch, using the original value as fallback
currentValue :: a -> patch -> Lens' a field -> Lens' patch (Change field) -> field
currentValue original patch viewLens patchLens =
  case patch ^. patchLens of
    Just (_, after) -> after
    Nothing -> original ^. viewLens

-- | Create a lens from Model to field for a specific entity
--   This incorporates the currentValue logic and patch lookup
mkFieldLens
  :: (Ord a, Default patch)
  => Lens' a field
  -> Lens' patch (Change field)
  -> (a -> Lens' (Model a patch f) field)
mkFieldLens viewLens patchLens original =
  let getter model =
        case Map.lookup original (model ^. #patches) of
          Nothing -> original ^. viewLens -- No patch yet, use original value
          Just patch -> currentValue original patch viewLens patchLens
      setter model newValue =
        let oldPatch = Map.findWithDefault def original (model ^. #patches)
            newPatch = oldPatch & patchLens ?~ (original ^. viewLens, newValue)
         in model & #patches % at original ?~ newPatch
   in lens getter setter

textEditorField :: Lens' a Text -> Lens' patch (Change Text) -> EditorField a patch f
textEditorField viewLens patchLens =
  EditorField
    { viewer = textViewer viewLens
    , editor = textEditor viewLens patchLens
    }

textViewer :: Lens' a Text -> a -> M.View (Model a patch f) (Action a patch)
textViewer viewLens a = V.text_ (M.ms $ a ^. viewLens)

textEditor
  :: Lens' a Text
  -> Lens' patch (Change Text)
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
textEditor viewLens patchLens refocusTarget original patch =
  M.input_ $
    [ V.fullWidth
    , M.onChange
        (\v -> UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, M.fromMisoString v)))
    , M.value_ (M.ms $ currentValue original patch viewLens patchLens)
    ]
      <> refocusTargetAttr refocusTarget

msIso :: O.Iso' Text M.MisoString
msIso = O.iso M.ms M.fromMisoString

-- | Rich text editor field with markup rendering
--   Viewer: renders task content markup (paragraphs, emphasis, math, lists)
--   Editor: textarea for entering markup
richTextEditorField :: Lens' a RichContent -> Lens' patch (Change RichContent) -> EditorField a patch f
richTextEditorField viewLens patchLens =
  EditorField
    { viewer = richTextViewer viewLens
    , editor = richTextEditor viewLens patchLens
    }

richTextViewer :: Lens' a RichContent -> a -> M.View (Model a patch f) (Action a patch)
richTextViewer viewLens a =
  let content = a ^. viewLens
   in if content == mempty
        then Typography.placeholder "No content"
        else renderRichText content

richTextEditor
  :: Lens' a RichContent
  -> Lens' patch (Change RichContent)
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
richTextEditor viewLens patchLens refocusTarget original patch =
  let currentContent = currentValue original patch viewLens patchLens
   in M.div_
        [class_ "flex gap-4 w-full"]
        [ -- Editor panel (left)
          M.div_
            [class_ "flex-1 min-w-0"]
            [ M.span_ [class_ "block mb-1"] [Typography.fieldLabel "Markup"]
            , M.textarea_
                ( [ class_ "w-full min-h-[200px] resize-y font-mono text-sm p-2 border border-stone-300 rounded-md"
                  , M.onChange
                      (\v -> UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, fromTrustedInput (M.fromMisoString v))))
                  , M.value_ (M.ms (toRawText currentContent))
                  ]
                    <> refocusTargetAttr refocusTarget
                )
                []
            ]
        , -- Preview panel (right)
          M.div_
            [class_ "flex-1 min-w-0"]
            [ M.span_ [class_ "block mb-1"] [Typography.fieldLabel "Preview"]
            , M.div_
                [class_ "min-h-[200px] p-3 border border-stone-200 rounded-md bg-stone-50 overflow-auto"]
                [ if currentContent == mempty
                    then Typography.placeholder "No content"
                    else renderRichText currentContent
                ]
            ]
        ]

boolEditorField :: Lens' a Bool -> Lens' patch (Change Bool) -> EditorField a patch f
boolEditorField viewLens patchLens =
  EditorField
    { viewer = boolViewer viewLens
    , editor = boolEditor viewLens patchLens
    }

boolViewer :: Lens' a Bool -> a -> M.View (Model a patch f) (Action a patch)
boolViewer viewLens a =
  M.input_ [M.type_ "checkbox", M.checked_ (a ^. viewLens), M.disabled_]

boolEditor
  :: Lens' a Bool
  -> Lens' patch (Change Bool)
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
boolEditor viewLens patchLens refocusTarget original patch =
  M.input_ $
    [ M.type_ "checkbox"
    , M.checked_ (currentValue original patch viewLens patchLens)
    , M.onClick (UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, not $ currentValue original patch viewLens patchLens)))
    ]
      <> refocusTargetAttr refocusTarget

-- | Editor field for selectors (e.g., user selection, observation selection)
--   Takes an EntityPatchTransformedLens and transforms it to operate on the Model
selectorEditorField
  :: forall a f b f' b' patch ef cm ca s
   . (Eq cm, Ord a, Default patch)
  => M.MisoString
  -> EntityPatchTransformedLens a patch f b f' b'
  -> ( a
       -> s
       -> SelectorTransformedLens (Model a patch ef) f b f' b'
       -> M.Component (Model a patch ef) cm ca
     )
  -> (s, s)
  -> EditorField a patch ef
selectorEditorField k eptl mkEditorComponent (viewerStyle, editorStyle) =
  let mkLens = mkFieldLens eptl.viewLens eptl.patchLens
      l' a = selectorTransformedLens eptl.transform eptl.embed (mkLens a)
   in EditorField
        { viewer = \a -> V.component (k <> "-viewer") (mkEditorComponent a viewerStyle (l' a))
        , editor = \refocusTarget a _ ->
            componentA (k <> "-editor") (refocusTargetAttr refocusTarget) (
              mkEditorComponent a editorStyle (l' a))
        }

-- | Editor field for selectors without style parameter (e.g., searchable selectors)
--   Uses the same component for both viewing and editing
selectorEditorFieldNoStyle
  :: forall a f b f' b' patch ef cm ca
   . (Eq cm, Ord a, Default patch)
  => M.MisoString
  -> EntityPatchTransformedLens a patch f b f' b'
  -> ( a
       -> SelectorTransformedLens (Model a patch ef) f b f' b'
       -> M.Component (Model a patch ef) cm ca
     )
  -> EditorField a patch ef
selectorEditorFieldNoStyle k eptl mkEditorComponent =
  let mkLens = mkFieldLens eptl.viewLens eptl.patchLens
      l' a = selectorTransformedLens eptl.transform eptl.embed (mkLens a)
   in EditorField
        { viewer = \a -> V.component (k <> "-viewer") (mkEditorComponent a (l' a))
        , editor = \refocusTarget a _ ->
            componentA (k <> "-editor") (refocusTargetAttr refocusTarget) (
              mkEditorComponent a (l' a))
        }

-- | Editor field for selectors with separate viewer and editor components
--   Viewer: read-only display component (e.g., comma-separated list of selected items)
--   Editor: interactive selection component (e.g., searchable combobox)
selectorEditorFieldWithViewer
  :: forall a f b f' b' patch ef vmm vma emm ema
   . (Eq vmm, Eq emm, Ord a, Default patch)
  => M.MisoString
  -> EntityPatchTransformedLens a patch f b f' b'
  -> ( a
       -> SelectorTransformedLens (Model a patch ef) f b f' b'
       -> M.Component (Model a patch ef) vmm vma
     )
  -- ^ Viewer component factory (read-only display)
  -> ( a
       -> SelectorTransformedLens (Model a patch ef) f b f' b'
       -> M.Component (Model a patch ef) emm ema
     )
  -- ^ Editor component factory (interactive selection)
  -> EditorField a patch ef
selectorEditorFieldWithViewer k eptl mkViewerComponent mkEditorComponent =
  let mkLens = mkFieldLens eptl.viewLens eptl.patchLens
      l' a = selectorTransformedLens eptl.transform eptl.embed (mkLens a)
   in EditorField
        { viewer = \a -> V.component (k <> "-viewer") (mkViewerComponent a (l' a))
        , editor = \refocusTarget a _ ->
            componentA (k <> "-editor") (refocusTargetAttr refocusTarget) (
              mkEditorComponent a (l' a))
        }

dayEditorField :: Lens' a Day -> Lens' patch (Change Day) -> EditorField a patch f
dayEditorField viewLens patchLens =
  EditorField
    { viewer = dayViewer viewLens
    , editor = dayEditor viewLens patchLens
    }

dayViewer :: Lens' a Day -> a -> M.View (Model a patch f) (Action a patch)
dayViewer viewLens a =
  M.input_ [M.type_ "date", M.value_ (showTime $ a ^. viewLens), M.disabled_]
  where
    showTime day = M.toMisoString $ show day

dayEditor
  :: Lens' a Day
  -> Lens' patch (Change Day)
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
dayEditor viewLens patchLens refocusTarget original patch =
  M.input_ $
    [ M.type_ "date"
    , M.value_ (showTime $ currentValue original patch viewLens patchLens)
    , M.onChange
        ( \v -> case parseTime v of
            Just v' -> UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, v'))
            Nothing -> UpdatePatch original patch
        )
    ]
      <> refocusTargetAttr refocusTarget
  where
    parseTime dateStr = parseTimeM False defaultTimeLocale "%Y-%m-%d" (M.fromMisoString dateStr)
    showTime day = M.toMisoString $ show day

-- | Optional day editor field for Maybe Day fields
--   Viewer: shows date or "Not set"
--   Editor: date input where empty string clears the value
optionalDayEditorField :: Lens' a (Maybe Day) -> Lens' patch (Change (Maybe Day)) -> EditorField a patch f
optionalDayEditorField viewLens patchLens =
  EditorField
    { viewer = optionalDayViewer viewLens
    , editor = optionalDayEditor viewLens patchLens
    }

optionalDayViewer :: Lens' a (Maybe Day) -> a -> M.View (Model a patch f) (Action a patch)
optionalDayViewer viewLens a =
  case a ^. viewLens of
    Nothing -> Typography.placeholder "Not set"
    Just day -> M.input_ [M.type_ "date", M.value_ (showTime day), M.disabled_]
  where
    showTime day = M.toMisoString $ show day

optionalDayEditor
  :: Lens' a (Maybe Day)
  -> Lens' patch (Change (Maybe Day))
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
optionalDayEditor viewLens patchLens refocusTarget original patch =
  M.input_ $
    [ M.type_ "date"
    , M.value_ (maybe "" showTime $ currentValue original patch viewLens patchLens)
    , M.onChange
        ( \v ->
            let newVal = parseTime v
             in UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, newVal))
        )
    ]
      <> refocusTargetAttr refocusTarget
  where
    parseTime dateStr =
      let s = M.fromMisoString dateStr
       in if T.null s then Nothing else parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack s)
    showTime day = M.toMisoString $ show day

-- | Optional int editor field for Maybe Int fields
--   Viewer: shows number or "Not set"
--   Editor: number input where empty string clears the value
optionalIntEditorField :: Lens' a (Maybe Int) -> Lens' patch (Change (Maybe Int)) -> EditorField a patch f
optionalIntEditorField viewLens patchLens =
  EditorField
    { viewer = optionalIntViewer viewLens
    , editor = optionalIntEditor viewLens patchLens
    }

optionalIntViewer :: Lens' a (Maybe Int) -> a -> M.View (Model a patch f) (Action a patch)
optionalIntViewer viewLens a =
  case a ^. viewLens of
    Nothing -> Typography.placeholder "Not set"
    Just n -> M.span_ [] [M.text $ M.ms $ show n]

optionalIntEditor
  :: Lens' a (Maybe Int)
  -> Lens' patch (Change (Maybe Int))
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
optionalIntEditor viewLens patchLens refocusTarget original patch =
  M.input_ $
    [ M.type_ "number"
    , M.min_ "0"
    , M.value_ (maybe "" (M.ms . show) $ currentValue original patch viewLens patchLens)
    , M.onChange
        ( \v ->
            let newVal = parseInt v
             in UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, newVal))
        )
    ]
      <> refocusTargetAttr refocusTarget
  where
    parseInt numStr =
      let s = M.fromMisoString numStr :: Text
       in if T.null s then Nothing else readMaybe (T.unpack s)

enumParseMap :: (Show e, Bounded e, Enum e) => Map.Map M.MisoString e
enumParseMap = Map.fromList $ map (\e -> (M.ms $ show e, e)) [minBound .. maxBound]

enumEditorField
  :: forall a e patch f
   . (Show e, Bounded e, Enum e, Eq e)
  => (e -> M.MisoString)
  -> Lens' a e
  -> Lens' patch (Change e)
  -> EditorField a patch f
enumEditorField toText viewLens patchLens =
  EditorField
    { viewer = enumViewer toText viewLens
    , editor = enumEditor toText viewLens patchLens
    }

enumViewer :: (e -> M.MisoString) -> Lens' a e -> a -> M.View (Model a patch f) (Action a patch)
enumViewer toText viewLens a = V.text_ (toText $ a ^. viewLens)

enumEditor
  :: forall a e patch f
   . (Show e, Bounded e, Enum e, Eq e)
  => (e -> M.MisoString)
  -> Lens' a e
  -> Lens' patch (Change e)
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
enumEditor toText viewLens patchLens refocusTarget original patch =
  M.select_
    ( [ M.onChange
          ( \v -> case enumParseMap Map.!? v of
              Just v' -> UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, v'))
              Nothing -> UpdatePatch original patch
          )
      , V.fullWidth
      ]
        <> refocusTargetAttr refocusTarget
    )
    $ map
      ( \e ->
          M.option_
            [ M.value_ (M.ms $ show e)
            , M.selected_ (e == currentValue original patch viewLens patchLens)
            ]
            [M.text_ [toText e]]
      )
      [minBound .. maxBound]

enumEditorField'
  :: forall a e patch f
   . (Show e, Bounded e, Enum e, Eq e)
  => Lens' a e
  -> Lens' patch (Change e)
  -> EditorField a patch f
enumEditorField' = enumEditorField (M.ms . show)

refocusTargetAttr :: Bool -> [M.Attribute action]
refocusTargetAttr True = [M.id_ refocusTargetString]
refocusTargetAttr False = []
