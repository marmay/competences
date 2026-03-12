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
  , fileUploadEditorField
  )
where

import Competences.Command.Common (Change)
import Competences.Frontend.Component.Editor.Types (Action (..), Model (..))
import Competences.Frontend.Component.Editor.View (refocusTargetString)
import Competences.Frontend.Component.FileUpload (fileUploadComponent)
import Competences.Frontend.Component.MarkdownEditor (ContentState (..), richContentEditorComponent)
import Competences.Frontend.Component.RichContent (FormulaCache, renderRichText)
import Competences.Document.FileRef (FileRef (..))
import Competences.Frontend.SyncContext.SyncDocument (SyncContext)
import Competences.TaskContent.RichContent (RichContent)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Frontend.Component.Selector.Common
  ( EntityPatchTransformedLens (..)
  , SelectorTransformedLens
  , selectorTransformedLens
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Text (text_)
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
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
textViewer viewLens a = text_ (M.ms $ a ^. viewLens)

textEditor
  :: Lens' a Text
  -> Lens' patch (Change Text)
  -> Bool
  -> a
  -> patch
  -> M.View (Model a patch f) (Action a patch)
textEditor viewLens patchLens refocusTarget original patch =
  M.input_ $
    [ class_ "w-full"
    , M.onChange
        (\v -> UpdatePatch original (patch & patchLens ?~ (original ^. viewLens, M.fromMisoString v)))
    , M.value_ (M.ms $ currentValue original patch viewLens patchLens)
    ]
      <> refocusTargetAttr refocusTarget

msIso :: O.Iso' Text M.MisoString
msIso = O.iso M.ms M.fromMisoString

-- | Create a lens from Model to 'ContentState RichContent' for a specific entity.
--
-- On 'get': returns the stored 'ContentState', defaulting to 'Valid' with the
-- current value if no state is recorded.
-- On 'set': writes the state into 'contentStates', and — when 'Valid' —
-- also writes the value into 'patches'.
mkContentStateLens
  :: (Ord a, Default patch)
  => Text
  -> Lens' a RichContent
  -> Lens' patch (Change RichContent)
  -> a
  -> Lens' (Model a patch f) (ContentState RichContent)
mkContentStateLens fieldName viewLens patchLens original = lens getter setter
  where
    getter m = case Map.lookup original m.contentStates >>= Map.lookup fieldName of
      Just cs -> cs
      Nothing -> Valid (currentValue original (Map.findWithDefault def original m.patches) viewLens patchLens)

    setter m cs@(Valid rc) =
      m
        { contentStates = insertCS m cs
        , patches = Map.alter (Just . setPatch) original m.patches
        }
      where
        setPatch = maybe (def & patchLens ?~ (original ^. viewLens, rc)) (\p -> p & patchLens ?~ (original ^. viewLens, rc))
    setter m cs = m{contentStates = insertCS m cs}

    insertCS m cs = Map.alter (Just . Map.insert fieldName cs . fromMaybe Map.empty) original m.contentStates

-- | Rich text editor field with markup rendering
--   Viewer: renders task content markup (paragraphs, emphasis, math, lists)
--   Editor: self-contained component with edit/preview toggle
richTextEditorField
  :: (Ord a, Default patch)
  => FormulaCache
  -> Text
  -> Lens' a RichContent
  -> Lens' patch (Change RichContent)
  -> EditorField a patch f
richTextEditorField fc fieldName viewLens patchLens =
  EditorField
    { viewer = richTextViewer fc viewLens
    , editor = \refocusTarget original patch ->
        inlineComponentAttrs
          "rc-editor"
          (refocusTargetAttr refocusTarget)
          (richContentEditorComponent fc
            (currentValue original patch viewLens patchLens)
            (mkContentStateLens fieldName viewLens patchLens original))
    }

richTextViewer :: FormulaCache -> Lens' a RichContent -> a -> M.View (Model a patch f) (Action a patch)
richTextViewer fc viewLens a =
  let content = a ^. viewLens
   in if content == mempty
        then Typography.placeholder "No content"
        else renderRichText fc content

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
        { viewer = \a -> inlineComponent (k <> "-viewer") (mkEditorComponent a viewerStyle (l' a))
        , editor = \refocusTarget a _ ->
            inlineComponentAttrs (k <> "-editor") (refocusTargetAttr refocusTarget) (
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
        { viewer = \a -> inlineComponent (k <> "-viewer") (mkEditorComponent a (l' a))
        , editor = \refocusTarget a _ ->
            inlineComponentAttrs (k <> "-editor") (refocusTargetAttr refocusTarget) (
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
        { viewer = \a -> inlineComponent (k <> "-viewer") (mkViewerComponent a (l' a))
        , editor = \refocusTarget a _ ->
            inlineComponentAttrs (k <> "-editor") (refocusTargetAttr refocusTarget) (
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
enumViewer toText viewLens a = text_ (toText $ a ^. viewLens)

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
      , class_ "w-full"
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

-- | File upload editor field for [FileRef] fields
--   Viewer: shows list of file names or placeholder
--   Editor: self-contained file upload component
fileUploadEditorField
  :: (Ord a, Default patch)
  => SyncContext
  -> Lens' a [FileRef]
  -> Lens' patch (Change [FileRef])
  -> EditorField a patch f
fileUploadEditorField r viewLens patchLens =
  EditorField
    { viewer = attachmentsViewer viewLens
    , editor = \_ original patch ->
        inlineComponent "file-upload"
          (fileUploadComponent r
            Nothing
            (currentValue original patch viewLens patchLens)
            (mkFieldLens viewLens patchLens original))
    }

attachmentsViewer :: Lens' a [FileRef] -> a -> M.View (Model a patch f) (Action a patch)
attachmentsViewer viewLens a =
  let files = a ^. viewLens
   in if null files
        then Typography.placeholder "—"
        else M.ul_ [class_ "text-sm space-y-1"]
              [M.li_ [] [M.text (M.ms f.fileName)] | f <- files]

refocusTargetAttr :: Bool -> [M.Attribute action]
refocusTargetAttr True = [M.id_ refocusTargetString]
refocusTargetAttr False = []
