-- |
-- Module      : Competences.Frontend.Component.MarkdownEditor
-- Description : Reusable markdown editor with edit/preview toggle
--
-- Provides a self-contained Miso component ('richContentEditorComponent')
-- that manages its own preview state and bidirectionally binds a 'RichContent'
-- field on the parent model.
--
-- The component only propagates valid parses to the parent, so the parent
-- always holds a well-formed 'RichContent' value.
module Competences.Frontend.Component.MarkdownEditor
  ( -- * Stateless view helper
    markdownTextarea

    -- * Self-contained RichContent component
  , richContentEditorComponent
  , RichContentEditorModel
  , RichContentEditorAction
  )
where

import Competences.Frontend.Component.RichContent (renderMarkdownText)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.RichContent (RichContent, fromTrustedInput, toRawText)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)
import Optics.Core qualified as O

-- ============================================================================
-- Stateless view helpers
-- ============================================================================

-- | A markdown textarea with parse error display.
--
-- Extracted from 'markdownEditorView' so it can be reused (e.g. inside
-- 'richContentEditorComponent').  Extra attributes (e.g. an id for
-- refocus-target) can be passed via the first parameter.
markdownTextarea
  :: [M.Attribute action]
  -- ^ Extra attributes for the textarea
  -> Text
  -- ^ Current raw text content
  -> (Text -> action)
  -- ^ Action constructor for text changes
  -> Text
  -- ^ Minimum height CSS value
  -> M.View model action
markdownTextarea extraAttrs rawText onTextChange minHeight =
  let hasError = checkParseError rawText
   in MH.div_
        []
        [ MH.textarea_
            ( [ class_ $
                  "w-full px-3 py-2 border rounded-md bg-background font-mono text-sm resize-y"
                    <> (if hasError then " border-red-300" else " border-input")
              , MP.value_ (ms rawText)
              , MH.onInput (onTextChange . M.fromMisoString)
              , MC.style_ [("min-height", ms minHeight)]
              ]
                <> extraAttrs
            )
            []
        , -- Show parse error below textarea
          if hasError
            then parseErrorBox rawText
            else M.text ""
        ]

-- ============================================================================
-- RichContent editor component
-- ============================================================================

-- | Internal model for 'richContentEditorComponent'.
data RichContentEditorModel = RichContentEditorModel
  { rawText :: !Text
  -- ^ Current textarea content (always up-to-date with user input)
  , previewing :: !Bool
  -- ^ Edit/preview toggle state
  , validContent :: !RichContent
  -- ^ Bound to parent — only updated when parse succeeds
  }
  deriving (Eq, Generic)

-- | Actions for 'richContentEditorComponent'.
data RichContentEditorAction
  = -- | User typed in textarea
    RCSetText !Text
  | -- | Toggle edit / preview
    RCTogglePreview
  deriving (Eq, Show)

-- | A self-contained Miso component for editing 'RichContent'.
--
-- Takes an initial 'RichContent' value and an optics 'Lens'' pointing to the
-- parent's 'RichContent' field.  The component manages its own raw-text buffer
-- and preview toggle.  Only valid parses are propagated to the parent via a
-- child-to-parent binding (@\<---@).
--
-- @
-- componentA "rc-editor" []
--   (richContentEditorComponent initialContent #description)
-- @
richContentEditorComponent
  :: RichContent
  -> O.Lens' p RichContent
  -> M.Component p RichContentEditorModel RichContentEditorAction
richContentEditorComponent initialContent parentLens =
  (M.component model update view)
    { M.bindings = [O.toLensVL parentLens M.<--- O.toLensVL #validContent]
    }
  where
    model =
      RichContentEditorModel
        { rawText = toRawText initialContent
        , previewing = False
        , validContent = initialContent
        }

    update (RCSetText txt) = do
      M.modify $ \m -> m{rawText = txt}
      case Markdown.parseMarkdown txt of
        Right _doc -> M.modify $ \m -> m{validContent = fromTrustedInput txt}
        Left _err -> pure () -- keep last valid value

    update RCTogglePreview =
      M.modify $ \m -> m{previewing = not m.previewing}

    view m =
      MH.div_
        [class_ "w-full"]
        [ -- Toggle bar
          MH.div_
            [class_ "flex items-center justify-end mb-1"]
            [ Button.ghostSm $
                Button.button
                  (if m.previewing then ("Edit" :: M.MisoString) else "Preview")
                  (Just RCTogglePreview)
            ]
        , -- Content area
          if m.previewing
            then previewView (toRawText m.validContent) "150px"
            else markdownTextarea [] m.rawText RCSetText "150px"
        ]

-- ============================================================================
-- Shared helpers
-- ============================================================================

-- | Preview view: rendered markdown
previewView :: Text -> Text -> M.View model action
previewView rawText minHeight =
  MH.div_
    [ class_ "p-3 border border-input rounded-md bg-muted/50 overflow-auto"
    , MC.style_ [("min-height", ms minHeight)]
    ]
    [ renderMarkdownText rawText
    ]

-- | Check if text has a parse error
checkParseError :: Text -> Bool
checkParseError t = case Markdown.parseMarkdown t of
  Left _ -> True
  Right _ -> False

-- | Show a styled parse error box
parseErrorBox :: Text -> M.View model action
parseErrorBox raw =
  case Markdown.parseMarkdown raw of
    Left err ->
      MH.div_
        [class_ "mt-2 p-2 bg-red-50 border border-red-200 rounded text-red-700 text-xs font-mono whitespace-pre-wrap"]
        [M.text $ ms $ Markdown.formatParseError err]
    Right _ -> M.text ""
