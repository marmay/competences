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
--
-- Validation (markdown + geometry blocks) is debounced: errors are only
-- computed after 1 second of typing silence, avoiding wasteful re-parses
-- on every keystroke.
module Competences.Frontend.Component.MarkdownEditor
  ( -- * Stateless view helper
    markdownTextarea

    -- * Self-contained RichContent component
  , richContentEditorComponent
  , RichContentEditorModel
  , RichContentEditorAction

    -- * Content state wrapper
  , ContentState (..)
  , contentValue
  , isContentValid
  )
where

import Competences.Frontend.Component.RichContent (FormulaCache, renderMarkdownText)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Competences.Markdown.Parser qualified as Markdown
import Competences.Markdown.Validation (ValidationError (..), validateMarkdown)
import Competences.TaskContent.RichContent (RichContent, fromTrustedInput, toRawText)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Data.Text qualified as T
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

-- | A markdown textarea (no inline validation).
--
-- Renders a textarea with optional error-border styling. Validation errors
-- are displayed separately by the component view.
markdownTextarea
  :: [M.Attribute action]
  -- ^ Extra attributes for the textarea
  -> Text
  -- ^ Current raw text content
  -> (Text -> action)
  -- ^ Action constructor for text changes
  -> Text
  -- ^ Minimum height CSS value
  -> Bool
  -- ^ Whether to show error border
  -> M.View model action
markdownTextarea extraAttrs rawText onTextChange minHeight hasError =
  MH.textarea_
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

-- ============================================================================
-- Content state wrapper
-- ============================================================================

-- | Wrapper that communicates a field's readiness state to the parent.
--
-- Used by the MarkdownEditor to signal whether content is still being
-- debounced, has been validated successfully, or failed validation.
data ContentState a
  = -- | User is still typing; debounce timer has not fired yet
    Debouncing
  | -- | Content parsed and validated successfully
    Valid !a
  | -- | Content failed validation with an error message
    Invalid !Text
  deriving (Eq, Show, Generic)

-- | Extract the value from a 'ContentState', falling back to a default.
contentValue :: a -> ContentState a -> a
contentValue _ (Valid a) = a
contentValue fallback _ = fallback

-- | Check if a 'ContentState' is 'Valid'.
isContentValid :: ContentState a -> Bool
isContentValid (Valid _) = True
isContentValid _ = False

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
  -- ^ Internal: for preview rendering (always last valid parse)
  , contentState :: !(ContentState RichContent)
  -- ^ Exposed via binding — communicates readiness to parent
  , validationGen :: !Int
  -- ^ Generation counter for debouncing validation
  , validationErrors :: ![ValidationError]
  -- ^ Last validation result
  }
  deriving (Eq, Generic)

-- | Actions for 'richContentEditorComponent'.
data RichContentEditorAction
  = -- | User typed in textarea
    RCSetText !Text
  | -- | Toggle edit / preview
    RCTogglePreview
  | -- | Debounced validation trigger (carries generation counter)
    RCValidate !Int
  deriving (Eq, Show)

-- | A self-contained Miso component for editing 'RichContent'.
--
-- Takes an initial 'RichContent' value and an optics 'Lens'' pointing to the
-- parent's 'RichContent' field.  The component manages its own raw-text buffer
-- and preview toggle.  Only valid parses are propagated to the parent via a
-- child-to-parent binding (@\<---@).
--
-- Validation is debounced: after each keystroke, a 1-second timer is started.
-- If no further keystrokes arrive, validation runs and errors are displayed.
-- Stale timers are discarded via a generation counter.
--
-- @
-- componentA "rc-editor" []
--   (richContentEditorComponent initialContent #description)
-- @
richContentEditorComponent
  :: FormulaCache
  -> RichContent
  -> O.Lens' p (ContentState RichContent)
  -> M.Component p RichContentEditorModel RichContentEditorAction
richContentEditorComponent fc initialContent parentLens =
  (M.component model update view)
    { M.bindings = [O.toLensVL parentLens M.<--- O.toLensVL #contentState]
    , M.initialAction = Just (RCValidate 0)
    }
  where
    model =
      RichContentEditorModel
        { rawText = toRawText initialContent
        , previewing = False
        , validContent = initialContent
        , contentState = Valid initialContent
        , validationGen = 0
        , validationErrors = []
        }

    update (RCSetText txt) = do
      m <- M.get
      let gen = m.validationGen + 1
      M.modify $ \m' -> m'{rawText = txt, validationGen = gen, contentState = Debouncing}
      -- Schedule debounced validation
      M.io $ do
        threadDelay 1000000
        pure (RCValidate gen)

    update RCTogglePreview =
      M.modify $ \m -> m{previewing = not m.previewing}

    update (RCValidate gen) = do
      m <- M.get
      -- Only run if this is the most recent generation (not stale)
      if gen /= m.validationGen
        then pure ()
        else do
          let errors = validateMarkdown m.rawText
          case Markdown.parseMarkdown m.rawText of
            Right _doc ->
              let rc = fromTrustedInput m.rawText
               in M.modify $ \m' ->
                    m'{validationErrors = errors, validContent = rc, contentState = Valid rc}
            Left err ->
              M.modify $ \m' ->
                m'{validationErrors = errors, contentState = Invalid (T.pack $ show err)}

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
            then previewView fc (toRawText m.validContent) "150px"
            else editView m
        ]

-- | Edit view: textarea + validation errors
editView :: RichContentEditorModel -> M.View RichContentEditorModel RichContentEditorAction
editView m =
  MH.div_
    []
    [ markdownTextarea [] m.rawText RCSetText "150px" (not $ null m.validationErrors)
    , -- Show validation errors below textarea
      if null m.validationErrors
        then M.text ""
        else
          MH.div_
            [class_ "mt-2 space-y-1"]
            (map validationErrorView m.validationErrors)
    ]

-- ============================================================================
-- Shared helpers
-- ============================================================================

-- | Preview view: rendered markdown
previewView :: FormulaCache -> Text -> Text -> M.View model action
previewView fc rawText minHeight =
  MH.div_
    [ class_ "p-3 border border-input rounded-md bg-muted/50 overflow-auto"
    , MC.style_ [("min-height", ms minHeight)]
    ]
    [ renderMarkdownText fc rawText
    ]

-- | Render a single validation error with its context label
validationErrorView :: ValidationError -> M.View model action
validationErrorView err =
  MH.div_
    [class_ "p-2 bg-red-50 border border-red-200 rounded text-red-700 text-xs font-mono whitespace-pre-wrap"]
    [ MH.span_ [class_ "font-semibold"] [M.text $ ms $ err.context <> ": "]
    , M.text $ ms err.message
    ]
