-- |
-- Module      : Competences.Frontend.Component.MarkdownEditor
-- Description : Reusable markdown editor with edit/preview toggle
--
-- A view helper for markdown content editing with:
--
-- * Edit mode: monospace textarea with parse error display
-- * Preview mode: rendered markdown output (or error)
-- * Toggle button to switch between modes
-- * Parse errors shown immediately in editing mode
--
-- The parent component owns the state; this module provides view functions.
--
-- Usage in parent model:
--
-- @
-- data Model = Model { descriptionText :: !Text, descriptionPreview :: !Bool }
-- data Action = SetDescription !Text | ToggleDescriptionPreview
--
-- view m = markdownEditorView
--   m.descriptionText
--   m.descriptionPreview
--   SetDescription
--   ToggleDescriptionPreview
--   "150px"
-- @
module Competences.Frontend.Component.MarkdownEditor
  ( markdownEditorView
  )
where

import Competences.Frontend.Component.RichContent (renderRichText)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Tailwind (class_)
import Competences.Markdown.Parser qualified as Markdown
import Competences.TaskContent.RichContent (fromTrustedInput)
import Data.Text (Text)
import Miso qualified as M
import Miso.CSS qualified as MC
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (ms)

-- | Render a markdown editor with edit/preview toggle.
--
-- @rawText@ - current raw text content
-- @previewing@ - whether preview mode is active
-- @onTextChange@ - action constructor for text changes
-- @onToggle@ - action for toggling edit/preview
-- @minHeight@ - minimum height CSS value (e.g., "150px")
markdownEditorView
  :: Text
  -- ^ Current raw text content
  -> Bool
  -- ^ True = preview mode, False = editing mode
  -> (Text -> action)
  -- ^ Action constructor for text changes
  -> action
  -- ^ Action for toggling edit/preview mode
  -> Text
  -- ^ Minimum height CSS value
  -> M.View model action
markdownEditorView rawText previewing onTextChange onToggle minHeight =
  MH.div_
    [class_ "w-full"]
    [ -- Toggle bar
      MH.div_
        [class_ "flex items-center justify-end mb-1"]
        [ Button.ghostSm $
            Button.button
              (if previewing then ("Edit" :: M.MisoString) else "Preview")
              (Just onToggle)
        ]
    , -- Content area
      if previewing
        then previewView rawText minHeight
        else editingView rawText onTextChange minHeight
    ]

-- | Editing view: textarea with optional parse error below
editingView :: Text -> (Text -> action) -> Text -> M.View model action
editingView rawText onTextChange minHeight =
  let hasError = checkParseError rawText
   in MH.div_
        []
        [ MH.textarea_
            [ class_ $
                "w-full px-3 py-2 border rounded-md bg-background font-mono text-sm resize-y"
                  <> (if hasError then " border-red-300" else " border-input")
            , MP.value_ (ms rawText)
            , MH.onInput (onTextChange . M.fromMisoString)
            , MC.style_ [("min-height", ms minHeight)]
            ]
            []
        , -- Show parse error below textarea
          if hasError
            then parseErrorBox rawText
            else M.text ""
        ]

-- | Preview view: rendered markdown
previewView :: Text -> Text -> M.View model action
previewView rawText minHeight =
  MH.div_
    [ class_ "p-3 border border-input rounded-md bg-muted/50 overflow-auto"
    , MC.style_ [("min-height", ms minHeight)]
    ]
    [ renderRichText (fromTrustedInput rawText)
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
