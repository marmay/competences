{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.TaskContentView
-- Description : Renderer component for task content markup
--
-- Renders parsed 'TaskContent' AST to Miso views, using MathJax
-- for inline and block math rendering.
--
-- Usage:
--
-- @
-- import Competences.TaskContent.Parser (parseTaskContent)
-- import Competences.Frontend.Component.TaskContentView (taskContentView, renderTaskContentText)
--
-- -- Parse and render in one step
-- renderTaskContentText "Solve $x + 1 = 2$"
--
-- -- Or parse separately
-- case parseTaskContent content of
--   Left err -> -- show error
--   Right ast -> taskContentView ast
-- @
module Competences.Frontend.Component.TaskContentView
  ( -- * Rendering functions
    taskContentView
  , renderTaskContentText
  , renderBlock
  , renderInline
  )
where

import Competences.Frontend.Component.MathJax (MathDisplay (..), mathJaxView)
import Competences.Frontend.View.Tailwind (class_)
import Competences.TaskContent.AST
import Competences.TaskContent.Parser (parseTaskContent)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)

-- | Render parsed TaskContent AST to Miso view
--
-- Creates a container div with appropriate spacing between blocks.
taskContentView :: TaskContent -> M.View p a
taskContentView (TaskContent blocks) =
  M.div_ [class_ "task-content space-y-4"] $
    map renderBlock blocks

-- | Convenience function to parse and render text in one step
--
-- On parse failure, shows the raw text in a code block with error styling.
renderTaskContentText :: Text -> M.View p a
renderTaskContentText content =
  case parseTaskContent content of
    Left _err ->
      -- Parse error - show raw text as fallback
      M.pre_
        [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
        [M.text (ms content)]
    Right ast ->
      taskContentView ast

-- | Render a single block element
renderBlock :: Block -> M.View p a
renderBlock = \case
  Paragraph inlines ->
    M.p_ [class_ "text-stone-800 leading-relaxed"] $
      map renderInline inlines
  SubTaskList items ->
    -- Lettered list: a., b., c.
    M.ol_
      [class_ "list-[lower-alpha] ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map renderListItem items
  SubQuestionList items ->
    -- Numbered list: 1., 2., 3.
    M.ol_
      [class_ "list-decimal ml-6 space-y-2 marker:font-medium marker:text-stone-600"]
      $ map renderListItem items
  MathBlock latex ->
    -- Display math (centered, block-level)
    mathJaxView Block latex

-- | Render a list item
renderListItem :: ListItem -> M.View p a
renderListItem item =
  M.li_ [class_ "text-stone-800 leading-relaxed pl-1"] $
    map renderInline item.content

-- | Render an inline element
renderInline :: Inline -> M.View p a
renderInline = \case
  Plain text ->
    M.text (ms text)
  Emph inlines ->
    M.em_ [class_ "italic"] $ map renderInline inlines
  Strong inlines ->
    M.strong_ [class_ "font-semibold"] $ map renderInline inlines
  MathInline latex ->
    -- Inline math
    mathJaxView Inline latex
