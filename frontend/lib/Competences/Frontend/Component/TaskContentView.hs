{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.TaskContentView
-- Description : Renderer component for task content markup
--
-- Renders parsed 'TaskContent' AST to Miso views, using RichContent
-- for math rendering with SVG references (avoids DOM conflicts).
--
-- Usage:
--
-- @
-- import Competences.TaskContent.Parser (parseTaskContent)
-- import Competences.Frontend.Component.TaskContentView (taskContentView, renderRichText)
--
-- -- Parse and render in one step
-- renderRichText "Solve $x + 1 = 2$"
--
-- -- Or parse separately
-- case parseTaskContent content of
--   Left err -> -- show error
--   Right ast -> taskContentView ast
-- @
module Competences.Frontend.Component.TaskContentView
  ( -- * Rendering functions
    taskContentView
  , renderRichText
  )
where

import Competences.Frontend.Component.RichContent (richContentView)
import Competences.Frontend.View.Tailwind (class_)
import Competences.TaskContent.AST (TaskContent (..))
import Competences.TaskContent.Parser (parseTaskContent)
import Data.Bits (xor, (.&.))
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Numeric (showHex)

-- | Render parsed TaskContent AST to Miso view
--
-- Uses RichContent component which renders math formulas to a hidden
-- SVG container and references them via <use> elements.
taskContentView :: TaskContent -> M.View p a
taskContentView ast =
  -- Generate a stable key from the content for component identity
  let key = hashContent ast
   in richContentView key ast

-- | Convenience function to parse and render text in one step
--
-- On parse failure, shows the raw text in a code block with error styling.
renderRichText :: Text -> M.View p a
renderRichText content =
  case parseTaskContent content of
    Left _err ->
      -- Parse error - show raw text as fallback
      M.pre_
        [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
        [M.text (ms content)]
    Right ast ->
      taskContentView ast

-- | Generate a stable hash key from TaskContent
-- Uses DJB2-like hash (works on 32-bit WASM)
hashContent :: TaskContent -> Text
hashContent (TaskContent blocks) =
  let str = show blocks
      djb2Hash = foldl' (\h c -> ((h * 33) `xor` ord c) .&. 0x7FFFFFFF) 5381 str
   in "tc-" <> T.pack (showHex djb2Hash "")
