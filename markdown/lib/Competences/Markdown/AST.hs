-- |
-- Module      : Competences.Markdown.AST
-- Description : AST for CommonMark-subset markdown with extensions
--
-- Supports:
--
-- * Paragraphs (blank-line separated)
-- * Headings (@#@ through @######@)
-- * Emphasis (@*...*@) and strong (@**...**@)
-- * Inline code (@`...`@)
-- * Inline math (@$...$@) and display math (@$$...$$@)
-- * Links (@[text](url)@ and @[text](url "title")@)
-- * Fenced code blocks (with optional info string)
-- * Ordered lists (@1. 2. 3.@)
-- * Lettered lists (@a. b. c.@ extension)
-- * Thematic breaks (@---@ or @***@)
-- * Soft and hard line breaks
module Competences.Markdown.AST
  ( -- * Document structure
    Document (..)
  , Block (..)
  , Inline (..)
  , Url
  )
where

import Data.Text (Text)

-- | Root document type - a sequence of blocks
newtype Document = Document [Block]
  deriving (Eq, Show)

-- | Block-level elements
data Block
  = -- | Regular paragraph containing inline elements
    Paragraph ![Inline]
  | -- | Heading with level (1-6) and inline content
    Heading !Int ![Inline]
  | -- | Fenced code block with optional info string and body
    FencedCodeBlock !(Maybe Text) !Text
  | -- | Ordered list with start number. Each item is a list of blocks.
    OrderedList !Int ![[Block]]
  | -- | Lettered list (a. b. c. extension). Each item is a list of blocks.
    LetterList ![[Block]]
  | -- | Display math block ($$...$$ or \[...\])
    MathBlock !Text
  | -- | Thematic break (--- or ***)
    ThematicBreak
  deriving (Eq, Show)

-- | Inline elements within paragraphs and list items
data Inline
  = -- | Regular text
    Plain !Text
  | -- | Emphasized text (*...*)
    Emph ![Inline]
  | -- | Strong/bold text (**...**)
    Strong ![Inline]
  | -- | Inline code (`...`)
    Code !Text
  | -- | Inline math ($...$)
    MathInline !Text
  | -- | Link [content](url) or [content](url "title")
    Link !Url ![Inline] !(Maybe Text)
  | -- | Soft line break (single newline within paragraph)
    SoftLineBreak
  | -- | Hard line break (trailing \\ or two spaces before newline)
    HardLineBreak
  deriving (Eq, Show)

-- | URL type alias
type Url = Text
