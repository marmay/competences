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
-- * Bullet lists (@- item@, @* item@, @+ item@)
-- * Lettered lists (@a. b. c.@ extension)
-- * Thematic breaks (@---@ or @***@)
-- * Soft and hard line breaks
-- * Admonition blocks (definition, theorem, proof, etc.)
module Competences.Markdown.AST
  ( -- * Document structure
    Document (..)
  , Block (..)
  , Inline (..)
  , AdmonitionType (..)
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
  | -- | Bullet list (-, *, + markers). Each item is a list of blocks.
    BulletList ![[Block]]
  | -- | Lettered list (a. b. c. extension). Each item is a list of blocks.
    LetterList ![[Block]]
  | -- | Display math block ($$...$$ or \[...\])
    MathBlock !Text
  | -- | Thematic break (--- or ***)
    ThematicBreak
  | -- | Admonition block with type, optional title, and body blocks
    --   > [!theorem] Title text
    --   > Body paragraph...
    Admonition !AdmonitionType !(Maybe [Inline]) ![Block]
  | -- | BTC notes grid (2×2): four cells of block-level content.
    --   Cells: top-left, top-right, bottom-left, bottom-right.
    NotesGrid ![Block] ![Block] ![Block] ![Block]
  deriving (Eq, Show)

-- | Admonition types for math content callouts
data AdmonitionType
  = Definition
  | Theorem
  | Lemma
  | Proof
  | -- | Additional info or context ("Bemerkung")
    Remark
  | -- | Key takeaway — "Remember this!"
    Merksatz
  | Example
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
  | -- | File embed ![caption](file:name) or ![caption](fileIdx:N)
    FileEmbed !Url ![Inline] !(Maybe Text)
  | -- | Soft line break (single newline within paragraph)
    SoftLineBreak
  | -- | Hard line break (trailing \\ or two spaces before newline)
    HardLineBreak
  deriving (Eq, Show)

-- | URL type alias
type Url = Text
