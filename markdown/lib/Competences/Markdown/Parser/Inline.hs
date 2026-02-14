-- |
-- Module      : Competences.Markdown.Parser.Inline
-- Description : Inline-level markdown parsers
--
-- Parses inline elements: plain text, emphasis, strong, code spans,
-- inline math, links, and line breaks.
module Competences.Markdown.Parser.Inline
  ( inlinesP
  , inlineP
  )
where

import Competences.Markdown.AST
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

-- | Parse a sequence of inline elements (at least one)
inlinesP :: Parser [Inline]
inlinesP = some inlineP

-- | Parse a single inline element
inlineP :: Parser Inline
inlineP =
  choice
    [ hardLineBreakP
    , softLineBreakP
    , strongP
    , emphP
    , codeSpanP
    , mathInlineParenP
    , mathInlineP
    , linkP
    , plainP
    ]

-- | Hard line break: backslash + newline, or two+ spaces + newline
hardLineBreakP :: Parser Inline
hardLineBreakP =
  HardLineBreak
    <$ try
      ( choice
          [ void (char '\\' *> newline)
          , void (count 2 (char ' ') *> skipMany (char ' ') *> newline)
          ]
      )

-- | Soft line break: single newline that doesn't start a new block
softLineBreakP :: Parser Inline
softLineBreakP = SoftLineBreak <$ try softNewline
  where
    softNewline = do
      _ <- newline
      -- Don't consume newline if next line looks like a block start
      notFollowedBy (void newline) -- blank line = paragraph break
      notFollowedBy (void $ char '#') -- heading
      notFollowedBy (void $ string "$$") -- math block
      notFollowedBy (void $ string "\\[") -- math block alt
      notFollowedBy (void $ string "---") -- thematic break
      notFollowedBy (void $ string "***") -- thematic break
      notFollowedBy (void $ string "```") -- fenced code
      notFollowedBy (void $ string "~~~") -- fenced code
      -- Don't consume if it looks like an admonition start
      notFollowedBy (try $ char '>' *> hspace *> string "[!")
      -- Don't consume if it looks like a list item at start of line
      notFollowedBy (try letterListMarker)
      notFollowedBy (try numberListMarker)
      pure ()

    letterListMarker = do
      _ <- lowerChar
      void $ char '.'
      void $ char ' '

    numberListMarker = do
      _ <- some digitChar
      void $ char '.'
      void $ char ' '

-- | Strong emphasis: **...**
strongP :: Parser Inline
strongP = do
  _ <- try (string "**")
  content <- someTill inlineInStrongP (string "**")
  pure $ Strong content

-- | Emphasis: *...*
emphP :: Parser Inline
emphP = do
  _ <- try $ do
    _ <- char '*'
    notFollowedBy (char '*') -- not **
    pure ()
  content <- someTill inlineInEmphP (char '*')
  pure $ Emph content

-- | Inline elements allowed inside strong delimiters
inlineInStrongP :: Parser Inline
inlineInStrongP =
  choice
    [ emphP
    , codeSpanP
    , mathInlineP
    , linkP
    , plainInDelimP "*"
    ]

-- | Inline elements allowed inside emph delimiters
inlineInEmphP :: Parser Inline
inlineInEmphP =
  choice
    [ strongP
    , codeSpanP
    , mathInlineP
    , linkP
    , plainInDelimP "*"
    ]

-- | Code span: `...`
codeSpanP :: Parser Inline
codeSpanP = do
  _ <- char '`'
  notFollowedBy (char '`') -- not a fenced code block
  content <- takeWhileP (Just "code content") (/= '`')
  _ <- char '`'
  pure $ Code content

-- | Inline math: $...$
mathInlineP :: Parser Inline
mathInlineP = do
  _ <- try $ do
    _ <- char '$'
    notFollowedBy (char '$') -- not $$
    pure ()
  content <- takeWhile1P (Just "math content") (\c -> c /= '$' && c /= '\n')
  _ <- char '$'
  pure $ MathInline content

-- | Inline math with \(...\) delimiters
mathInlineParenP :: Parser Inline
mathInlineParenP = do
  _ <- try (string "\\(")
  content <- manyTill anySingle (string "\\)")
  pure $ MathInline (T.pack content)

-- | Link: [text](url) or [text](url "title")
linkP :: Parser Inline
linkP = do
  _ <- try (char '[')
  content <- someTill linkInlineP (char ']')
  _ <- char '('
  hspace
  url' <- takeWhileP (Just "URL") (\c -> c /= ')' && c /= ' ' && c /= '"')
  title <- optional $ do
    hspace
    _ <- char '"'
    t <- takeWhileP (Just "title") (/= '"')
    _ <- char '"'
    pure t
  hspace
  _ <- char ')'
  pure $ Link url' content title

-- | Inline elements allowed inside link text
linkInlineP :: Parser Inline
linkInlineP =
  choice
    [ strongP
    , emphP
    , codeSpanP
    , mathInlineP
    , plainInDelimP "]"
    ]

-- | Plain text inside delimiters (stops at delimiter chars and special chars)
plainInDelimP :: Text -> Parser Inline
plainInDelimP extra = Plain <$> takeWhile1P (Just "text") isPlainInDelim
  where
    isPlainInDelim c =
      c /= '*' && c /= '$' && c /= '`' && c /= '[' && c /= '\\' && c /= '\n'
        && not (T.any (== c) extra)

-- | Plain text (everything that's not a special marker)
plainP :: Parser Inline
plainP = Plain <$> (plainChunk <|> singleSpecial)
  where
    plainChunk = takeWhile1P (Just "text") isPlainChar

    -- Consume a single special character that didn't match any other parser
    singleSpecial = do
      notFollowedBy (void newline)
      T.singleton <$> anySingle

    isPlainChar c =
      c /= '*' && c /= '$' && c /= '`' && c /= '[' && c /= '\\' && c /= '\n'
