-- |
-- Module      : Competences.Markdown.Parser.Inline
-- Description : Inline-level markdown parsers
--
-- Parses inline elements: plain text, emphasis, strong, code spans,
-- inline math, links, file embeds (with optional style attributes),
-- and line breaks.
module Competences.Markdown.Parser.Inline
  ( inlinesP
  , inlineP
  , lineInlinesP
  )
where

import Competences.Markdown.AST
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Char qualified
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
    , clozeBlankP
    , fileEmbedP
    , linkP
    , plainP
    ]

-- | Parse a sequence of inline elements restricted to a single line.
-- Like 'inlinesP' but without line-break parsers, so parsing stops at newline.
lineInlinesP :: Parser [Inline]
lineInlinesP = some lineInlineP
  where
    lineInlineP =
      choice
        [ strongP
        , emphP
        , codeSpanP
        , mathInlineParenP
        , mathInlineP
        , clozeBlankP
        , fileEmbedP
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
      notFollowedBy (try bulletListMarker)
      notFollowedBy (try letterListMarker)
      notFollowedBy (try numberListMarker)
      pure ()

    bulletListMarker = do
      _ <- oneOf ("-*+" :: String)
      void $ char ' '

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
    , clozeBlankP
    , fileEmbedP
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
    , clozeBlankP
    , fileEmbedP
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

-- | File embed: ![alt](url) or ![alt](url "title") with optional {thumb=size}
fileEmbedP :: Parser Inline
fileEmbedP = do
  _ <- try (char '!' *> char '[')
  content <- manyTill linkInlineP (char ']')
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
  (imgSize, imgPos, backdrop) <- imageStyleAttrP
  pure $ FileEmbed url' content title imgSize imgPos backdrop

-- | Accumulated image attributes during parsing.
data ImageAttr = AttrSize !ImageSize | AttrPos !ImagePosition | AttrBackdrop !(Set BackdropContext)

-- | Parse optional {attr ...} block for image style attributes.
-- Attributes are space-separated, order-independent.
-- Defaults: ExactSize, Centered, empty backdrop.
imageStyleAttrP :: Parser (ImageSize, ImagePosition, Set BackdropContext)
imageStyleAttrP = do
  mAttrs <- optional $ do
    _ <- char '{'
    hspace
    attrs <- many (singleAttrP <* hspace)
    _ <- char '}'
    pure attrs
  let attrs = concat mAttrs
      size = lastMay [s | AttrSize s <- attrs]
      pos = lastMay [p | AttrPos p <- attrs]
      bd = mconcat [b | AttrBackdrop b <- attrs]
  pure (fromMaybe ExactSize size, fromMaybe Centered pos, bd)
  where
    lastMay [] = Nothing
    lastMay xs = Just (last xs)

-- | Parse a single image attribute (size, position, or backdrop).
singleAttrP :: Parser ImageAttr
singleAttrP =
  (AttrSize <$> sizeAttrP)
    <|> (AttrPos <$> posAttrP)
    <|> (AttrBackdrop <$> backdropAttrP)
  where
    sizeAttrP =
      (ExactSize <$ string "exact")
        <|> do
          _ <- string "thumb="
          Thumb
            <$> ( (ThumbSmall <$ string "small")
                    <|> (ThumbMedium <$ string "medium")
                    <|> (ThumbLarge <$ string "large")
                )
    posAttrP =
      (Centered <$ string "center")
        <|> do
          _ <- string "float="
          (FloatLeft <$ string "left") <|> (FloatRight <$ string "right")
    backdropAttrP = do
      _ <- string "backdrop"
      ctxs <-
        optional $
          char '=' *> do
            (allContexts <$ string "always") <|> contextListP
      pure $ fromMaybe allContexts ctxs
    contextListP = do
      first <- contextP
      rest <- many (char ',' *> contextP)
      pure $ Set.fromList (first : rest)
    contextP =
      (BackdropPrint <$ string "print")
        <|> (BackdropThumb <$ string "thumb")
        <|> (BackdropFull <$ string "full")
    allContexts = Set.fromList [BackdropPrint, BackdropThumb, BackdropFull]

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
    , clozeBlankP
    , fileEmbedP
    , plainInDelimP "]"
    ]

-- | Plain text inside delimiters (stops at delimiter chars and special chars)
plainInDelimP :: Text -> Parser Inline
plainInDelimP extra = Plain <$> takeWhile1P (Just "text") isPlainInDelim
  where
    isPlainInDelim c =
      c /= '*' && c /= '$' && c /= '`' && c /= '[' && c /= '!' && c /= '\\' && c /= '\n'
        && not (T.any (== c) extra)

-- | Cloze blank: ___ or ___N___ where N is a decimal (cm, half-cm steps)
-- Stored as millimeters: ___2___ → Just 20, ___1.5___ → Just 15
clozeBlankP :: Parser Inline
clozeBlankP = do
  _ <- try (string "___")
  -- Try to parse a width number followed by ___
  mWidth <- optional $ try $ do
    intPart <- takeWhile1P (Just "digit") (\c -> c >= '0' && c <= '9')
    fracPart <- optional $ try $ do
      _ <- char '.'
      d <- satisfy (\c -> c >= '0' && c <= '9')
      pure d
    _ <- string "___"
    let mm = read (T.unpack intPart) * 10
    pure $ case fracPart of
      Nothing -> mm
      Just d -> mm + (Data.Char.ord d - Data.Char.ord '0')
  pure $ ClozeBlank mWidth

-- | Plain text (everything that's not a special marker)
plainP :: Parser Inline
plainP = Plain <$> (plainChunk <|> singleSpecial)
  where
    plainChunk = takeWhile1P (Just "text") isPlainChar

    -- Consume a single special character that didn't match any other parser
    singleSpecial = do
      notFollowedBy (void newline)
      notFollowedBy (void $ string "___")
      T.singleton <$> anySingle

    isPlainChar c =
      c /= '*' && c /= '$' && c /= '`' && c /= '[' && c /= '!' && c /= '\\' && c /= '\n'
        && c /= '_'
