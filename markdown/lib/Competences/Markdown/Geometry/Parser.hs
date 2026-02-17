-- |
-- Module      : Competences.Markdown.Geometry.Parser
-- Description : Parser for the geometry DSL (V1)
--
-- Keyword-dispatch parser. The @labeled@ suffix is desugared here into
-- separate 'Draw' + 'Label' commands — the AST and evaluator never see it.
--
-- @
-- defPoint A (0, 0)
-- defPointBy M (midpoint A B)
-- defSegment c A -- B
-- drawPoint A labeled "A" below-left
-- drawSegment A -- B labeled "c" below 0.4
-- axes {
--   dashed { drawSegment M -- C }
-- }
-- @
module Competences.Markdown.Geometry.Parser
  ( parseGeometry

    -- * Version helpers
  , currentGeometryVersion
  , parseGeometryVersion
  , isGeometryInfo
  , geometryVersionText
  )
where

import Competences.Markdown.Geometry.AST
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read (readMaybe)

type Parser = Parsec Void Text

-- | Parse geometry DSL text into a list of commands
parseGeometry :: Text -> Either (ParseErrorBundle Text Void) [Command]
parseGeometry input
  | T.null (T.strip input) = Right []
  | otherwise = parse (commandsP <* ws <* eof) "geometry" input

-- -----------------------------------------------------------------
-- Version helpers
-- -----------------------------------------------------------------

-- | The current geometry DSL version supported by the parser/renderer.
currentGeometryVersion :: GeometryVersion
currentGeometryVersion = (1, 0)

-- | Parse a version string like @"V1.0"@ into a 'GeometryVersion'.
parseGeometryVersion :: Text -> Maybe GeometryVersion
parseGeometryVersion t = case T.uncons t of
  Just ('V', rest) -> case T.splitOn "." rest of
    [majT, minT] -> do
      maj <- readMaybe (T.unpack majT)
      mn <- readMaybe (T.unpack minT)
      Just (maj, mn)
    _ -> Nothing
  _ -> Nothing

-- | Check whether a fenced code block info string indicates a geometry block.
-- The info string is @"geometry"@ optionally followed by a version tag.
isGeometryInfo :: Text -> Bool
isGeometryInfo t =
  let w = T.strip t
   in w == "geometry" || "geometry " `T.isPrefixOf` w

-- | Extract the version text (e.g. @"V1.0"@) from an info string like
-- @"geometry V1.0"@. Returns 'Nothing' for plain @"geometry"@.
geometryVersionText :: Text -> Maybe Text
geometryVersionText t =
  let w = T.strip t
   in case T.stripPrefix "geometry " w of
        Nothing -> Nothing
        Just rest ->
          let v = T.strip rest
           in if T.null v then Nothing else Just v

-- | Parse zero or more commands (top-level or inside a block).
-- Uses 'try' so that whitespace consumed before a failed command attempt
-- is rolled back, allowing the caller to see @}@ or EOF.
commandsP :: Parser [Command]
commandsP = concat <$> many (try (ws *> commandP))

-- | Parse a single command. Returns a list because @labeled@ desugars to two.
commandP :: Parser [Command]
commandP = do
  kw <- lexeme keywordP
  case kw of
    "defPoint" -> one <$> defPointP
    "defPointBy" -> one <$> defPointByP
    "defSegment" -> one <$> defSegmentP
    "drawPoint" -> drawPointP
    "drawSegment" -> drawSegmentP
    "labelPoint" -> one <$> labelPointP
    "labelSegment" -> one <$> labelSegmentP
    -- Modifier blocks
    "color" -> one <$> colorBlockP
    "dashed" -> one <$> modifierBlockP (EnvMod SetDashed)
    "thick" -> one <$> modifierBlockP (EnvMod SetThick)
    "thin" -> one <$> modifierBlockP (EnvMod SetThin)
    "axes" -> one <$> modifierBlockP (AutoDec Axes)
    "grid" -> one <$> modifierBlockP (AutoDec Grid)
    "labelAll" -> one <$> labelAllBlockP
    "background" -> one <$> modifierBlockP (LayerMod Background)
    "foreground" -> one <$> modifierBlockP (LayerMod Foreground)
    _ -> fail $ "Unknown command: " <> T.unpack kw
  where
    one x = [x]

-- -----------------------------------------------------------------
-- Definition commands
-- -----------------------------------------------------------------

-- | @defPoint A (x, y)@
defPointP :: Parser Command
defPointP = do
  name <- lexeme nameP
  vec <- vec2P
  pure $ DefPoint name vec

-- | @defPointBy M (construction ...)@
defPointByP :: Parser Command
defPointByP = do
  name <- lexeme nameP
  constr <- constructionP
  pure $ DefPointBy name constr

-- | @defSegment s A -- B@
defSegmentP :: Parser Command
defSegmentP = do
  name <- lexeme nameP
  a <- lexeme nameP
  _ <- lexeme (string "--")
  b <- lexeme nameP
  pure $ DefSegment name a b

-- -----------------------------------------------------------------
-- Draw commands (with optional labeled suffix)
-- -----------------------------------------------------------------

-- | @drawPoint A@ or @drawPoint A labeled "A" below-left@
drawPointP :: Parser [Command]
drawPointP = do
  name <- lexeme nameP
  mLabel <- optional (lexeme (string "labeled") *> pointLabelTailP name)
  pure $ [Draw (DrawPoint name)] <> maybe [] (\lbl -> [Label lbl]) mLabel

-- | @drawSegment s@ or @drawSegment A -- B@, with optional @labeled@
drawSegmentP :: Parser [Command]
drawSegmentP = do
  name1 <- lexeme nameP
  segRef <-
    (lexeme (string "--") *> (SegInline name1 <$> lexeme nameP))
      <|> pure (SegByName name1)
  mLabel <- optional (lexeme (string "labeled") *> segmentLabelTailP segRef)
  pure $ [Draw (DrawSegment segRef)] <> maybe [] (\lbl -> [Label lbl]) mLabel

-- -----------------------------------------------------------------
-- Label commands
-- -----------------------------------------------------------------

-- | @labelPoint A "text" above@
labelPointP :: Parser Command
labelPointP = do
  name <- lexeme nameP
  lbl <- pointLabelTailP name
  pure $ Label lbl

-- | @labelSegment s "text" above 0.4@ or @labelSegment A -- B "text" above@
labelSegmentP :: Parser Command
labelSegmentP = do
  name1 <- lexeme nameP
  segRef <-
    (lexeme (string "--") *> (SegInline name1 <$> lexeme nameP))
      <|> pure (SegByName name1)
  lbl <- segmentLabelTailP segRef
  pure $ Label lbl

-- -----------------------------------------------------------------
-- Label tail parsers (shared by draw-labeled and label commands)
-- -----------------------------------------------------------------

-- | Parse @"text" position@ for a point label
pointLabelTailP :: Name -> Parser LabelPrimitive
pointLabelTailP name = do
  txt <- lexeme quotedTextP
  pos <- lexeme labelPositionP
  pure $ LabelAtPoint name txt pos

-- | Parse @"text" side [fraction]@ for a segment label
segmentLabelTailP :: SegmentRef -> Parser LabelPrimitive
segmentLabelTailP ref = do
  txt <- lexeme quotedTextP
  side <- lexeme segmentSideP
  frac <- option 0.5 (lexeme doubleP)
  pure $ LabelOnSegment ref txt side frac

-- -----------------------------------------------------------------
-- Point constructions
-- -----------------------------------------------------------------

-- | Parse a parenthesized point construction
constructionP :: Parser PointConstruction
constructionP = between (lexeme (char '(')) (char ')') innerConstructionP

innerConstructionP :: Parser PointConstruction
innerConstructionP = do
  kw <- lexeme keywordP
  case kw of
    "midpoint" -> do
      a <- lexeme nameP
      b <- lexeme nameP
      pure $ Midpoint a b
    "lerp" -> do
      a <- lexeme nameP
      b <- lexeme nameP
      t <- lexeme doubleP
      pure $ Lerp a b t
    "rotate" -> do
      center <- lexeme nameP
      degrees <- lexeme doubleP
      p <- lexeme nameP
      pure $ Rotate center degrees p
    "reflect" -> do
      ref <- lexeme lineRefP
      p <- lexeme nameP
      pure $ Reflect ref p
    "translate" -> do
      v <- lexeme vec2P
      p <- lexeme nameP
      pure $ Translate v p
    _ -> fail $ "Unknown construction: " <> T.unpack kw

-- | Parse @(line A B)@
lineRefP :: Parser LineRef
lineRefP = between (lexeme (char '(')) (lexeme (char ')')) $ do
  _ <- lexeme (string "line")
  a <- lexeme nameP
  b <- lexeme nameP
  pure $ LineThrough a b

-- -----------------------------------------------------------------
-- Modifier blocks
-- -----------------------------------------------------------------

-- | @color <name> { ... }@
colorBlockP :: Parser Command
colorBlockP = do
  name <- lexeme nameP
  modifierBlockP (EnvMod (SetColor (NamedColor name)))

-- | @labelAll <position> { ... }@
labelAllBlockP :: Parser Command
labelAllBlockP = do
  pos <- lexeme labelPositionP
  modifierBlockP (AutoDec (LabelAll pos))

-- | Parse @{ commands }@ with a given modifier
modifierBlockP :: Modifier -> Parser Command
modifierBlockP modifier = do
  _ <- lexeme (char '{')
  cmds <- commandsP
  ws
  _ <- char '}'
  pure $ ModifierBlock modifier cmds

-- -----------------------------------------------------------------
-- Primitives
-- -----------------------------------------------------------------

-- | Parse a parenthesized (x, y) coordinate
vec2P :: Parser Vec2
vec2P = between (lexeme (char '(')) (char ')') $ do
  x <- lexeme doubleP
  _ <- lexeme (char ',')
  y <- lexeme doubleP
  pure $ Vec2 x y

-- | Keyword: sequence of alphanumeric characters (no underscores — those are names)
keywordP :: Parser Text
keywordP = T.pack <$> some alphaNumChar

-- | Name: alphanumeric + underscores
nameP :: Parser Name
nameP = T.pack <$> some (alphaNumChar <|> char '_')

-- | Quoted string: @"text"@
quotedTextP :: Parser Text
quotedTextP = do
  _ <- char '"'
  txt <- takeWhileP (Just "label text") (/= '"')
  _ <- char '"'
  pure txt

-- | Double literal (signed, supports both integer and decimal notation)
doubleP :: Parser Double
doubleP = L.signed hspace (try L.float <|> (fromIntegral <$> (L.decimal :: Parser Int)))

labelPositionP :: Parser LabelPosition
labelPositionP =
  choice
    [ AboveLeft <$ string "above-left"
    , AboveRight <$ string "above-right"
    , BelowLeft <$ string "below-left"
    , BelowRight <$ string "below-right"
    , Above <$ string "above"
    , Below <$ string "below"
    , LeftOf <$ string "left"
    , RightOf <$ string "right"
    ]

segmentSideP :: Parser SegmentSide
segmentSideP =
  choice
    [ SegAbove <$ string "above"
    , SegBelow <$ string "below"
    ]

-- | Consume horizontal whitespace + newlines (used between commands)
ws :: Parser ()
ws = L.space space1 empty empty

-- | Consume trailing horizontal whitespace after a token
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)
