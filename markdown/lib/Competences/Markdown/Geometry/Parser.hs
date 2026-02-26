-- |
-- Module      : Competences.Markdown.Geometry.Parser
-- Description : Parser for the geometry DSL (V1)
--
-- Keyword-dispatch parser. The @labeled@ suffix is desugared here into
-- separate 'Draw' + 'Label' commands — the AST and evaluator never see it.
--
-- Modifiers use the @\@@ prefix for unambiguous parsing:
--
-- @
-- defPoint A (0, 0)
-- defPointBy M (midpoint A B)
-- defSegment c A -- B
-- drawPoint A labeled "A" below-left
-- drawSegment A -- B labeled "c" below 0.4
-- \@axes {
--   \@dashed { drawSegment M -- C }
-- }
-- @
module Competences.Markdown.Geometry.Parser
  ( parseGeometry
  , parseLabelContent

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
-- Commands starting with @\@@ are modifier blocks; everything else is a plain command.
commandP :: Parser [Command]
commandP =
  (one <$> modifierCommandP)
    <|> plainCommandP
  where
    one x = [x]

-- | Parse @\@modifier { body }@
modifierCommandP :: Parser Command
modifierCommandP = do
  _ <- lexeme (char '@')
  modifier <- modifierValueP
  modifierBlockP modifier

-- | Parse a plain (non-modifier) command by keyword dispatch.
plainCommandP :: Parser [Command]
plainCommandP = do
  kw <- lexeme keywordP
  case kw of
    "defPoint" -> one <$> defPointP
    "defPointBy" -> one <$> defPointByP
    "defSegment" -> one <$> defSegmentP
    "drawPoint" -> drawPointP
    "drawSegment" -> drawSegmentP
    "drawAngle" -> drawAngleP
    "drawRightAngle" -> one . Draw . DrawRightAngle <$> angleRefP
    "labelPoint" -> one <$> labelPointP
    "labelSegment" -> one <$> labelSegmentP
    "labelAngle" -> one <$> labelAngleP
    "drawPoly" -> drawPolyP
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
-- Angle commands
-- -----------------------------------------------------------------

-- | Parse three point names as an 'AngleRef'
angleRefP :: Parser AngleRef
angleRefP = do
  a <- lexeme nameP
  b <- lexeme nameP
  c <- lexeme nameP
  pure $ AngleRef a b c

-- | @drawAngle A B C@ or @drawAngle A B C labeled "$\alpha$"@
drawAngleP :: Parser [Command]
drawAngleP = do
  ref <- angleRefP
  mLabel <- optional (lexeme (string "labeled") *> angleLabelTailP ref)
  pure $ [Draw (DrawAngle ref)] <> maybe [] (\lbl -> [Label lbl]) mLabel

-- | Parse @"text"@ for an angle label (no position needed — auto-placed at bisector).
-- Optionally followed by @+(dx, dy)@ for external label placement with leader line.
angleLabelTailP :: AngleRef -> Parser LabelPrimitive
angleLabelTailP ref = do
  txt <- lexeme quotedTextP
  mOffset <- optional offsetP
  pure $ LabelAngle ref (parseLabelContent txt) mOffset

-- | Parse @+(dx, dy)@ offset for external label placement
offsetP :: Parser Vec2
offsetP = do
  _ <- lexeme (char '+')
  _ <- lexeme (char '(')
  x <- lexeme doubleP
  _ <- lexeme (char ',')
  y <- lexeme doubleP
  _ <- lexeme (char ')')
  pure $ Vec2 x y

-- | @labelAngle A B C "$\alpha$"@
labelAngleP :: Parser Command
labelAngleP = do
  ref <- angleRefP
  lbl <- angleLabelTailP ref
  pure $ Label lbl

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
  pure $ LabelAtPoint name (parseLabelContent txt) pos

-- | Parse @"text" side [fraction]@ for a segment label
segmentLabelTailP :: SegmentRef -> Parser LabelPrimitive
segmentLabelTailP ref = do
  txt <- lexeme quotedTextP
  side <- lexeme segmentSideP
  frac <- option 0.5 (lexeme doubleP)
  pure $ LabelOnSegment ref (parseLabelContent txt) side frac

-- | Classify quoted text as plain or math label.
-- Text wrapped in @$...$@ (non-empty) is treated as LaTeX math.
parseLabelContent :: Text -> LabelContent
parseLabelContent txt =
  case T.stripPrefix "$" txt >>= T.stripSuffix "$" of
    Just latex | not (T.null latex) -> MathLabel latex
    _ -> PlainLabel txt

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

-- | Parse a modifier keyword (+ any arguments) and return a 'Modifier' value.
-- Called after the @\@@ prefix has been consumed.
modifierValueP :: Parser Modifier
modifierValueP = do
  kw <- lexeme keywordP
  case kw of
    "color" -> EnvMod . SetColor . NamedColor <$> lexeme nameP
    "fill" -> EnvMod . SetFill . NamedColor <$> lexeme nameP
    "dashed" -> pure $ EnvMod SetDashed
    "thick" -> pure $ EnvMod SetThick
    "thin" -> pure $ EnvMod SetThin
    "axes" -> pure $ AutoDec Axes
    "grid" -> pure $ AutoDec Grid
    "labelAll" -> AutoDec . LabelAll <$> lexeme labelPositionP
    "background" -> pure $ LayerMod Background
    "foreground" -> pure $ LayerMod Foreground
    "labelDist" -> EnvMod . SetLabelDist <$> lexeme doubleP
    "fontSize" -> EnvMod . SetFontSize <$> lexeme doubleP
    "dotRadius" -> EnvMod . SetDotRadius <$> lexeme doubleP
    _ -> fail $ "Unknown modifier: " <> T.unpack kw

-- | Parse @{ commands }@ with a given modifier, optionally preceded by
-- comma-separated additional @\@@-prefixed modifiers: @\@axes, \@grid { ... }@
modifierBlockP :: Modifier -> Parser Command
modifierBlockP modifier = do
  extras <- many (lexeme (char ',') *> lexeme (char '@') *> modifierValueP)
  _ <- lexeme (char '{')
  cmds <- commandsP
  ws
  _ <- char '}'
  pure $ nestModifiers (modifier : extras) cmds

-- | Nest a list of modifiers around a command body, right-associatively.
nestModifiers :: [Modifier] -> [Command] -> Command
nestModifiers [] _ = error "nestModifiers: impossible empty list"
nestModifiers [m] cmds = ModifierBlock m cmds
nestModifiers (m : ms) cmds = ModifierBlock m [nestModifiers ms cmds]

-- -----------------------------------------------------------------
-- drawPoly command
-- -----------------------------------------------------------------

-- | Internal representation of a polygon vertex (not exported)
data PolyVertex = PolyNamed !Name | PolyInline !Vec2

-- | Vertex decoration
data PolyVertexDec
  = PVPoint !(Maybe LabelContent) !(Maybe LabelPosition)
  | PVAngle !(Maybe LabelContent)
  | PVRightAngle
  | PVLabel !LabelContent !(Maybe LabelPosition)

-- | Edge decoration
data PolyEdgeDec = PESegment !LabelContent !(Maybe SegmentSide)

-- | Decoration wrapped with optional modifiers
data Decorated a = Decorated ![Modifier] !a

-- | @drawPoly vertex (edge vertex)* [edge "close"]@
drawPolyP :: Parser [Command]
drawPolyP = do
  offset <- getOffset
  -- Parse first vertex
  v0 <- lexeme polyVertexP
  decs0 <- option [] polyVertexDecsP
  -- Parse (edge vertex)* with optional close
  (edges, hasClose, closeDec) <- polyEdgesP
  let allVertices = (v0, decs0) : map snd edges
      allEdgeDecs = map fst edges
  if length allVertices < 3 && not hasClose
    then fail "drawPoly requires at least 3 vertices"
    else desugarPoly offset allVertices allEdgeDecs hasClose closeDec

-- | Parse remaining edges and vertices after the first vertex.
-- Returns: ([(edgeDec, (vertex, vertexDecs))], hasClose, maybeClosingEdgeDec)
polyEdgesP :: Parser ([(Maybe (Decorated PolyEdgeDec), (PolyVertex, [Decorated PolyVertexDec]))], Bool, Maybe (Decorated PolyEdgeDec))
polyEdgesP = go []
  where
    go acc = do
      mEdge <- optional (try polyEdgeP)
      case mEdge of
        Nothing -> pure (reverse acc, False, Nothing)
        Just edgeDec -> do
          -- Check for "close"
          mClose <- optional (try (lexeme (string "close")))
          case mClose of
            Just _ -> pure (reverse acc, True, edgeDec)
            Nothing -> do
              v <- lexeme polyVertexP
              decs <- option [] polyVertexDecsP
              go ((edgeDec, (v, decs)) : acc)

-- | Parse a polygon vertex: @(x, y)@ or a name (but not "close")
polyVertexP :: Parser PolyVertex
polyVertexP =
  (PolyInline <$> vec2P)
    <|> do
      n <- lookAhead nameP
      if n == "close"
        then fail "unexpected close"
        else PolyNamed <$> lexeme nameP

-- | Parse vertex decorations: @[point "A", \@color red { point "A" }, angle "$\alpha$"]@
polyVertexDecsP :: Parser [Decorated PolyVertexDec]
polyVertexDecsP = between (lexeme (char '[')) (lexeme (char ']')) $
  polyVertexDecItemP `sepBy1` lexeme (char ',')

-- | Parse a single vertex decoration item, optionally wrapped with @\@@-modifiers.
polyVertexDecItemP :: Parser (Decorated PolyVertexDec)
polyVertexDecItemP =
  ( do
      mods <- modifierListP
      _ <- lexeme (char '{')
      dec <- polyVertexDecPlainP
      _ <- lexeme (char '}')
      pure $ Decorated mods dec
  )
    <|> (Decorated [] <$> polyVertexDecPlainP)

-- | Parse a plain vertex decoration keyword and its arguments.
polyVertexDecPlainP :: Parser PolyVertexDec
polyVertexDecPlainP = do
  kw <- lexeme keywordP
  case kw of
    "point" -> do
      mTxt <- optional (lexeme quotedTextP)
      mPos <- optional (lexeme labelPositionP)
      pure $ PVPoint (parseLabelContent <$> mTxt) mPos
    "angle" -> do
      mTxt <- optional (lexeme quotedTextP)
      pure $ PVAngle (parseLabelContent <$> mTxt)
    "rightAngle" -> pure PVRightAngle
    "label" -> do
      txt <- lexeme quotedTextP
      mPos <- optional (lexeme labelPositionP)
      pure $ PVLabel (parseLabelContent txt) mPos
    _ -> fail $ "Unknown vertex decoration: " <> T.unpack kw

-- | Parse one or more @\@@-prefixed modifiers: @\@color red@ or @\@color red, \@thick@
modifierListP :: Parser [Modifier]
modifierListP = do
  _ <- lexeme (char '@')
  mod1 <- modifierValueP
  extras <- many (lexeme (char ',') *> lexeme (char '@') *> modifierValueP)
  pure (mod1 : extras)

-- | Parse an edge: @--@ (bare) or @-[segment "label" side]-@ or @-[\@mods { segment "label" }]-@
polyEdgeP :: Parser (Maybe (Decorated PolyEdgeDec))
polyEdgeP =
  (Nothing <$ try (lexeme (string "--") <* notFollowedBy (char '[')))
    <|> (Just <$> decoratedEdgeP)

decoratedEdgeP :: Parser (Decorated PolyEdgeDec)
decoratedEdgeP = do
  _ <- lexeme (string "-[")
  mMods <- optional (try modifierListP)
  dec <- case mMods of
    Just mods -> do
      _ <- lexeme (char '{')
      d <- edgeDecPlainP
      _ <- lexeme (char '}')
      pure $ Decorated mods d
    Nothing -> Decorated [] <$> edgeDecPlainP
  _ <- lexeme (string "]-")
  pure dec

-- | Parse a plain edge decoration keyword and its arguments.
edgeDecPlainP :: Parser PolyEdgeDec
edgeDecPlainP = do
  kw <- lexeme keywordP
  case kw of
    "segment" -> do
      txt <- lexeme quotedTextP
      mSide <- optional (lexeme segmentSideP)
      pure $ PESegment (parseLabelContent txt) mSide
    _ -> fail $ "Unknown edge decoration: " <> T.unpack kw

-- | Desugar a parsed polygon into a list of commands
desugarPoly
  :: Int
  -> [(PolyVertex, [Decorated PolyVertexDec])]
  -> [Maybe (Decorated PolyEdgeDec)]
  -> Bool
  -> Maybe (Decorated PolyEdgeDec)
  -> Parser [Command]
desugarPoly offset vertices edgeDecs _hasClose closeEdgeDec = do
  let prefix = "_p" <> T.pack (show offset)
      n = length vertices

      -- Assign names to each vertex
      vertexName :: Int -> PolyVertex -> Name
      vertexName idx (PolyInline _) = prefix <> "_v" <> T.pack (show idx)
      vertexName _ (PolyNamed name) = name

      names = [vertexName i (fst v) | (i, v) <- zip [0 ..] vertices]

      -- DefPoint commands for inline coordinates
      defPoints =
        [ DefPoint (vertexName i (PolyInline vec)) vec
        | (i, (PolyInline vec, _)) <- zip [0 ..] vertices
        ]

      -- All edge decorations including closing edge
      allEdgeDecs = edgeDecs <> [closeEdgeDec]

      -- Edge pairs: (from, to) for each edge including closing
      edgePairs = [(names !! i, names !! ((i + 1) `mod` n)) | i <- [0 .. n - 1]]

      -- DrawFilledPolygon
      fillCmd = [Draw (DrawFilledPolygon names)]

      -- Segments
      segmentCmds = concat
        [ Draw (DrawSegment (SegInline from to))
            : case allEdgeDecs !! i of
              Just (Decorated mods (PESegment lbl mSide)) ->
                wrapModifiers mods
                  [Label (LabelOnSegment (SegInline from to) lbl (maybe SegBelow id mSide) 0.5)]
              _ -> []
        | (i, (from, to)) <- zip [0 ..] edgePairs
        ]

      -- Vertex decorations
      vertexCmds = concat
        [ desugarVertexDecs (names !! i) (names !! ((i - 1 + n) `mod` n)) (names !! ((i + 1) `mod` n)) decs
        | (i, (_, decs)) <- zip [0 ..] vertices
        ]

  pure $ defPoints <> fillCmd <> segmentCmds <> vertexCmds

-- | Wrap commands in nested modifier blocks, or return them bare if no modifiers.
wrapModifiers :: [Modifier] -> [Command] -> [Command]
wrapModifiers [] cmds = cmds
wrapModifiers mods cmds = [nestModifiers mods cmds]

-- | Desugar vertex decorations into commands.
-- @prev@ is the predecessor vertex, @succ@ is the successor vertex.
desugarVertexDecs :: Name -> Name -> Name -> [Decorated PolyVertexDec] -> [Command]
desugarVertexDecs _name _prev _succ [] = []
desugarVertexDecs name prev succ_ decs = concatMap go decs
  where
    angleRef = AngleRef prev name succ_
    go (Decorated mods dec) = wrapModifiers mods (goPlain dec)
    goPlain = \case
      PVPoint mLbl mPos ->
        [Draw (DrawPoint name)]
          <> case mLbl of
            Nothing -> []
            Just lbl -> case mPos of
              Just pos -> [Label (LabelAtPoint name lbl pos)]
              Nothing -> [Label (LabelAutoPoint angleRef lbl)]
      PVAngle mLbl ->
        [Draw (DrawAngle angleRef)]
          <> maybe [] (\lbl -> [Label (LabelAngle angleRef lbl Nothing)]) mLbl
      PVRightAngle ->
        [Draw (DrawRightAngle angleRef)]
      PVLabel lbl mPos -> case mPos of
        Just pos -> [Label (LabelAtPoint name lbl pos)]
        Nothing -> [Label (LabelAutoPoint angleRef lbl)]

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
    , SegAbove <$ string "left"
    , SegBelow <$ string "below"
    , SegBelow <$ string "right"
    ]

-- | Consume horizontal whitespace + newlines (used between commands)
ws :: Parser ()
ws = L.space space1 empty empty

-- | Consume trailing horizontal whitespace after a token
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space hspace1 empty empty)
