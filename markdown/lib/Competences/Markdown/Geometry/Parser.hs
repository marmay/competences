-- |
-- Module      : Competences.Markdown.Geometry.Parser
-- Description : Parser for the geometry DSL
--
-- Parses line-oriented geometry commands from fenced code blocks.
--
-- @
-- point A (0, 0)
-- point B (4, 0)
-- segment A B
-- label A "A" below-left
-- @
module Competences.Markdown.Geometry.Parser
  ( parseGeometry
  )
where

import Competences.Markdown.Geometry.AST
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | Parse geometry DSL text into a scene
parseGeometry :: Text -> Either (ParseErrorBundle Text Void) GeometryScene
parseGeometry input
  | T.null (T.strip input) = Right (GeometryScene [])
  | otherwise = parse sceneP "geometry" input

sceneP :: Parser GeometryScene
sceneP = do
  skipMany blankLine
  cmds <- sepEndBy commandP (some blankLine <|> (eof *> pure []))
  eof
  pure $ GeometryScene cmds
  where
    blankLine = hspace *> newline

commandP :: Parser GeometryCommand
commandP =
  hspace
    *> choice
      [ pointP
      , segmentP
      , lineP
      , circleP
      , angleP
      , labelP
      ]

pointP :: Parser GeometryCommand
pointP = do
  _ <- string "point"
  hspace1
  name <- nameP
  hspace1
  _ <- char '('
  hspace
  x <- doubleP
  hspace
  _ <- char ','
  hspace
  y <- doubleP
  hspace
  _ <- char ')'
  pure $ DefinePoint name (Coord x y)

segmentP :: Parser GeometryCommand
segmentP = do
  _ <- string "segment"
  hspace1
  a <- nameP
  hspace1
  b <- nameP
  pure $ DrawSegment a b

lineP :: Parser GeometryCommand
lineP = do
  _ <- string "line"
  hspace1
  a <- nameP
  hspace1
  b <- nameP
  pure $ DrawLine a b

circleP :: Parser GeometryCommand
circleP = do
  _ <- string "circle"
  hspace1
  center <- nameP
  hspace1
  radius <- doubleP
  pure $ DrawCircle center radius

angleP :: Parser GeometryCommand
angleP = do
  _ <- string "angle"
  hspace1
  a <- nameP
  hspace1
  b <- nameP
  hspace1
  c <- nameP
  pure $ DrawAngle a b c

labelP :: Parser GeometryCommand
labelP = do
  _ <- string "label"
  hspace1
  name <- nameP
  hspace1
  _ <- char '"'
  txt <- takeWhileP (Just "label text") (/= '"')
  _ <- char '"'
  hspace1
  pos <- labelPositionP
  pure $ Label name txt pos

nameP :: Parser Name
nameP = T.pack <$> some (alphaNumChar <|> char '_')

doubleP :: Parser Double
doubleP = L.signed hspace L.float <|> (fromIntegral <$> L.signed hspace (L.decimal :: Parser Int))

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
