-- |
-- Module      : Competences.Markdown.Geometry.AST
-- Description : AST for the geometry DSL
--
-- A simple DSL for describing 2D geometry scenes, inspired by tkz-euclide.
-- Used inside fenced code blocks with info string @geometry@.
module Competences.Markdown.Geometry.AST
  ( GeometryScene (..)
  , GeometryCommand (..)
  , Coord (..)
  , Name
  , LabelPosition (..)
  )
where

import Data.Text (Text)

-- | A geometry scene is a sequence of commands
newtype GeometryScene = GeometryScene [GeometryCommand]
  deriving (Eq, Show)

-- | Individual geometry commands
data GeometryCommand
  = -- | Define a named point at coordinates
    DefinePoint !Name !Coord
  | -- | Draw a line segment between two named points
    DrawSegment !Name !Name
  | -- | Draw an infinite line through two named points
    DrawLine !Name !Name
  | -- | Draw a circle with center point and radius
    DrawCircle !Name !Double
  | -- | Draw an angle arc between three points
    DrawAngle !Name !Name !Name
  | -- | Label a point with text at a position
    Label !Name !Text !LabelPosition
  deriving (Eq, Show)

-- | 2D coordinate
data Coord = Coord !Double !Double
  deriving (Eq, Show)

-- | Point name
type Name = Text

-- | Position for labels relative to a point
data LabelPosition
  = Above
  | Below
  | LeftOf
  | RightOf
  | AboveLeft
  | AboveRight
  | BelowLeft
  | BelowRight
  deriving (Eq, Show)
