-- |
-- Module      : Competences.Markdown.Geometry.AST
-- Description : AST for the geometry DSL (V1)
--
-- Three orthogonal operations on geometric primitives:
-- @def*@ (name a primitive), @draw*@ (render it), @label*@ (annotate it).
-- V1 supports points and segments. The architecture extends mechanically
-- to circles, arcs, angles, etc.
module Competences.Markdown.Geometry.AST
  ( -- * Commands
    Command (..)
  , DrawPrimitive (..)
  , LabelPrimitive (..)
  , SegmentRef (..)
  , AngleRef (..)

    -- * Point constructions
  , PointConstruction (..)
  , LineRef (..)

    -- * Modifiers
  , Modifier (..)
  , EnvModifier (..)
  , AutoDecorator (..)

    -- * Drawing environment
  , DrawEnv (..)
  , defaultDrawEnv
  , Color (..)
  , LineStyle (..)
  , LineWidth (..)
  , Layer (..)

    -- * Label content
  , LabelContent (..)

    -- * Geometry primitives
  , Vec2 (..)
  , Name
  , LabelPosition (..)
  , SegmentSide (..)

    -- * Version
  , GeometryVersion

    -- * Render output
  , RenderPrimitive (..)
  , RenderResult (..)
  , RenderLog (..)
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | DSL version: (major, minor)
type GeometryVersion = (Int, Int)

-- | Top-level commands
data Command
  = -- | @defPoint A (x, y)@
    DefPoint !Name !Vec2
  | -- | @defPointBy M (midpoint A B)@
    DefPointBy !Name !PointConstruction
  | -- | @defSegment s A B@
    DefSegment !Name !Name !Name
  | -- | @drawPoint A@, @drawSegment s@, etc.
    Draw !DrawPrimitive
  | -- | @labelPoint A "text" above@, etc.
    Label !LabelPrimitive
  | -- | @color red { ... }@, @dashed { ... }@, etc.
    ModifierBlock !Modifier ![Command]
  deriving (Eq, Show)

-- | Reference to a segment: by name or inline (two point names)
data SegmentRef
  = SegByName !Name
  | SegInline !Name !Name
  deriving (Eq, Show)

-- | Reference to an angle: three points A B C where B is the vertex
data AngleRef = AngleRef !Name !Name !Name
  deriving (Eq, Show)

-- | Drawable primitives
data DrawPrimitive
  = DrawPoint !Name
  | DrawSegment !SegmentRef
  | DrawAngle !AngleRef
  | DrawRightAngle !AngleRef
  | DrawFilledPolygon ![Name]
  deriving (Eq, Show)

-- | Label content: plain text or LaTeX math (delimited by @$...$@)
data LabelContent = PlainLabel !Text | MathLabel !Text
  deriving (Eq, Show)

-- | Labelable primitives
data LabelPrimitive
  = LabelAtPoint !Name !LabelContent !LabelPosition
  | LabelOnSegment !SegmentRef !LabelContent !SegmentSide !Double
  | LabelAngle !AngleRef !LabelContent
  | LabelAutoPoint !AngleRef !LabelContent
  deriving (Eq, Show)

-- | Point constructions (for @defPointBy@)
data PointConstruction
  = Midpoint !Name !Name
  | Lerp !Name !Name !Double
  | Rotate !Name !Double !Name
  | Reflect !LineRef !Name
  | Translate !Vec2 !Name
  deriving (Eq, Show)

-- | Line reference for reflect
data LineRef = LineThrough !Name !Name
  deriving (Eq, Show)

-- | Modifiers (block-scoped)
data Modifier
  = EnvMod !EnvModifier
  | AutoDec !AutoDecorator
  | LayerMod !Layer
  deriving (Eq, Show)

data EnvModifier
  = SetColor !Color
  | SetDashed
  | SetThick
  | SetThin
  | SetFill !Color
  deriving (Eq, Show)

data AutoDecorator
  = LabelAll !LabelPosition
  | Axes
  | Grid
  deriving (Eq, Show)

-- | Side of a segment for label placement
data SegmentSide = SegAbove | SegBelow
  deriving (Eq, Show)

-- | Drawing environment (scoped by modifier blocks)
data DrawEnv = DrawEnv
  { color :: !Color
  , lineStyle :: !LineStyle
  , lineWidth :: !LineWidth
  , layer :: !Layer
  , fillColor :: !(Maybe Color)
  }
  deriving (Eq, Show)

defaultDrawEnv :: DrawEnv
defaultDrawEnv =
  DrawEnv
    { color = CurrentColor
    , lineStyle = Solid
    , lineWidth = NormalWidth
    , layer = Main
    , fillColor = Nothing
    }

-- | 2D vector / point
data Vec2 = Vec2 !Double !Double
  deriving (Eq, Show)

-- | Point / object name
type Name = Text

-- | Stroke/fill color
data Color
  = CurrentColor
  | NamedColor !Text
  deriving (Eq, Show)

data LineStyle = Solid | Dashed
  deriving (Eq, Show)

data LineWidth = ThinWidth | NormalWidth | ThickWidth
  deriving (Eq, Show)

data Layer = Background | Main | Foreground
  deriving (Eq, Show)

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

-- | Render output primitives (consumed by the SVG renderer)
data RenderPrimitive
  = RenderDot !Vec2 !DrawEnv
  | RenderSegment !Vec2 !Vec2 !DrawEnv
  | RenderLabel !Vec2 !LabelContent !LabelPosition !DrawEnv
  | RenderAxisLine !Vec2 !Vec2 !DrawEnv
  | RenderTick !Vec2 !Text !DrawEnv
  | RenderGridLine !Vec2 !Vec2 !DrawEnv
  | -- | Angle arc: vertex, startAngle (rad), sweepAngle (rad), radius
    RenderAngleArc !Vec2 !Double !Double !Double !DrawEnv
  | -- | Right-angle arc (German style: arc + dot): same fields as 'RenderAngleArc'
    RenderRightAngle !Vec2 !Double !Double !Double !DrawEnv
  | -- | Filled polygon: list of vertices + draw environment (fillColor used)
    RenderFilledPolygon ![Vec2] !DrawEnv
  deriving (Eq, Show)

-- | Three-layer render result
data RenderResult = RenderResult
  { background :: ![RenderPrimitive]
  , main :: ![RenderPrimitive]
  , foreground :: ![RenderPrimitive]
  }
  deriving (Eq, Show)

instance Semigroup RenderResult where
  RenderResult bg1 mn1 fg1 <> RenderResult bg2 mn2 fg2 =
    RenderResult (bg1 <> bg2) (mn1 <> mn2) (fg1 <> fg2)

instance Monoid RenderResult where
  mempty = RenderResult [] [] []

-- | Log of what was drawn (for auto-decorators)
data RenderLog = RenderLog
  { drawnPoints :: ![(Name, Vec2)]
  , allPoints :: !(Map Name Vec2)
  }
  deriving (Eq, Show)

instance Semigroup RenderLog where
  RenderLog dp1 ap1 <> RenderLog dp2 ap2 =
    RenderLog (dp1 <> dp2) (Map.union ap1 ap2)

instance Monoid RenderLog where
  mempty = RenderLog [] Map.empty
