-- |
-- Module      : Competences.Frontend.Component.Geometry
-- Description : SVG renderer for the geometry DSL (V1)
--
-- Converts geometry DSL text (parsed → evaluated → 'RenderResult') into an
-- SVG Miso view. Renders three layers: background → main → foreground.
-- Y-axis is flipped for mathematical convention (positive up).
module Competences.Frontend.Component.Geometry
  ( renderGeometry
  , renderGeometryText
  , renderGeometryBlock
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Competences.Markdown.Geometry.AST
import Competences.Markdown.Geometry.Eval (evalScene)
import Competences.Markdown.Geometry.Parser
  ( currentGeometryVersion
  , geometryVersionText
  , parseGeometry
  , parseGeometryVersion
  )
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property (height_, width_)
import Miso.String (ms)
import Miso.Svg.Element qualified as Svg
import Miso.Svg.Property qualified as SP

-- | Render a RenderResult to an SVG Miso view
renderGeometry :: RenderResult -> M.View model action
renderGeometry result =
  let allPrims = background result <> main result <> foreground result
      vb = computeViewBox allPrims
   in Svg.svg_
        [ class_ "geometry-scene mx-auto my-2"
        , SP.viewBox_ (ms vb)
        , width_ "400"
        , height_ "300"
        ]
        [ -- Background layer
          Svg.g_ [class_ "geometry-bg"] (map renderPrimitive (background result))
        , -- Main layer
          Svg.g_ [class_ "geometry-main"] (map renderPrimitive (main result))
        , -- Foreground layer
          Svg.g_ [class_ "geometry-fg"] (map renderPrimitive (foreground result))
        ]

-- | Parse geometry text, evaluate, and render. Shows errors if parse fails.
renderGeometryText :: Text -> M.View model action
renderGeometryText txt =
  case parseGeometry txt of
    Left _err ->
      MH.div_
        [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
        [M.text $ ms ("Geometry parse error" :: Text)]
    Right cmds -> renderGeometry (evalScene cmds)

-- | Render a geometry fenced code block, checking the version tag first.
--
-- Takes the full info string (e.g. @"geometry V1.0"@) and the block body.
-- If the version is unsupported, shows an error instead of rendering.
renderGeometryBlock :: Maybe Text -> Text -> M.View model action
renderGeometryBlock mInfo body =
  case mInfo >>= geometryVersionText of
    Nothing -> renderGeometryText body
    Just vText -> case parseGeometryVersion vText of
      Nothing ->
        versionErrorView $ "Unbekannte Versionsangabe: " <> vText
      Just (maj, _min)
        | maj > fst currentGeometryVersion ->
            versionErrorView $
              "Dieser Geometrie-Block benötigt Version " <> vText
                <> ", aber nur V"
                <> T.pack (show (fst currentGeometryVersion))
                <> "."
                <> T.pack (show (snd currentGeometryVersion))
                <> " wird unterstützt."
        | otherwise -> renderGeometryText body

-- | Error view for unsupported geometry versions
versionErrorView :: Text -> M.View model action
versionErrorView msg =
  MH.div_
    [class_ "text-amber-700 bg-amber-50 font-mono text-sm p-2 rounded border border-amber-200"]
    [M.text $ ms msg]

-- -----------------------------------------------------------------
-- ViewBox computation
-- -----------------------------------------------------------------

-- | Compute SVG viewBox from all render primitives
computeViewBox :: [RenderPrimitive] -> Text
computeViewBox prims
  | null vecs = "-1 -1 2 2"
  | otherwise =
      let xs = [x | Vec2 x _ <- vecs]
          ys = [y | Vec2 _ y <- vecs]
          xMin = minimum xs
          xMax = maximum xs
          yMin = minimum ys
          yMax = maximum ys
          rangeX = max 1 (xMax - xMin)
          rangeY = max 1 (yMax - yMin)
          padX = max 0.5 (rangeX * 0.15)
          padY = max 0.5 (rangeY * 0.15)
          -- SVG Y axis is flipped (positive down), so we negate Y
          vbX = xMin - padX
          vbY = -(yMax + padY)
          vbW = rangeX + 2 * padX
          vbH = rangeY + 2 * padY
       in T.pack $ show vbX <> " " <> show vbY <> " " <> show vbW <> " " <> show vbH
  where
    vecs = concatMap primVecs prims

-- | Extract all Vec2 positions from a render primitive
primVecs :: RenderPrimitive -> [Vec2]
primVecs = \case
  RenderDot v _ -> [v]
  RenderSegment v1 v2 _ -> [v1, v2]
  RenderLabel v _ _ _ -> [v]
  RenderAxisLine v1 v2 _ -> [v1, v2]
  RenderTick v _ _ -> [v]
  RenderGridLine v1 v2 _ -> [v1, v2]

-- -----------------------------------------------------------------
-- Primitive rendering
-- -----------------------------------------------------------------

-- | Render a single primitive to SVG
renderPrimitive :: RenderPrimitive -> M.View model action
renderPrimitive = \case
  RenderDot (Vec2 x y) env ->
    Svg.circle_
      [ SP.cx_ (ms $ show x)
      , SP.cy_ (ms $ show (-y))
      , SP.r_ "0.08"
      , SP.fill_ (ms $ envColor env)
      ]
  RenderSegment (Vec2 x1 y1) (Vec2 x2 y2) env ->
    Svg.line_
      [ SP.x1_ (ms $ show x1)
      , SP.y1_ (ms $ show (-y1))
      , SP.x2_ (ms $ show x2)
      , SP.y2_ (ms $ show (-y2))
      , SP.stroke_ (ms $ envColor env)
      , SP.strokeWidth_ (ms $ envStrokeWidth env)
      , envDashAttr env
      ]
  RenderLabel (Vec2 x y) txt pos env ->
    let (dx, dy, anchor) = labelOffset pos
     in Svg.text_
          [ SP.x_ (ms $ show (x + dx))
          , SP.y_ (ms $ show (-(y - dy)))
          , M.textProp (ms ("text-anchor" :: Text)) (ms anchor)
          , M.textProp (ms ("font-size" :: Text)) (ms ("0.35" :: Text))
          , SP.fill_ (ms $ envColor env)
          , M.textProp (ms ("dominant-baseline" :: Text)) (ms ("central" :: Text))
          ]
          [M.text (ms txt)]
  RenderAxisLine (Vec2 x1 y1) (Vec2 x2 y2) env ->
    Svg.line_
      [ SP.x1_ (ms $ show x1)
      , SP.y1_ (ms $ show (-y1))
      , SP.x2_ (ms $ show x2)
      , SP.y2_ (ms $ show (-y2))
      , SP.stroke_ (ms $ envColor env)
      , SP.strokeWidth_ "0.03"
      ]
  RenderTick (Vec2 x y) txt env ->
    Svg.g_
      []
      [ -- Tick mark
        Svg.line_
          [ SP.x1_ (ms $ show x)
          , SP.y1_ (ms $ show (-y - 0.08))
          , SP.x2_ (ms $ show x)
          , SP.y2_ (ms $ show (-y + 0.08))
          , SP.stroke_ (ms $ envColor env)
          , SP.strokeWidth_ "0.02"
          ]
      , -- Tick label
        Svg.text_
          [ SP.x_ (ms $ show x)
          , SP.y_ (ms $ show (-y + 0.35))
          , M.textProp (ms ("text-anchor" :: Text)) (ms ("middle" :: Text))
          , M.textProp (ms ("font-size" :: Text)) (ms ("0.25" :: Text))
          , SP.fill_ (ms $ envColor env)
          ]
          [M.text (ms txt)]
      ]
  RenderGridLine (Vec2 x1 y1) (Vec2 x2 y2) env ->
    Svg.line_
      [ SP.x1_ (ms $ show x1)
      , SP.y1_ (ms $ show (-y1))
      , SP.x2_ (ms $ show x2)
      , SP.y2_ (ms $ show (-y2))
      , SP.stroke_ (ms $ envColor env)
      , SP.strokeWidth_ "0.01"
      ]

-- -----------------------------------------------------------------
-- Environment to SVG attributes
-- -----------------------------------------------------------------

envColor :: DrawEnv -> Text
envColor env = case color env of
  CurrentColor -> "currentColor"
  NamedColor c -> c

envStrokeWidth :: DrawEnv -> Text
envStrokeWidth env = case lineWidth env of
  ThinWidth -> "0.02"
  NormalWidth -> "0.04"
  ThickWidth -> "0.08"

envDashAttr :: DrawEnv -> M.Attribute action
envDashAttr env = case lineStyle env of
  Solid -> SP.strokeDasharray_ "none"
  Dashed -> SP.strokeDasharray_ "0.12,0.08"

-- | Offset and anchor for label positions
labelOffset :: LabelPosition -> (Double, Double, Text)
labelOffset = \case
  Above -> (0, -0.4, "middle")
  Below -> (0, 0.4, "middle")
  LeftOf -> (-0.4, 0, "end")
  RightOf -> (0.4, 0, "start")
  AboveLeft -> (-0.3, -0.3, "end")
  AboveRight -> (0.3, -0.3, "start")
  BelowLeft -> (-0.3, 0.3, "end")
  BelowRight -> (0.3, 0.3, "start")
