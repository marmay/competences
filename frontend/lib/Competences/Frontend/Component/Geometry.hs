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

import Competences.Frontend.SvgEmbed.Manager (EmbeddedSymbol (..), MathDisplay (..), SymbolId, hashLatex)
import Competences.Frontend.View.Tailwind (class_)
import Competences.Markdown.Geometry.AST
import Competences.Markdown.Geometry.Eval (evalScene)
import Competences.Markdown.Geometry.Parser
  ( currentGeometryVersion
  , geometryVersionText
  , parseGeometry
  , parseGeometryVersion
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (ms)
import Miso.Svg.Element qualified as Svg
import Miso.Svg.Property qualified as SP
import Text.Read (readMaybe)

-- | Render a RenderResult to an SVG Miso view
renderGeometry :: Map SymbolId EmbeddedSymbol -> RenderResult -> M.View model action
renderGeometry symbols result =
  let allPrims = background result <> main result <> foreground result
      (vb, w, h) = computeViewBox allPrims
   in Svg.svg_
        [ class_ "geometry-scene mx-auto my-2 max-w-full h-auto"
        , SP.viewBox_ (ms vb)
        , M.textProp "width" (ms (T.pack (show w) <> "cm"))
        , M.textProp "height" (ms (T.pack (show h) <> "cm"))
        , M.textProp "font-family" "var(--font-sans)"
        ]
        [ -- Background layer
          Svg.g_ [class_ "geometry-bg"] (map (renderPrimitive symbols) (background result))
        , -- Main layer
          Svg.g_ [class_ "geometry-main"] (map (renderPrimitive symbols) (main result))
        , -- Foreground layer
          Svg.g_ [class_ "geometry-fg"] (map (renderPrimitive symbols) (foreground result))
        ]

-- | Parse geometry text, evaluate, and render. Shows errors if parse fails.
renderGeometryText :: Map SymbolId EmbeddedSymbol -> Text -> M.View model action
renderGeometryText symbols txt =
  case parseGeometry txt of
    Left _err ->
      MH.div_
        [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
        [M.text $ ms ("Geometry parse error" :: Text)]
    Right cmds -> renderGeometry symbols (evalScene cmds)

-- | Render a geometry fenced code block, checking the version tag first.
--
-- Takes the full info string (e.g. @"geometry V1.0"@) and the block body.
-- If the version is unsupported, shows an error instead of rendering.
renderGeometryBlock :: Map SymbolId EmbeddedSymbol -> Maybe Text -> Text -> M.View model action
renderGeometryBlock symbols mInfo body =
  case mInfo >>= geometryVersionText of
    Nothing -> renderGeometryText symbols body
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
        | otherwise -> renderGeometryText symbols body

-- | Error view for unsupported geometry versions
versionErrorView :: Text -> M.View model action
versionErrorView msg =
  MH.div_
    [class_ "text-amber-700 bg-amber-50 font-mono text-sm p-2 rounded border border-amber-200"]
    [M.text $ ms msg]

-- -----------------------------------------------------------------
-- ViewBox computation
-- -----------------------------------------------------------------

-- | Compute SVG viewBox string and physical dimensions (in coordinate units = cm)
computeViewBox :: [RenderPrimitive] -> (Text, Double, Double)
computeViewBox prims
  | null vecs = ("-1 -1 2 2", 2, 2)
  | otherwise =
      let xs = [x | Vec2 x _ <- vecs]
          ys = [y | Vec2 _ y <- vecs]
          xMin = minimum xs
          xMax = maximum xs
          yMin = minimum ys
          yMax = maximum ys
          rangeX = max 1 (xMax - xMin)
          rangeY = max 1 (yMax - yMin)
          padX = max 0.8 (rangeX * 0.15)
          padY = max 0.8 (rangeY * 0.15)
          -- SVG Y axis is flipped (positive down), so we negate Y
          vbX = xMin - padX
          vbY = -(yMax + padY)
          vbW = rangeX + 2 * padX
          vbH = rangeY + 2 * padY
       in (T.pack $ show vbX <> " " <> show vbY <> " " <> show vbW <> " " <> show vbH, vbW, vbH)
  where
    vecs = concatMap primVecs prims

-- | Extract all Vec2 positions from a render primitive.
-- For labels, estimates text bounding box so the viewBox includes them.
primVecs :: RenderPrimitive -> [Vec2]
primVecs = \case
  RenderDot v _ -> [v]
  RenderSegment v1 v2 _ -> [v1, v2]
  RenderLabel v lbl pos _ -> labelBounds v lbl pos
  RenderAxisLine v1 v2 _ -> [v1, v2]
  RenderTick v _ _ -> [v]
  RenderGridLine v1 v2 _ -> [v1, v2]
  RenderAngleArc v startA sweepA r _ -> angleArcVecs v startA sweepA r
  RenderRightAngle v startA sweepA r _ -> angleArcVecs v startA sweepA r
  RenderFilledPolygon vs _ -> vs

-- | Estimate bounding-box corners of a label for viewBox calculation.
labelBounds :: Vec2 -> LabelContent -> LabelPosition -> [Vec2]
labelBounds (Vec2 x y) lbl pos =
  let (dx, dy, anchor) = labelOffset pos
      cx = x + dx
      cy = y - dy -- in math coords (positive up)
      fontSize = 0.45 :: Double
      halfH = fontSize / 2
      charW = 0.27 :: Double -- approximate average character width at fontSize
      textLen = case lbl of
        PlainLabel t -> fromIntegral (T.length t)
        MathLabel t -> fromIntegral (T.length t)
      textW = max 0.3 (textLen * charW) :: Double -- floor at 0.3 for single-char labels
      (leftX, rightX) = case anchor of
        "middle" -> (cx - textW / 2, cx + textW / 2)
        "end" -> (cx - textW, cx)
        _ -> (cx, cx + textW) -- "start"
   in [Vec2 leftX (cy - halfH), Vec2 rightX (cy + halfH)]

-- -----------------------------------------------------------------
-- Primitive rendering
-- -----------------------------------------------------------------

-- | Render a single primitive to SVG
renderPrimitive :: Map SymbolId EmbeddedSymbol -> RenderPrimitive -> M.View model action
renderPrimitive symbols = \case
  RenderDot (Vec2 x y) env ->
    Svg.circle_
      [ SP.cx_ (ms $ show x)
      , SP.cy_ (ms $ show (-y))
      , SP.r_ "0.1"
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
  RenderLabel (Vec2 x y) lbl pos env ->
    case lbl of
      PlainLabel txt ->
        let (dx, dy, anchor) = labelOffset pos
         in Svg.text_
              [ SP.x_ (ms $ show (x + dx))
              , SP.y_ (ms $ show (-(y - dy)))
              , SP.textAnchor_ (ms anchor)
              , SP.fontSize_ "0.45"
              , SP.fill_ (ms $ envColor env)
              , SP.dominantBaseline_ "central"
              ]
              [M.text (ms txt)]
      MathLabel latex ->
        let sid = hashLatex Inline latex
         in case Map.lookup sid symbols of
              Nothing ->
                let (dx, dy, anchor) = labelOffset pos
                 in Svg.text_
                      [ SP.x_ (ms $ show (x + dx))
                      , SP.y_ (ms $ show (-(y - dy)))
                      , SP.textAnchor_ (ms anchor)
                      , SP.fontSize_ "0.40"
                      , SP.fill_ (ms $ envColor env)
                      , SP.dominantBaseline_ "central"
                      , SP.fontStyle_ "italic"
                      ]
                      [M.text "[math]"]
              Just es -> renderMathLabel (Vec2 x y) es pos env
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
    let isYAxis = x == 0
     in Svg.g_
          []
          [ -- Tick mark
            Svg.line_
              ( ( if isYAxis
                    then
                      [ SP.x1_ (ms $ show (x - 0.1))
                      , SP.y1_ (ms $ show (-y))
                      , SP.x2_ (ms $ show (x + 0.1))
                      , SP.y2_ (ms $ show (-y))
                      ]
                    else
                      [ SP.x1_ (ms $ show x)
                      , SP.y1_ (ms $ show (-y - 0.1))
                      , SP.x2_ (ms $ show x)
                      , SP.y2_ (ms $ show (-y + 0.1))
                      ]
                )
                  <> [ SP.stroke_ (ms $ envColor env)
                     , SP.strokeWidth_ "0.02"
                     ]
              )
          , -- Tick label
            Svg.text_
              ( ( if isYAxis
                    then
                      [ SP.x_ (ms $ show (x - 0.2))
                      , SP.y_ (ms $ show (-y + 0.12))
                      , SP.textAnchor_ "end"
                      ]
                    else
                      [ SP.x_ (ms $ show x)
                      , SP.y_ (ms $ show (-y + 0.5))
                      , SP.textAnchor_ "middle"
                      ]
                )
                  <> [ SP.fontSize_ "0.35"
                     , SP.fill_ (ms $ envColor env)
                     ]
              )
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
  RenderAngleArc (Vec2 vx vy) startAngle sweepAngle radius env ->
    let (sx, sy, ex, ey, largeArc, sweepFlag) =
          arcParams vx vy startAngle sweepAngle radius
     in Svg.path_
          [ SP.d_ $ ms $
              "M " <> show sx <> " " <> show sy
                <> " A " <> show radius <> " " <> show radius
                <> " 0 " <> show largeArc <> " " <> show sweepFlag
                <> " " <> show ex <> " " <> show ey
          , SP.stroke_ (ms $ envColor env)
          , SP.strokeWidth_ (ms $ envStrokeWidth env)
          , SP.fill_ "none"
          , envDashAttr env
          ]
  RenderRightAngle (Vec2 vx vy) startAngle sweepAngle radius env ->
    let (sx, sy, ex, ey, largeArc, sweepFlag) =
          arcParams vx vy startAngle sweepAngle radius
        -- Dot at bisector, halfway along radius
        bisector = startAngle + sweepAngle / 2
        dotX = vx + radius * 0.5 * cos bisector
        dotY = -(vy + radius * 0.5 * sin bisector)
     in Svg.g_
          []
          [ Svg.path_
              [ SP.d_ $ ms $
                  "M " <> show sx <> " " <> show sy
                    <> " A " <> show radius <> " " <> show radius
                    <> " 0 " <> show largeArc <> " " <> show sweepFlag
                    <> " " <> show ex <> " " <> show ey
              , SP.stroke_ (ms $ envColor env)
              , SP.strokeWidth_ (ms $ envStrokeWidth env)
              , SP.fill_ "none"
              , envDashAttr env
              ]
          , Svg.circle_
              [ SP.cx_ (ms $ show dotX)
              , SP.cy_ (ms $ show dotY)
              , SP.r_ "0.04"
              , SP.fill_ (ms $ envColor env)
              ]
          ]
  RenderFilledPolygon vecs env ->
    case fillColor env of
      Nothing -> Svg.g_ [] []
      Just fc ->
        Svg.polygon_
          [ SP.points_ $ ms $ pointsString vecs
          , SP.fill_ (ms $ envColor' fc)
          , M.textProp "fill-opacity" "0.15"
          , SP.stroke_ "none"
          ]

-- | Render a MathJax-rendered formula as an SVG @\<image\>@ element.
-- Converts MathJax's @ex@ units to geometry coordinate units.
-- Relies on 'svgToDataUrl' producing base64 data URLs (required by Chrome).
--
-- Chrome refuses to render @\<image\>@ elements whose width or height is
-- below ~0.5 SVG user units (geometry coordinates are small — typically
-- sub-1.0). We work around this by rendering the image at a 100× nominal
-- size and wrapping it in a @\<g transform="translate(…) scale(0.01)"\>@.
renderMathLabel :: Vec2 -> EmbeddedSymbol -> LabelPosition -> DrawEnv -> M.View model action
renderMathLabel (Vec2 x y) es pos _env =
  let parseEx t = maybe 1.0 id $ T.stripSuffix "ex" t >>= (readMaybe . T.unpack)
      exToCoord = 0.22 :: Double -- ~0.5 * geometry fontSize (0.45)
      imgW = parseEx es.width * exToCoord
      imgH = parseEx es.height * exToCoord
      (dx, dy, anchor) = labelOffset pos
      anchorDx = case anchor of
        "middle" -> -(imgW / 2)
        "end" -> -imgW
        _ -> 0
      imgX = x + dx + anchorDx
      imgY = -(y - dy) - imgH / 2
      -- Scale factor: render at 100× then scale down
      s = 0.01 :: Double
      nomW = imgW / s
      nomH = imgH / s
   in Svg.g_
        [ SP.transform_ $ ms $
            "translate(" <> show imgX <> "," <> show imgY <> ") scale(" <> show s <> ")"
        ]
        [ Svg.image_
            [ SP.x_ "0"
            , SP.y_ "0"
            , M.textProp "width" (ms $ show nomW)
            , M.textProp "height" (ms $ show nomH)
            , M.textProp "href" (ms es.dataUrl)
            ]
        ]

-- | Vec2s contributed by an angle arc to the viewBox calculation:
-- the vertex and the two arc endpoints.
angleArcVecs :: Vec2 -> Double -> Double -> Double -> [Vec2]
angleArcVecs (Vec2 vx vy) startA sweepA r =
  let endA = startA + sweepA
   in [ Vec2 vx vy
      , Vec2 (vx + r * cos startA) (vy + r * sin startA)
      , Vec2 (vx + r * cos endA) (vy + r * sin endA)
      ]

-- | Compute SVG arc path parameters from angle arc fields.
-- Returns @(startX, startY, endX, endY, largeArcFlag, sweepFlag)@ in SVG
-- coordinates (Y negated).
arcParams :: Double -> Double -> Double -> Double -> Double -> (Double, Double, Double, Double, Int, Int)
arcParams vx vy startAngle sweepAngle radius =
  let sx = vx + radius * cos startAngle
      sy = -(vy + radius * sin startAngle)
      endAngle = startAngle + sweepAngle
      ex = vx + radius * cos endAngle
      ey = -(vy + radius * sin endAngle)
      -- Y-negation of endpoints preserves the rotation direction in SVG
      -- coordinates, so math-CCW (positive sweep) maps to SVG-CCW (sweepFlag=0).
      sweepFlag = if sweepAngle > 0 then 0 else 1
      largeArc = if abs sweepAngle > pi then 1 else 0
   in (sx, sy, ex, ey, largeArc, sweepFlag)

-- -----------------------------------------------------------------
-- Environment to SVG attributes
-- -----------------------------------------------------------------

-- | Convert a list of Vec2 to an SVG points string (Y-flipped)
pointsString :: [Vec2] -> Text
pointsString = T.intercalate " " . map (\(Vec2 x y) -> T.pack (show x <> "," <> show (-y)))

-- | Color value from a 'Color' (without DrawEnv lookup)
envColor' :: Color -> Text
envColor' CurrentColor = "currentColor"
envColor' (NamedColor c) = c

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
  Above -> (0, -0.20, "middle")
  Below -> (0, 0.20, "middle")
  LeftOf -> (-0.12, 0, "end")
  RightOf -> (0.12, 0, "start")
  AboveLeft -> (-0.12, -0.15, "end")
  AboveRight -> (0.12, -0.15, "start")
  BelowLeft -> (-0.12, 0.15, "end")
  BelowRight -> (0.12, 0.15, "start")
