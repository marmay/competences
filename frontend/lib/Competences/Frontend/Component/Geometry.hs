-- |
-- Module      : Competences.Frontend.Component.Geometry
-- Description : SVG renderer for the geometry DSL
--
-- Converts a 'GeometryScene' (parsed from fenced code blocks with info
-- string @geometry@) into an SVG Miso view. Auto-computes viewBox from
-- point positions with padding.
module Competences.Frontend.Component.Geometry
  ( renderGeometry
  , renderGeometryText
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Competences.Markdown.Geometry.AST
import Competences.Markdown.Geometry.Parser (parseGeometry)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property (height_, width_)
import Miso.String (ms)
import Miso.Svg.Element qualified as Svg
import Miso.Svg.Property qualified as SP

-- | Render a GeometryScene to an SVG Miso view
renderGeometry :: GeometryScene -> M.View model action
renderGeometry scene@(GeometryScene cmds) =
  let points = collectPoints cmds
      vb = computeViewBox points
   in Svg.svg_
        [ class_ "geometry-scene mx-auto my-2"
        , SP.viewBox_ (ms vb)
        , width_ "400"
        , height_ "300"
        ]
        (renderCommands points scene)

-- | Parse geometry text and render, showing errors if parse fails
renderGeometryText :: Text -> M.View model action
renderGeometryText txt =
  case parseGeometry txt of
    Left _err ->
      MH.div_
        [class_ "text-red-600 bg-red-50 font-mono text-sm p-2 rounded border border-red-200"]
        [M.text $ ms ("Geometry parse error" :: Text)]
    Right scene -> renderGeometry scene

-- | Collect all named points from the commands
collectPoints :: [GeometryCommand] -> Map Name Coord
collectPoints = foldl' go Map.empty
  where
    go acc (DefinePoint name coord) = Map.insert name coord acc
    go acc _ = acc

-- | Compute SVG viewBox from points with padding
-- Returns "minX minY width height" string
computeViewBox :: Map Name Coord -> Text
computeViewBox points
  | Map.null points = "-1 -1 2 2"
  | otherwise =
      let coords = Map.elems points
          xs = map (\(Coord x _) -> x) coords
          ys = map (\(Coord _ y) -> y) coords
          minX = minimum xs
          maxX = maximum xs
          minY = minimum ys
          maxY = maximum ys
          -- Add padding (15% of range or at least 0.5)
          rangeX = max 1 (maxX - minX)
          rangeY = max 1 (maxY - minY)
          padX = max 0.5 (rangeX * 0.15)
          padY = max 0.5 (rangeY * 0.15)
          -- SVG Y axis is flipped (positive down), so we negate Y
          vbX = minX - padX
          vbY = -(maxY + padY)
          vbW = rangeX + 2 * padX
          vbH = rangeY + 2 * padY
       in T.pack $ show vbX <> " " <> show vbY <> " " <> show vbW <> " " <> show vbH

-- | Render all commands to SVG elements
renderCommands :: Map Name Coord -> GeometryScene -> [M.View model action]
renderCommands points (GeometryScene cmds) = concatMap (renderCommand points) cmds

renderCommand :: Map Name Coord -> GeometryCommand -> [M.View model action]
renderCommand points = \case
  DefinePoint _name coord ->
    -- Render point as a small filled circle
    let (Coord x y) = coord
     in [ Svg.circle_
            [ SP.cx_ (ms $ show x)
            , SP.cy_ (ms $ show (-y)) -- Flip Y axis
            , SP.r_ "0.08"
            , SP.fill_ "currentColor"
            ]
        ]
  DrawSegment name1 name2 ->
    case (Map.lookup name1 points, Map.lookup name2 points) of
      (Just (Coord x1 y1), Just (Coord x2 y2)) ->
        [ Svg.line_
            [ SP.x1_ (ms $ show x1)
            , SP.y1_ (ms $ show (-y1))
            , SP.x2_ (ms $ show x2)
            , SP.y2_ (ms $ show (-y2))
            , SP.stroke_ "currentColor"
            , SP.strokeWidth_ "0.04"
            ]
        ]
      _ -> []
  DrawLine name1 name2 ->
    case (Map.lookup name1 points, Map.lookup name2 points) of
      (Just (Coord x1 y1), Just (Coord x2 y2)) ->
        let dx = x2 - x1
            dy = y2 - y1
            ext = 10.0 :: Double
         in [ Svg.line_
                [ SP.x1_ (ms $ show (x1 - ext * dx))
                , SP.y1_ (ms $ show (-(y1 - ext * dy)))
                , SP.x2_ (ms $ show (x2 + ext * dx))
                , SP.y2_ (ms $ show (-(y2 + ext * dy)))
                , SP.stroke_ "currentColor"
                , SP.strokeWidth_ "0.03"
                , SP.strokeDasharray_ "0.1,0.08"
                ]
            ]
      _ -> []
  DrawCircle centerName radius ->
    case Map.lookup centerName points of
      Just (Coord cx cy) ->
        [ Svg.circle_
            [ SP.cx_ (ms $ show cx)
            , SP.cy_ (ms $ show (-cy))
            , SP.r_ (ms $ show radius)
            , SP.fill_ "none"
            , SP.stroke_ "currentColor"
            , SP.strokeWidth_ "0.04"
            ]
        ]
      Nothing -> []
  DrawAngle _name1 _name2 _name3 ->
    -- Angle arcs are complex - placeholder for now
    []
  Label name txt pos ->
    case Map.lookup name points of
      Just coord -> [renderLabel txt coord pos]
      Nothing -> []

-- | Render a text label at a position relative to a coordinate
renderLabel :: Text -> Coord -> LabelPosition -> M.View model action
renderLabel txt (Coord x y) pos =
  let offset = 0.25 :: Double
      (dx, dy, anchor) = case pos of
        Above -> (0, -offset, "middle" :: Text)
        Below -> (0, offset, "middle")
        LeftOf -> (-offset, 0, "end")
        RightOf -> (offset, 0, "start")
        AboveLeft -> (-offset, -offset, "end")
        AboveRight -> (offset, -offset, "start")
        BelowLeft -> (-offset, offset, "end")
        BelowRight -> (offset, offset, "start")
   in Svg.text_
        [ SP.x_ (ms $ show (x + dx))
        , SP.y_ (ms $ show (-(y - dy)))
        , M.textProp (ms ("text-anchor" :: Text)) (ms anchor)
        , M.textProp (ms ("font-size" :: Text)) (ms ("0.35" :: Text))
        , SP.fill_ "currentColor"
        , M.textProp (ms ("dominant-baseline" :: Text)) (ms ("central" :: Text))
        ]
        [M.text (ms txt)]
