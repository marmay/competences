-- |
-- Module      : Competences.Markdown.Geometry.Eval
-- Description : Evaluator for the geometry DSL (V1)
--
-- Pure evaluation of 'Command' lists into 'RenderResult'.
-- Uses State monad internally for point/segment maps and draw environment.
-- Auto-decorators (axes, grid, labelAll) inspect the render log.
module Competences.Markdown.Geometry.Eval
  ( evalScene
  , extractMathLabels
  )
where

import Competences.Markdown.Geometry.AST
import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | Evaluation state
data EvalState = EvalState
  { esPoints :: !(Map Name Vec2)
  , esSegments :: !(Map Name (Name, Name))
  , esDrawEnv :: !DrawEnv
  }

-- | Evaluation monad
type Eval a = State EvalState a

-- | Evaluate a list of commands into a three-layer render result
evalScene :: [Command] -> RenderResult
evalScene cmds =
  let initState =
        EvalState
          { esPoints = Map.empty
          , esSegments = Map.empty
          , esDrawEnv = defaultDrawEnv
          }
      (result, _log) = fst $ runState (evalCommands cmds) initState
   in result

-- | Evaluate commands, returning render result and render log
evalCommands :: [Command] -> Eval (RenderResult, RenderLog)
evalCommands cmds = do
  results <- mapM evalCommand cmds
  pure $ mconcat results

-- | Evaluate a single command
evalCommand :: Command -> Eval (RenderResult, RenderLog)
evalCommand = \case
  DefPoint name vec -> do
    modify' $ \s -> s {esPoints = Map.insert name vec (esPoints s)}
    let lg = mempty {allPoints = Map.singleton name vec}
    pure (mempty, lg)
  DefPointBy name constr -> do
    mVec <- evalConstruction constr
    case mVec of
      Nothing -> pure (mempty, mempty)
      Just vec -> do
        modify' $ \s -> s {esPoints = Map.insert name vec (esPoints s)}
        let lg = mempty {allPoints = Map.singleton name vec}
        pure (mempty, lg)
  DefSegment name a b -> do
    modify' $ \s -> s {esSegments = Map.insert name (a, b) (esSegments s)}
    pure (mempty, mempty)
  Draw prim -> evalDraw prim
  Label prim -> evalLabel prim
  ModifierBlock modifier children -> evalModifierBlock modifier children

-- -----------------------------------------------------------------
-- Point constructions
-- -----------------------------------------------------------------

evalConstruction :: PointConstruction -> Eval (Maybe Vec2)
evalConstruction = \case
  Midpoint a b -> do
    pts <- gets esPoints
    pure $ do
      Vec2 ax ay <- Map.lookup a pts
      Vec2 bx by <- Map.lookup b pts
      Just $ Vec2 ((ax + bx) / 2) ((ay + by) / 2)
  Lerp a b t -> do
    pts <- gets esPoints
    pure $ do
      Vec2 ax ay <- Map.lookup a pts
      Vec2 bx by <- Map.lookup b pts
      Just $ Vec2 (ax + t * (bx - ax)) (ay + t * (by - ay))
  Rotate center degrees point -> do
    pts <- gets esPoints
    pure $ do
      Vec2 cx cy <- Map.lookup center pts
      Vec2 px py <- Map.lookup point pts
      let rad = degrees * pi / 180
          dx = px - cx
          dy = py - cy
          cosR = cos rad
          sinR = sin rad
      Just $ Vec2 (cx + dx * cosR - dy * sinR) (cy + dx * sinR + dy * cosR)
  Reflect (LineThrough a b) point -> do
    pts <- gets esPoints
    pure $ do
      Vec2 ax ay <- Map.lookup a pts
      Vec2 bx by <- Map.lookup b pts
      Vec2 px py <- Map.lookup point pts
      let dx = bx - ax
          dy = by - ay
          lenSq = dx * dx + dy * dy
      if lenSq == 0
        then Nothing
        else do
          let t = ((px - ax) * dx + (py - ay) * dy) / lenSq
              projX = ax + t * dx
              projY = ay + t * dy
          Just $ Vec2 (2 * projX - px) (2 * projY - py)
  Translate (Vec2 dx dy) point -> do
    pts <- gets esPoints
    pure $ do
      Vec2 px py <- Map.lookup point pts
      Just $ Vec2 (px + dx) (py + dy)

-- -----------------------------------------------------------------
-- Draw evaluation
-- -----------------------------------------------------------------

evalDraw :: DrawPrimitive -> Eval (RenderResult, RenderLog)
evalDraw = \case
  DrawPoint name -> do
    pts <- gets esPoints
    env <- gets esDrawEnv
    case Map.lookup name pts of
      Nothing -> pure (mempty, mempty)
      Just vec -> do
        let prim = RenderDot vec env
            lg = mempty {drawnPoints = [(name, vec)], allPoints = Map.singleton name vec}
        pure (emitToLayer env prim, lg)
  DrawSegment segRef -> do
    mEndpoints <- resolveSegmentRef segRef
    env <- gets esDrawEnv
    case mEndpoints of
      Nothing -> pure (mempty, mempty)
      Just (v1, v2) -> do
        let prim = RenderSegment v1 v2 env
        pure (emitToLayer env prim, mempty)
  DrawAngle ref -> do
    mPts <- resolveAngleRef ref
    env <- gets esDrawEnv
    case mPts of
      Nothing -> pure (mempty, mempty)
      Just (va, vb, vc) -> do
        let (start, sweep) = computeAngleArc va vb vc
            radius = clampRadius 1.0 vb va vc
            prim = RenderAngleArc vb start sweep radius env
        pure (emitToLayer env prim, mempty)
  DrawRightAngle ref -> do
    mPts <- resolveAngleRef ref
    env <- gets esDrawEnv
    case mPts of
      Nothing -> pure (mempty, mempty)
      Just (va, vb, vc) -> do
        let (start, sweep) = computeAngleArc va vb vc
            radius = clampRadius 0.7 vb va vc
            prim = RenderRightAngle vb start sweep radius env
        pure (emitToLayer env prim, mempty)
  DrawFilledPolygon names -> do
    pts <- gets esPoints
    env <- gets esDrawEnv
    case fillColor env of
      Nothing -> pure (mempty, mempty)
      Just _ -> do
        let mVecs = mapM (`Map.lookup` pts) names
        case mVecs of
          Nothing -> pure (mempty, mempty)
          Just vecs -> do
            let bgEnv = env {layer = Background}
                prim = RenderFilledPolygon vecs bgEnv
            pure (mempty {background = [prim]}, mempty)

-- -----------------------------------------------------------------
-- Label evaluation
-- -----------------------------------------------------------------

evalLabel :: LabelPrimitive -> Eval (RenderResult, RenderLog)
evalLabel = \case
  LabelAtPoint name txt pos -> do
    pts <- gets esPoints
    env <- gets esDrawEnv
    case Map.lookup name pts of
      Nothing -> pure (mempty, mempty)
      Just vec -> do
        let prim = RenderLabel vec txt pos env
        pure (emitToLayer env prim, mempty)
  LabelOnSegment segRef txt side frac -> do
    mEndpoints <- resolveSegmentRef segRef
    env <- gets esDrawEnv
    case mEndpoints of
      Nothing -> pure (mempty, mempty)
      Just (Vec2 ax ay, Vec2 bx by) -> do
        let -- Position along segment
            mx = ax + frac * (bx - ax)
            my = ay + frac * (by - ay)
            -- Perpendicular direction (left of A→B)
            dx = bx - ax
            dy = by - ay
            len = sqrt (dx * dx + dy * dy)
            offset = 0.05
            (nx, ny)
              | len == 0 = (0, offset)
              | otherwise =
                  let perpX = -dy / len
                      perpY = dx / len
                   in case side of
                        SegAbove -> (perpX * offset, perpY * offset)
                        SegBelow -> (-perpX * offset, -perpY * offset)
            labelVec = Vec2 (mx + nx) (my + ny)
            -- Choose label position based on perpendicular direction
            labelPos = segmentSideToPosition side dx dy
            prim = RenderLabel labelVec txt labelPos env
        pure (emitToLayer env prim, mempty)
  LabelAngle ref txt mOffset -> do
    mPts <- resolveAngleRef ref
    env <- gets esDrawEnv
    case mPts of
      Nothing -> pure (mempty, mempty)
      Just (va, vb, vc) -> do
        let -- Angle bisector: average of normalized arm directions → into the angle
            (bix, biy) = angleBisector va vb vc
            labelDist = 0.5
            Vec2 bx by = vb
            internalPos = Vec2 (bx + bix * labelDist) (by + biy * labelDist)
            internalAnchor = directionToLabelPos bix biy
        case mOffset of
          Nothing -> do
            let prim = RenderLabel internalPos txt internalAnchor env
            pure (emitToLayer env prim, mempty)
          Just (Vec2 dx dy) -> do
            let externalPos = Vec2 (bx + dx) (by + dy)
                -- Anchor based on direction from internal to external
                (edx, edy) = (dx - bix * labelDist, dy - biy * labelDist)
                externalAnchor = directionToLabelPos edx edy
                line = RenderSegment internalPos externalPos env
                label = RenderLabel externalPos txt externalAnchor env
            pure (emitToLayer env line <> emitToLayer env label, mempty)
  LabelAutoPoint ref txt -> do
    mPts <- resolveAngleRef ref
    env <- gets esDrawEnv
    case mPts of
      Nothing -> pure (mempty, mempty)
      Just (va, vb, vc) -> do
        let -- Point label goes opposite to angle bisector (outside the angle)
            (bix, biy) = angleBisector va vb vc
            (nx, ny) = (-bix, -biy)
            Vec2 bx by = vb
            labelVec = Vec2 (bx + nx * 0.15) (by + ny * 0.15)
            anchor = directionToLabelPos nx ny
            prim = RenderLabel labelVec txt anchor env
        pure (emitToLayer env prim, mempty)

-- | Angle bisector at vertex B for angle ABC.
-- Returns a normalized direction pointing toward the angle interior
-- (the side where the arc is drawn).  Computed as the average of
-- the two normalized arm directions from B toward A and from B toward C.
angleBisector :: Vec2 -> Vec2 -> Vec2 -> (Double, Double)
angleBisector (Vec2 ax ay) (Vec2 bx by) (Vec2 cx cy) =
  let -- Normalized direction from B toward A
      dax = ax - bx
      day = ay - by
      lenA = sqrt (dax * dax + day * day)
      (nax, nay)
        | lenA == 0 = (0, 1)
        | otherwise = (dax / lenA, day / lenA)
      -- Normalized direction from B toward C
      dcx = cx - bx
      dcy = cy - by
      lenC = sqrt (dcx * dcx + dcy * dcy)
      (ncx, ncy)
        | lenC == 0 = (0, 1)
        | otherwise = (dcx / lenC, dcy / lenC)
      -- Sum → bisector direction (into the angle)
      sx = nax + ncx
      sy = nay + ncy
      sLen = sqrt (sx * sx + sy * sy)
   in if sLen == 0 then (nay, -nax) else (sx / sLen, sy / sLen)

-- | Map a 2D direction vector to the nearest of 8 label positions.
-- Uses angle sectors of 45 degrees each.
directionToLabelPos :: Double -> Double -> LabelPosition
directionToLabelPos ox oy =
  let angle = atan2 oy ox
      -- Normalize to [0, 2*pi)
      a = if angle < 0 then angle + 2 * pi else angle
      -- Divide into 8 sectors of pi/4 each, starting at -pi/8 from east
      sector = floor ((a + pi / 8) / (pi / 4)) :: Int
   in case sector `mod` 8 of
        0 -> RightOf -- ~0 rad (east)
        1 -> AboveRight -- ~pi/4 (northeast)
        2 -> Above -- ~pi/2 (north)
        3 -> AboveLeft -- ~3pi/4 (northwest)
        4 -> LeftOf -- ~pi (west)
        5 -> BelowLeft -- ~5pi/4 (southwest)
        6 -> Below -- ~6pi/4 (south)
        7 -> BelowRight -- ~7pi/4 (southeast)
        _ -> Above -- impossible, but safe

-- | Convert segment side to a label position based on segment direction.
--
-- Computes the perpendicular direction from the segment and picks the
-- visually appropriate 'LabelPosition'. For horizontal segments this
-- yields 'Above'/'Below'; for vertical segments 'LeftOf'/'RightOf';
-- for diagonals whichever axis dominates.
segmentSideToPosition :: SegmentSide -> Double -> Double -> LabelPosition
segmentSideToPosition side dx dy =
  let (px, py) = case side of
        SegAbove -> (-dy, dx) -- left perpendicular
        SegBelow -> (dy, -dx) -- right perpendicular
   in if abs py > abs px
        then if py > 0 then Above else Below
        else if px < 0 then LeftOf else RightOf

-- -----------------------------------------------------------------
-- Segment ref resolution
-- -----------------------------------------------------------------

resolveSegmentRef :: SegmentRef -> Eval (Maybe (Vec2, Vec2))
resolveSegmentRef = \case
  SegByName name -> do
    segs <- gets esSegments
    pts <- gets esPoints
    pure $ do
      (a, b) <- Map.lookup name segs
      v1 <- Map.lookup a pts
      v2 <- Map.lookup b pts
      Just (v1, v2)
  SegInline a b -> do
    pts <- gets esPoints
    pure $ do
      v1 <- Map.lookup a pts
      v2 <- Map.lookup b pts
      Just (v1, v2)

-- -----------------------------------------------------------------
-- Angle ref resolution
-- -----------------------------------------------------------------

resolveAngleRef :: AngleRef -> Eval (Maybe (Vec2, Vec2, Vec2))
resolveAngleRef (AngleRef a b c) = do
  pts <- gets esPoints
  pure $ do
    va <- Map.lookup a pts
    vb <- Map.lookup b pts
    vc <- Map.lookup c pts
    Just (va, vb, vc)

-- | Compute angle arc parameters from three points (A = arm1, B = vertex, C = arm2).
-- Returns @(startAngle, sweepAngle)@ in radians. The sweep goes from ray BA to ray BC
-- counterclockwise. If the resulting sweep exceeds pi, we flip to always show the
-- shorter arc.
computeAngleArc :: Vec2 -> Vec2 -> Vec2 -> (Double, Double)
computeAngleArc (Vec2 ax ay) (Vec2 bx by) (Vec2 cx cy) =
  let startAngle = atan2 (ay - by) (ax - bx)
      endAngle = atan2 (cy - by) (cx - bx)
      rawSweep = endAngle - startAngle
      -- Normalize to (-pi, pi]
      normalized
        | rawSweep > pi = rawSweep - 2 * pi
        | rawSweep <= (-pi) = rawSweep + 2 * pi
        | otherwise = rawSweep
   in (startAngle, normalized)

-- -----------------------------------------------------------------
-- Modifier blocks
-- -----------------------------------------------------------------

evalModifierBlock :: Modifier -> [Command] -> Eval (RenderResult, RenderLog)
evalModifierBlock modifier children = do
  savedEnv <- gets esDrawEnv
  -- Apply environment modifier
  case modifier of
    EnvMod envMod -> applyEnvMod envMod
    LayerMod layer -> modify' $ \s -> s {esDrawEnv = (esDrawEnv s) {layer = layer}}
    AutoDec _ -> pure ()
  -- Evaluate children
  (childResult, childLog) <- evalCommands children
  -- Restore environment
  modify' $ \s -> s {esDrawEnv = savedEnv}
  -- Apply auto-decorations
  case modifier of
    AutoDec dec -> do
      decorations <- evalAutoDecorator dec childLog savedEnv
      pure (childResult <> decorations, childLog)
    _ -> pure (childResult, childLog)

applyEnvMod :: EnvModifier -> Eval ()
applyEnvMod = \case
  SetColor c -> modify' $ \s -> s {esDrawEnv = (esDrawEnv s) {color = c}}
  SetFill c -> modify' $ \s -> s {esDrawEnv = (esDrawEnv s) {fillColor = Just c}}
  SetDashed -> modify' $ \s -> s {esDrawEnv = (esDrawEnv s) {lineStyle = Dashed}}
  SetThick -> modify' $ \s -> s {esDrawEnv = (esDrawEnv s) {lineWidth = ThickWidth}}
  SetThin -> modify' $ \s -> s {esDrawEnv = (esDrawEnv s) {lineWidth = ThinWidth}}

-- -----------------------------------------------------------------
-- Auto-decorators
-- -----------------------------------------------------------------

evalAutoDecorator :: AutoDecorator -> RenderLog -> DrawEnv -> Eval RenderResult
evalAutoDecorator = \case
  LabelAll pos -> \lg _env -> do
    currentEnv <- gets esDrawEnv
    let fgEnv = currentEnv {layer = Foreground}
        labels =
          [ RenderLabel vec (PlainLabel name) pos fgEnv
          | (name, vec) <- drawnPoints lg
          ]
    pure $ mempty {foreground = labels}
  Axes -> \_lg _env -> do
    pts <- gets esPoints
    pure $ generateAxes pts
  Grid -> \_lg _env -> do
    pts <- gets esPoints
    pure $ generateGrid pts

-- | Generate coordinate axes with integer tick marks
generateAxes :: Map Name Vec2 -> RenderResult
generateAxes pts
  | Map.null pts = mempty
  | otherwise =
      let allVecs = Map.elems pts
          xs = [x | Vec2 x _ <- allVecs]
          ys = [y | Vec2 _ y <- allVecs]
          minX = min 0 (minimum xs) - 1
          maxX = max 0 (maximum xs) + 1
          minY = min 0 (minimum ys) - 1
          maxY = max 0 (maximum ys) + 1
          axisEnv = defaultDrawEnv {layer = Background, color = NamedColor "gray"}
          tickEnv = axisEnv
          -- X axis
          xAxis = RenderAxisLine (Vec2 minX 0) (Vec2 maxX 0) axisEnv
          -- Y axis
          yAxis = RenderAxisLine (Vec2 0 minY) (Vec2 0 maxY) axisEnv
          -- X ticks
          xTicks =
            [ RenderTick (Vec2 (fromIntegral i) 0) (T.pack $ show i) tickEnv
            | i <- [ceiling minX .. floor maxX :: Int]
            , i /= 0
            ]
          -- Y ticks
          yTicks =
            [ RenderTick (Vec2 0 (fromIntegral i)) (T.pack $ show i) tickEnv
            | i <- [ceiling minY .. floor maxY :: Int]
            , i /= 0
            ]
       in mempty {background = [xAxis, yAxis] <> xTicks <> yTicks}

-- | Generate a unit grid covering the scene bounds
generateGrid :: Map Name Vec2 -> RenderResult
generateGrid pts
  | Map.null pts = mempty
  | otherwise =
      let allVecs = Map.elems pts
          xs = [x | Vec2 x _ <- allVecs]
          ys = [y | Vec2 _ y <- allVecs]
          minX = fromIntegral (floor (minimum xs) - 1 :: Int)
          maxX = fromIntegral (ceiling (maximum xs) + 1 :: Int)
          minY = fromIntegral (floor (minimum ys) - 1 :: Int)
          maxY = fromIntegral (ceiling (maximum ys) + 1 :: Int)
          gridEnv = defaultDrawEnv {layer = Background, color = NamedColor "lightgray"}
          -- Vertical grid lines
          vLines =
            [ RenderGridLine (Vec2 x minY) (Vec2 x maxY) gridEnv
            | i <- [round minX .. round maxX :: Int]
            , let x = fromIntegral i
            ]
          -- Horizontal grid lines
          hLines =
            [ RenderGridLine (Vec2 minX y) (Vec2 maxX y) gridEnv
            | i <- [round minY .. round maxY :: Int]
            , let y = fromIntegral i
            ]
       in mempty {background = vLines <> hLines}

-- -----------------------------------------------------------------
-- Math label extraction
-- -----------------------------------------------------------------

-- | Extract all LaTeX math labels from a list of commands.
-- Used by the frontend to pre-render MathJax formulas.
extractMathLabels :: [Command] -> [Text]
extractMathLabels = concatMap go
  where
    go = \case
      Label (LabelAtPoint _ (MathLabel latex) _) -> [latex]
      Label (LabelOnSegment _ (MathLabel latex) _ _) -> [latex]
      Label (LabelAngle _ (MathLabel latex) _) -> [latex]
      Label (LabelAutoPoint _ (MathLabel latex)) -> [latex]
      ModifierBlock _ children -> concatMap go children
      _ -> []

-- -----------------------------------------------------------------
-- Helpers
-- -----------------------------------------------------------------

-- | Desired radius clamped to half the distance to either arm point.
clampRadius :: Double -> Vec2 -> Vec2 -> Vec2 -> Double
clampRadius desired vb va vc =
  let distA = vecDist vb va
      distC = vecDist vb vc
   in min desired (0.5 * min distA distC)

-- | Euclidean distance between two points.
vecDist :: Vec2 -> Vec2 -> Double
vecDist (Vec2 x1 y1) (Vec2 x2 y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in sqrt (dx * dx + dy * dy)

-- | Route a render primitive to the appropriate layer
emitToLayer :: DrawEnv -> RenderPrimitive -> RenderResult
emitToLayer env prim = case layer env of
  Background -> mempty {background = [prim]}
  Main -> mempty {main = [prim]}
  Foreground -> mempty {foreground = [prim]}

