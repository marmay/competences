module Test.Markdown.GeometryTest (geometryTests) where

import Competences.Markdown.Geometry.AST
import Competences.Markdown.Geometry.Eval (evalScene, extractMathLabels)
import Competences.Markdown.Geometry.Palette (resolveFillColor, resolveStrokeColor)
import Competences.Markdown.Geometry.Parser
  ( currentGeometryVersion
  , geometryVersionText
  , isGeometryInfo
  , parseGeometry
  , parseGeometryVersion
  , parseLabelContent
  )
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

geometryTests :: TestTree
geometryTests =
  testGroup
    "Geometry DSL"
    [ parserGroup
    , labelContentGroup
    , extractMathLabelsGroup
    , evalGroup
    , drawPolyParserGroup
    , drawPolyEvalGroup
    , scaleTransformGroup
    , paletteGroup
    , versionGroup
    ]

-- | Parse and assert the result equals expected commands
parsesTo :: String -> Text -> [Command] -> TestTree
parsesTo name input expected = testCase name $ do
  case parseGeometry input of
    Left err -> assertFailure $ "Parse failed: " <> show err
    Right cmds -> cmds @?= expected

-- -----------------------------------------------------------------
-- Parser tests
-- -----------------------------------------------------------------

parserGroup :: TestTree
parserGroup =
  testGroup
    "Parser"
    [ testCase "empty input" $ do
        parseGeometry "" @?= Right []
        parseGeometry "  \n  " @?= Right []
    , parsesTo "defPoint" "defPoint A (1, 2)" [DefPoint "A" (Vec2 1 2)]
    , parsesTo "defPoint negative coords" "defPoint P (-3.5, 0)" [DefPoint "P" (Vec2 (-3.5) 0)]
    , parsesTo "defPointBy midpoint" "defPointBy M (midpoint A B)" [DefPointBy "M" (Midpoint "A" "B")]
    , parsesTo "defPointBy lerp" "defPointBy T (lerp A B 0.25)" [DefPointBy "T" (Lerp "A" "B" 0.25)]
    , parsesTo "defPointBy rotate" "defPointBy R (rotate O 90 P)" [DefPointBy "R" (Rotate "O" 90 "P")]
    , parsesTo
        "defPointBy reflect"
        "defPointBy R (reflect (line A B) P)"
        [DefPointBy "R" (Reflect (LineThrough "A" "B") "P")]
    , parsesTo
        "defPointBy translate"
        "defPointBy T (translate (3, -1) P)"
        [DefPointBy "T" (Translate (Vec2 3 (-1)) "P")]
    , parsesTo "defSegment" "defSegment c A -- B" [DefSegment "c" "A" "B"]
    , parsesTo "drawPoint" "drawPoint A" [Draw (DrawPoint "A")]
    , parsesTo
        "drawPoint labeled"
        "drawPoint A labeled \"A\" below-left"
        [ Draw (DrawPoint "A")
        , Label (LabelAtPoint "A" (PlainLabel "A") BelowLeft)
        ]
    , parsesTo "drawSegment by name" "drawSegment c" [Draw (DrawSegment (SegByName "c"))]
    , parsesTo "drawSegment inline" "drawSegment A -- B" [Draw (DrawSegment (SegInline "A" "B"))]
    , parsesTo
        "drawSegment labeled"
        "drawSegment A -- B labeled \"c\" below 0.4"
        [ Draw (DrawSegment (SegInline "A" "B"))
        , Label (LabelOnSegment (SegInline "A" "B") (PlainLabel "c") SegBelow 0.4)
        ]
    , parsesTo
        "drawSegment labeled default fraction"
        "drawSegment c labeled \"c\" above"
        [ Draw (DrawSegment (SegByName "c"))
        , Label (LabelOnSegment (SegByName "c") (PlainLabel "c") SegAbove 0.5)
        ]
    , parsesTo "labelPoint" "labelPoint A \"A\" above-right" [Label (LabelAtPoint "A" (PlainLabel "A") AboveRight)]
    , parsesTo
        "labelSegment by name"
        "labelSegment c \"c\" below"
        [Label (LabelOnSegment (SegByName "c") (PlainLabel "c") SegBelow 0.5)]
    , parsesTo
        "labelSegment inline with fraction"
        "labelSegment A -- B \"ab\" above 0.3"
        [Label (LabelOnSegment (SegInline "A" "B") (PlainLabel "ab") SegAbove 0.3)]
    , parsesTo
        "modifier block dashed"
        "@dashed {\n  drawSegment A -- B\n}"
        [ModifierBlock (EnvMod SetDashed) [Draw (DrawSegment (SegInline "A" "B"))]]
    , parsesTo
        "modifier block color"
        "@color red {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetColor (NamedColor "red"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "modifier block axes"
        "@axes {\n  drawPoint A\n}"
        [ModifierBlock (AutoDec Axes) [Draw (DrawPoint "A")]]
    , parsesTo
        "modifier block labelAll"
        "@labelAll above {\n  drawPoint A\n  drawPoint B\n}"
        [ ModifierBlock
            (AutoDec (LabelAll Above))
            [Draw (DrawPoint "A"), Draw (DrawPoint "B")]
        ]
    , parsesTo
        "nested modifiers"
        "@axes {\n  @dashed {\n    drawSegment A -- B\n  }\n}"
        [ ModifierBlock
            (AutoDec Axes)
            [ ModifierBlock
                (EnvMod SetDashed)
                [Draw (DrawSegment (SegInline "A" "B"))]
            ]
        ]
    , parsesTo
        "multiple commands"
        "defPoint A (0, 0)\ndefPoint B (4, 0)\ndrawSegment A -- B"
        [ DefPoint "A" (Vec2 0 0)
        , DefPoint "B" (Vec2 4 0)
        , Draw (DrawSegment (SegInline "A" "B"))
        ]
    , parsesTo
        "layer modifiers"
        "@background {\n  drawSegment A -- B\n}"
        [ModifierBlock (LayerMod Background) [Draw (DrawSegment (SegInline "A" "B"))]]
    , parsesTo
        "comma-separated modifiers"
        "@axes, @grid {\n  drawPoint A\n}"
        [ ModifierBlock
            (AutoDec Axes)
            [ ModifierBlock
                (AutoDec Grid)
                [Draw (DrawPoint "A")]
            ]
        ]
    , parsesTo
        "comma-separated three modifiers"
        "@axes, @grid, @dashed {\n  drawSegment A -- B\n}"
        [ ModifierBlock
            (AutoDec Axes)
            [ ModifierBlock
                (AutoDec Grid)
                [ ModifierBlock
                    (EnvMod SetDashed)
                    [Draw (DrawSegment (SegInline "A" "B"))]
                ]
            ]
        ]
    , parsesTo
        "comma-separated with color"
        "@color red, @thick {\n  drawPoint A\n}"
        [ ModifierBlock
            (EnvMod (SetColor (NamedColor "red")))
            [ ModifierBlock
                (EnvMod SetThick)
                [Draw (DrawPoint "A")]
            ]
        ]
    , parsesTo
        "drawAngle"
        "drawAngle A B C"
        [Draw (DrawAngle (AngleRef "A" "B" "C"))]
    , parsesTo
        "drawAngle labeled"
        "drawAngle A B C labeled \"$\\alpha$\""
        [ Draw (DrawAngle (AngleRef "A" "B" "C"))
        , Label (LabelAngle (AngleRef "A" "B" "C") (MathLabel "\\alpha") Nothing)
        ]
    , parsesTo
        "drawRightAngle"
        "drawRightAngle A B C"
        [Draw (DrawRightAngle (AngleRef "A" "B" "C"))]
    , parsesTo
        "labelAngle"
        "labelAngle A B C \"$\\beta$\""
        [Label (LabelAngle (AngleRef "A" "B" "C") (MathLabel "\\beta") Nothing)]
    , parsesTo
        "labelAngle with external offset"
        "labelAngle A B C \"$\\alpha$\" +(1.5, 1.5)"
        [Label (LabelAngle (AngleRef "A" "B" "C") (MathLabel "\\alpha") (Just (Vec2 1.5 1.5)))]
    , parsesTo
        "drawAngle labeled with external offset"
        "drawAngle A B C labeled \"$\\alpha$\" +(1, -1)"
        [ Draw (DrawAngle (AngleRef "A" "B" "C"))
        , Label (LabelAngle (AngleRef "A" "B" "C") (MathLabel "\\alpha") (Just (Vec2 1 (-1))))
        ]
    , parsesTo
        "labelDist modifier block"
        "@labelDist 0.5 {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetLabelDist 0.5)) [Draw (DrawPoint "A")]]
    , parsesTo
        "labelDist comma-separated"
        "@labelDist 0.6, @color red {\n  drawPoint A\n}"
        [ ModifierBlock
            (EnvMod (SetLabelDist 0.6))
            [ ModifierBlock
                (EnvMod (SetColor (NamedColor "red")))
                [Draw (DrawPoint "A")]
            ]
        ]
    , parsesTo
        "fontSize modifier block"
        "@fontSize 0.6 {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetFontSize 0.6)) [Draw (DrawPoint "A")]]
    , parsesTo
        "dotRadius modifier block"
        "@dotRadius 0.15 {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetDotRadius 0.15)) [Draw (DrawPoint "A")]]
    , testCase "parse error" $ do
        let result = parseGeometry "unknownCommand A B"
        assertBool "should fail" (isLeft result)
    ]

-- -----------------------------------------------------------------
-- LabelContent tests
-- -----------------------------------------------------------------

labelContentGroup :: TestTree
labelContentGroup =
  testGroup
    "LabelContent"
    [ testCase "plain text" $
        parseLabelContent "hello" @?= PlainLabel "hello"
    , testCase "math label" $
        parseLabelContent "$\\alpha$" @?= MathLabel "\\alpha"
    , testCase "incomplete — no closing $" $
        parseLabelContent "$incomplete" @?= PlainLabel "$incomplete"
    , testCase "empty math — $$" $
        parseLabelContent "$$" @?= PlainLabel "$$"
    , testCase "math label parses from geometry" $
        parseGeometry "labelPoint A \"$\\beta$\" above"
          @?= Right [Label (LabelAtPoint "A" (MathLabel "\\beta") Above)]
    ]

-- -----------------------------------------------------------------
-- extractMathLabels tests
-- -----------------------------------------------------------------

extractMathLabelsGroup :: TestTree
extractMathLabelsGroup =
  testGroup
    "extractMathLabels"
    [ testCase "no math labels" $
        extractMathLabels [DefPoint "A" (Vec2 0 0), Draw (DrawPoint "A")] @?= []
    , testCase "plain labels ignored" $
        extractMathLabels [Label (LabelAtPoint "A" (PlainLabel "A") Above)] @?= []
    , testCase "math label at point" $
        extractMathLabels [Label (LabelAtPoint "A" (MathLabel "\\alpha") Above)] @?= ["\\alpha"]
    , testCase "math label on segment" $
        extractMathLabels [Label (LabelOnSegment (SegByName "c") (MathLabel "c") SegAbove 0.5)] @?= ["c"]
    , testCase "nested in modifier block" $
        extractMathLabels
          [ ModifierBlock (EnvMod SetDashed)
              [Label (LabelAtPoint "A" (MathLabel "\\gamma") Below)]
          ]
          @?= ["\\gamma"]
    , testCase "math label on angle" $
        extractMathLabels [Label (LabelAngle (AngleRef "A" "B" "C") (MathLabel "\\alpha") Nothing)] @?= ["\\alpha"]
    , testCase "math label on angle with offset" $
        extractMathLabels [Label (LabelAngle (AngleRef "A" "B" "C") (MathLabel "\\alpha") (Just (Vec2 1 1)))] @?= ["\\alpha"]
    , testCase "mixed plain and math" $
        extractMathLabels
          [ Label (LabelAtPoint "A" (PlainLabel "A") Above)
          , Label (LabelAtPoint "B" (MathLabel "\\beta") Below)
          ]
          @?= ["\\beta"]
    ]

-- -----------------------------------------------------------------
-- Eval tests
-- -----------------------------------------------------------------

evalGroup :: TestTree
evalGroup =
  testGroup
    "Eval"
    [ testCase "defPoint + drawPoint" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 2)
                , Draw (DrawPoint "A")
                ]
        case main result of
          [RenderDot (Vec2 x y) _env] -> do
            x @?= 1
            y @?= 2
          other -> assertFailure $ "Expected [RenderDot], got: " <> show other
    , testCase "drawPoint for undefined point produces nothing" $ do
        let result = evalScene [Draw (DrawPoint "A")]
        main result @?= []
    , testCase "defSegment + drawSegment by name" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefSegment "c" "A" "B"
                , Draw (DrawSegment (SegByName "c"))
                ]
        case main result of
          [RenderSegment (Vec2 x1 y1) (Vec2 x2 y2) _env] -> do
            x1 @?= 0
            y1 @?= 0
            x2 @?= 4
            y2 @?= 0
          other -> assertFailure $ "Expected [RenderSegment], got: " <> show other
    , testCase "inline segment" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 3 4)
                , Draw (DrawSegment (SegInline "A" "B"))
                ]
        length (main result) @?= 1
    , testCase "midpoint construction" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPointBy "M" (Midpoint "A" "B")
                , Draw (DrawPoint "M")
                ]
        case main result of
          [RenderDot (Vec2 x y) _] -> do
            x @?= 2
            y @?= 0
          other -> assertFailure $ "Expected [RenderDot], got: " <> show other
    , testCase "lerp construction" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPointBy "T" (Lerp "A" "B" 0.25)
                , Draw (DrawPoint "T")
                ]
        case main result of
          [RenderDot (Vec2 x y) _] -> do
            x @?= 1
            y @?= 0
          other -> assertFailure $ "Expected [RenderDot], got: " <> show other
    , testCase "translate construction" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 2)
                , DefPointBy "B" (Translate (Vec2 3 (-1)) "A")
                , Draw (DrawPoint "B")
                ]
        case main result of
          [RenderDot (Vec2 x y) _] -> do
            x @?= 4
            y @?= 1
          other -> assertFailure $ "Expected [RenderDot], got: " <> show other
    , testCase "rotate construction 90 degrees" $ do
        let result =
              evalScene
                [ DefPoint "O" (Vec2 0 0)
                , DefPoint "P" (Vec2 1 0)
                , DefPointBy "Q" (Rotate "O" 90 "P")
                , Draw (DrawPoint "Q")
                ]
        case main result of
          [RenderDot (Vec2 x y) _] -> do
            assertApprox "x" 0 x
            assertApprox "y" 1 y
          other -> assertFailure $ "Expected [RenderDot], got: " <> show other
    , testCase "reflect construction" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "P" (Vec2 2 3)
                , DefPointBy "Q" (Reflect (LineThrough "A" "B") "P")
                , Draw (DrawPoint "Q")
                ]
        case main result of
          [RenderDot (Vec2 x y) _] -> do
            assertApprox "x" 2 x
            assertApprox "y" (-3) y
          other -> assertFailure $ "Expected [RenderDot], got: " <> show other
    , testCase "label at point" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 2)
                , Label (LabelAtPoint "A" (PlainLabel "A") Above)
                ]
        case main result of
          [RenderLabel _ _ lbl pos _] -> do
            lbl @?= PlainLabel "A"
            pos @?= Above
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "dashed modifier" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , ModifierBlock
                    (EnvMod SetDashed)
                    [Draw (DrawSegment (SegInline "A" "B"))]
                ]
        case main result of
          [RenderSegment _ _ env] ->
            lineStyle env @?= Dashed
          other -> assertFailure $ "Expected dashed segment, got: " <> show other
    , testCase "color modifier" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , ModifierBlock
                    (EnvMod (SetColor (NamedColor "red")))
                    [Draw (DrawPoint "A")]
                ]
        case main result of
          [RenderDot _ env] -> do
            lineColor env @?= NamedColor "red"
            textColor env @?= NamedColor "red"
          other -> assertFailure $ "Expected colored dot, got: " <> show other
    , testCase "environment scoping" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 1 1)
                , ModifierBlock
                    (EnvMod SetDashed)
                    [Draw (DrawSegment (SegInline "A" "B"))]
                , Draw (DrawSegment (SegInline "A" "B"))
                ]
        case main result of
          [RenderSegment _ _ env1, RenderSegment _ _ env2] -> do
            lineStyle env1 @?= Dashed
            lineStyle env2 @?= Solid
          other -> assertFailure $ "Expected two segments, got: " <> show other
    , testCase "flat namespace — defPoint inside block visible outside" $ do
        let result =
              evalScene
                [ ModifierBlock
                    (EnvMod (SetColor (NamedColor "red")))
                    [DefPoint "P" (Vec2 1 2)]
                , Draw (DrawPoint "P")
                ]
        length (main result) @?= 1
    , testCase "layer routing — background" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 1 1)
                , ModifierBlock
                    (LayerMod Background)
                    [Draw (DrawSegment (SegInline "A" "B"))]
                ]
        length (background result) @?= 1
        main result @?= []
    , testCase "layer routing — foreground" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , ModifierBlock
                    (LayerMod Foreground)
                    [Draw (DrawPoint "A")]
                ]
        length (foreground result) @?= 1
        main result @?= []
    , testCase "labelAll auto-decorator" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , ModifierBlock
                    (AutoDec (LabelAll Above))
                    [ Draw (DrawPoint "A")
                    , Draw (DrawPoint "B")
                    ]
                ]
        -- Points go to main
        length (main result) @?= 2
        -- Auto-labels go to foreground
        length (foreground result) @?= 2
    , testCase "axes auto-decorator" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 3)
                , ModifierBlock (AutoDec Axes) [Draw (DrawPoint "A")]
                ]
        -- Axes go to background
        assertBool "background should have axis elements" (not $ null $ background result)
    , testCase "grid auto-decorator" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 2 2)
                , ModifierBlock (AutoDec Grid) [Draw (DrawPoint "A")]
                ]
        assertBool "background should have grid lines" (not $ null $ background result)
    , testCase "angle arc on right triangle" $ do
        -- Triangle with right angle at B: A=(1,0), B=(0,0), C=(0,1)
        -- Angle ABC: ray BA along +x, ray BC along +y
        -- startAngle = atan2(0-0, 1-0) = 0, endAngle = atan2(1-0, 0-0) = pi/2
        -- sweep = pi/2
        -- radius: min(1.0, 0.5 * min(1, 1)) = 0.5
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , Draw (DrawAngle (AngleRef "A" "B" "C"))
                ]
        case main result of
          [RenderAngleArc (Vec2 vx vy) startA sweepA radius _env] -> do
            assertApprox "vx" 0 vx
            assertApprox "vy" 0 vy
            assertApprox "startAngle" 0 startA
            assertApprox "sweepAngle" (pi / 2) sweepA
            assertApprox "radius" 0.5 radius
          other -> assertFailure $ "Expected [RenderAngleArc], got: " <> show other
    , testCase "right angle arc" $ do
        -- radius: min(0.7, 0.5 * min(1, 1)) = 0.5
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , Draw (DrawRightAngle (AngleRef "A" "B" "C"))
                ]
        case main result of
          [RenderRightAngle (Vec2 vx vy) startA sweepA radius _env] -> do
            assertApprox "vx" 0 vx
            assertApprox "vy" 0 vy
            assertApprox "startAngle" 0 startA
            assertApprox "sweepAngle" (pi / 2) sweepA
            assertApprox "radius" 0.5 radius
          other -> assertFailure $ "Expected [RenderRightAngle], got: " <> show other
    , testCase "angle arc flips when sweep > pi" $ do
        -- A=(0,1), B=(0,0), C=(1,0): CCW from +y to +x is 3pi/2, should flip to -pi/2
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 1)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 1 0)
                , Draw (DrawAngle (AngleRef "A" "B" "C"))
                ]
        case main result of
          [RenderAngleArc _ startA sweepA _ _] -> do
            assertApprox "startAngle" (pi / 2) startA
            assertApprox "sweepAngle" (-(pi / 2)) sweepA
          other -> assertFailure $ "Expected [RenderAngleArc], got: " <> show other
    , testCase "angle label placement" $ do
        -- A=(1,0), B=(0,0), C=(0,1)
        -- bisector at B = normalize((1,0)+(0,1)) = (1,1)/sqrt(2) -> into the angle
        -- labelDist = 1.0 (default), angle natural distance = 0.75
        -- label at (0.75/sqrt(2), 0.75/sqrt(2))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , Label (LabelAngle (AngleRef "A" "B" "C") (PlainLabel "a") Nothing)
                ]
        case main result of
          [RenderLabel (Vec2 bx by) (Vec2 ox oy) lbl pos _env] -> do
            lbl @?= PlainLabel "a"
            let dist = 0.75 :: Double
                invSqrt2 = 1 / sqrt 2
            -- base is the vertex
            assertApprox "bx" 0 bx
            assertApprox "by" 0 by
            -- offset is bisector * dist
            assertApprox "ox" (invSqrt2 * dist) ox
            assertApprox "oy" (invSqrt2 * dist) oy
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "angle arc radius clamped for short edges" $ do
        -- Edges of length 1.0: min(1.0, 0.5*1.0) = 0.5
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , Draw (DrawAngle (AngleRef "A" "B" "C"))
                ]
        case main result of
          [RenderAngleArc _ _ _ radius _] ->
            assertApprox "clamped radius" 0.5 radius
          other -> assertFailure $ "Expected [RenderAngleArc], got: " <> show other
    , testCase "angle arc radius unclamped for long edges" $ do
        -- Edges of length 4.0 and 3.0: min(1.0, 0.5*3.0) = 1.0
        let result =
              evalScene
                [ DefPoint "A" (Vec2 4 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 3)
                , Draw (DrawAngle (AngleRef "A" "B" "C"))
                ]
        case main result of
          [RenderAngleArc _ _ _ radius _] ->
            assertApprox "unclamped radius" 1.0 radius
          other -> assertFailure $ "Expected [RenderAngleArc], got: " <> show other
    , testCase "right angle radius clamped for short edges" $ do
        -- Edges of length 0.6: min(0.7, 0.5*0.6) = 0.3
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0.6 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 0.6)
                , Draw (DrawRightAngle (AngleRef "A" "B" "C"))
                ]
        case main result of
          [RenderRightAngle _ _ _ radius _] ->
            assertApprox "clamped right angle radius" 0.3 radius
          other -> assertFailure $ "Expected [RenderRightAngle], got: " <> show other
    , testCase "external angle label with offset" $ do
        -- A=(4,0), B=(0,0), C=(0,3), offset +(1.5, 1.5)
        -- outward at B = (1,1)/sqrt(2), inward = (-1,-1)/sqrt(2)
        -- internalPos = (0 + (-1/sqrt2)*0.5, 0 + (-1/sqrt2)*0.5)
        -- externalPos = (0 + 1.5, 0 + 1.5)
        -- Should produce a leader line (segment) + label
        let result =
              evalScene
                [ DefPoint "A" (Vec2 4 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 3)
                , Label (LabelAngle (AngleRef "A" "B" "C") (MathLabel "\\alpha") (Just (Vec2 1.5 1.5)))
                ]
        case main result of
          [RenderSegment _ (Vec2 ex ey) _, RenderLabel (Vec2 lx ly) (Vec2 lox loy) lbl _ _] -> do
            lbl @?= MathLabel "\\alpha"
            assertApprox "external x" 1.5 ex
            assertApprox "external y" 1.5 ey
            assertApprox "label x" 1.5 lx
            assertApprox "label y" 1.5 ly
            -- External labels have zero offset
            assertApprox "label offset x" 0 lox
            assertApprox "label offset y" 0 loy
          other -> assertFailure $ "Expected [RenderSegment, RenderLabel], got: " <> show other
    , testCase "internal angle label (no offset) unchanged" $ do
        -- Same triangle as above but no offset
        let result =
              evalScene
                [ DefPoint "A" (Vec2 4 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 3)
                , Label (LabelAngle (AngleRef "A" "B" "C") (PlainLabel "a") Nothing)
                ]
        case main result of
          [RenderLabel _ _ lbl _ _] ->
            lbl @?= PlainLabel "a"
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "fontSize scoping" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 1 1)
                , ModifierBlock
                    (EnvMod (SetFontSize 0.6))
                    [Draw (DrawPoint "A")]
                , Draw (DrawPoint "B")
                ]
        case main result of
          [RenderDot _ env1, RenderDot _ env2] -> do
            fontSize env1 @?= 0.6
            fontSize env2 @?= 0.45
          other -> assertFailure $ "Expected two dots, got: " <> show other
    , testCase "dotRadius scoping" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 1 1)
                , ModifierBlock
                    (EnvMod (SetDotRadius 0.2))
                    [Draw (DrawPoint "A")]
                , Draw (DrawPoint "B")
                ]
        case main result of
          [RenderDot _ env1, RenderDot _ env2] -> do
            dotRadius env1 @?= 0.2
            dotRadius env2 @?= 0.1
          other -> assertFailure $ "Expected two dots, got: " <> show other
    , testCase "full example parse + eval" $ do
        let input =
              "defPoint A (0, 0)\n\
              \defPoint B (4, 0)\n\
              \defPoint C (2, 3)\n\
              \defPointBy M (midpoint A B)\n\
              \defSegment c A -- B\n\
              \drawSegment c\n\
              \drawSegment A -- C\n\
              \drawSegment B -- C\n\
              \drawPoint M\n"
        case parseGeometry input of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let result = evalScene cmds
            -- 3 segments + 1 point
            length (main result) @?= 4
    ]

-- -----------------------------------------------------------------
-- drawPoly parser tests
-- -----------------------------------------------------------------

drawPolyParserGroup :: TestTree
drawPolyParserGroup =
  testGroup
    "drawPoly Parser"
    [ testCase "minimal triangle — 3 segments + fill" $ do
        case parseGeometry "drawPoly A -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            -- Should have: DrawFilledPolygon + 3 DrawSegment (no point/label cmds)
            let fills = [ns | Draw (DrawFilledPolygon ns) <- cmds]
                segs = [s | Draw (DrawSegment s) <- cmds]
            fills @?= [["A", "B", "C"]]
            length segs @?= 3
    , testCase "with point decoration — adds DrawPoint + LabelAutoPoint" $ do
        case parseGeometry "drawPoly A [point \"A\"] -- B [point \"B\"] -- C [point \"C\"]" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let dots = [n | Draw (DrawPoint n) <- cmds]
                autoLabels = [lbl | Label (LabelAutoPoint _ lbl) <- cmds]
            length dots @?= 3
            length autoLabels @?= 3
    , testCase "with explicit label position — uses LabelAtPoint" $ do
        case parseGeometry "drawPoly A [point \"A\" below] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let atLabels = [pos | Label (LabelAtPoint _ _ pos) <- cmds]
            atLabels @?= [Below]
    , testCase "with segment decoration — adds LabelOnSegment with SegBelow default" $ do
        case parseGeometry "drawPoly A -[segment \"$c$\"]- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let segLabels = [(lbl, side) | Label (LabelOnSegment _ lbl side _) <- cmds]
            segLabels @?= [(MathLabel "c", SegBelow)]
    , testCase "with segment decoration — explicit side" $ do
        case parseGeometry "drawPoly A -[segment \"$c$\" left]- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let segLabels = [side | Label (LabelOnSegment _ _ side _) <- cmds]
            segLabels @?= [SegAbove]
    , testCase "with angle decoration" $ do
        case parseGeometry "drawPoly A [angle \"$\\\\alpha$\"] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let angles = [ref | Draw (DrawAngle ref) <- cmds]
                angleLbls = [lbl | Label (LabelAngle _ lbl _) <- cmds]
            length angles @?= 1
            length angleLbls @?= 1
    , testCase "with rightAngle decoration" $ do
        case parseGeometry "drawPoly A [rightAngle] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let rightAngles = [ref | Draw (DrawRightAngle ref) <- cmds]
            length rightAngles @?= 1
    , testCase "inline coordinates — generates DefPoint" $ do
        case parseGeometry "drawPoly (0, 0) [point \"A\"] -- (4, 0) -- (0, 3)" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let defs = [n | DefPoint n _ <- cmds]
            length defs @?= 3
    , testCase "with close and closing edge decoration" $ do
        case parseGeometry "drawPoly A -- B -- C -[segment \"$b$\"]- close" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let segLabels = [lbl | Label (LabelOnSegment _ lbl _ _) <- cmds]
            segLabels @?= [MathLabel "b"]
    , testCase "fewer than 3 vertices fails" $ do
        let result = parseGeometry "drawPoly A -- B"
        assertBool "should fail" (isLeft result)
    , testCase "vertex decoration with @modifier" $ do
        case parseGeometry "drawPoly A [@color red { point \"A\" }] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            -- The point decoration should be wrapped in a ModifierBlock
            let colorBlocks = [c | ModifierBlock (EnvMod (SetColor (NamedColor c))) _ <- cmds]
            colorBlocks @?= ["red"]
    , testCase "edge decoration with @modifier" $ do
        case parseGeometry "drawPoly A -[@dashed { segment \"c\" }]- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            -- The segment label should be wrapped in a ModifierBlock with dashed
            let dashedBlocks = [() | ModifierBlock (EnvMod SetDashed) _ <- cmds]
            length dashedBlocks @?= 1
    , testCase "mixed modified and plain vertex decorations" $ do
        case parseGeometry "drawPoly A [@color red { point \"A\" }, angle \"$\\\\alpha$\"] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            -- Should have a color-wrapped point + an angle
            let colorBlocks = [c | ModifierBlock (EnvMod (SetColor (NamedColor c))) _ <- cmds]
                angles = [ref | Draw (DrawAngle ref) <- cmds]
            colorBlocks @?= ["red"]
            length angles @?= 1
    , testCase "label with explicit position — LabelAtPoint only, no DrawPoint" $ do
        case parseGeometry "drawPoly A [label \"A\" below] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let atLabels = [(lbl, pos) | Label (LabelAtPoint _ lbl pos) <- cmds]
                dots = [n | Draw (DrawPoint n) <- cmds]
            atLabels @?= [(PlainLabel "A", Below)]
            dots @?= []
    , testCase "label with auto position — LabelAutoPoint only, no DrawPoint" $ do
        case parseGeometry "drawPoly A [label \"A\"] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let autoLabels = [lbl | Label (LabelAutoPoint _ lbl) <- cmds]
                dots = [n | Draw (DrawPoint n) <- cmds]
            autoLabels @?= [PlainLabel "A"]
            dots @?= []
    , testCase "label mixed with other decorations" $ do
        case parseGeometry "drawPoly A [label \"A\" below, angle \"$\\\\alpha$\"] -- B -- C" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            let atLabels = [pos | Label (LabelAtPoint _ _ pos) <- cmds]
                angles = [ref | Draw (DrawAngle ref) <- cmds]
                dots = [n | Draw (DrawPoint n) <- cmds]
            atLabels @?= [Below]
            length angles @?= 1
            dots @?= []
    ]

-- -----------------------------------------------------------------
-- drawPoly eval tests
-- -----------------------------------------------------------------

drawPolyEvalGroup :: TestTree
drawPolyEvalGroup =
  testGroup
    "drawPoly Eval"
    [ testCase "LabelAutoPoint on right triangle — correct position" $ do
        -- CCW triangle: A=(0,0), B=(4,0), C=(0,3)
        -- At vertex A=(0,0), prev=C=(0,3), succ=B=(4,0)
        -- Edge C->A: dir=(0,-3), right perp = (-3,0)/3 = (-1,0)
        -- Edge A->B: dir=(4,0), right perp = (0,-4)/4 = (0,-1)
        -- Average outward: (-1,-1), normalized: (-1/sqrt(2), -1/sqrt(2))
        -- Label offset: 0.50 along outward (0.50 * labelDist 1.0)
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 3)
                , Label (LabelAutoPoint (AngleRef "C" "A" "B") (PlainLabel "A"))
                ]
        case main result of
          [RenderLabel (Vec2 bx by) (Vec2 ox oy) lbl pos _] -> do
            lbl @?= PlainLabel "A"
            let invSqrt2 = 1 / sqrt 2
            -- base is the vertex
            assertApprox "bx" 0 bx
            assertApprox "by" 0 by
            -- offset is outward * labelDist
            assertApprox "ox" ((-invSqrt2) * 0.50) ox
            assertApprox "oy" ((-invSqrt2) * 0.50) oy
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "LabelAutoPoint at top vertex — centered" $ do
        -- Equilateral-ish: A=(0,0), B=(4,0), C=(2,3)
        -- At vertex C, prev=B=(4,0), succ=A=(0,0)
        -- Edge B->C: dir=(-2,3), outward normal = (3,2)/sqrt(13)
        -- Edge C->A: dir=(-2,-3), outward normal = (-3,2)/sqrt(13)
        -- Average: (0,4)/sqrt(13) -> Above
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 2 3)
                , Label (LabelAutoPoint (AngleRef "B" "C" "A") (PlainLabel "C"))
                ]
        case main result of
          [RenderLabel (Vec2 _bx by) (Vec2 _ox oy) _ pos _] -> do
            pos @?= Center
            -- Combined y should be offset upward from vertex
            assertBool "by + oy > 3" (by + oy > 3)
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "DrawFilledPolygon with fill active — renders to background" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 3)
                , ModifierBlock
                    (EnvMod (SetFillColor (NamedColor "red")))
                    [Draw (DrawFilledPolygon ["A", "B", "C"])]
                ]
        case background result of
          [RenderFilledPolygon vs env] -> do
            length vs @?= 3
            fillColor env @?= Just (NamedColor "red")
          other -> assertFailure $ "Expected [RenderFilledPolygon], got: " <> show other
    , testCase "DrawFilledPolygon without fill — nothing" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 3)
                , Draw (DrawFilledPolygon ["A", "B", "C"])
                ]
        background result @?= []
        main result @?= []
    , testCase "fill modifier scoping" $ do
        -- fill inside block, outside should be Nothing again
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 3)
                , ModifierBlock
                    (EnvMod (SetFillColor (NamedColor "blue")))
                    [Draw (DrawFilledPolygon ["A", "B", "C"])]
                , Draw (DrawFilledPolygon ["A", "B", "C"])
                ]
        -- First fill goes to background, second is a no-op
        length (background result) @?= 1
    , testCase "fillColor block parser" $ do
        case parseGeometry "@fillColor red {\n  drawPoly A -- B -- C\n}" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            -- Should be wrapped in ModifierBlock with SetFillColor
            case cmds of
              [ModifierBlock (EnvMod (SetFillColor (NamedColor "red"))) _children] -> pure ()
              other -> assertFailure $ "Expected fillColor modifier block, got: " <> show other
    , testCase "LabelAutoPoint isosceles right — vertex A=(0,0) 90deg" $ do
        -- Triangle A=(0,0), B=(4,0), C=(0,4), right angle at A
        -- AngleRef "C" "A" "B": vertex A, arms toward C and B
        -- Bisector = (1/sqrt(2), 1/sqrt(2)) -> outward = (-1/sqrt(2), -1/sqrt(2))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , Label (LabelAutoPoint (AngleRef "C" "A" "B") (PlainLabel "A"))
                ]
        case main result of
          [RenderLabel (Vec2 bx by) (Vec2 ox oy) lbl pos _] -> do
            lbl @?= PlainLabel "A"
            let invSqrt2 = 1 / sqrt 2
            assertApprox "bx" 0 bx
            assertApprox "by" 0 by
            assertApprox "ox" ((-invSqrt2) * 0.50) ox
            assertApprox "oy" ((-invSqrt2) * 0.50) oy
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "LabelAutoPoint isosceles right — vertex B=(4,0) 45deg" $ do
        -- AngleRef "A" "B" "C": vertex B, arms toward A and C
        -- Bisector = (-cos(pi/8), sin(pi/8)) -> outward = (cos(pi/8), -sin(pi/8))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , Label (LabelAutoPoint (AngleRef "A" "B" "C") (PlainLabel "B"))
                ]
        case main result of
          [RenderLabel (Vec2 bx by) (Vec2 ox oy) lbl pos _] -> do
            lbl @?= PlainLabel "B"
            pos @?= Center
            let cospi8 = cos (pi / 8)
                sinpi8 = sin (pi / 8)
            assertApprox "bx" 4 bx
            assertApprox "by" 0 by
            assertApprox "ox" (cospi8 * 0.50) ox
            assertApprox "oy" ((-sinpi8) * 0.50) oy
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "LabelAutoPoint isosceles right — vertex C=(0,4) 45deg" $ do
        -- AngleRef "B" "C" "A": vertex C, arms toward B and A
        -- Bisector = (sin(pi/8), -cos(pi/8)) -> outward = (-sin(pi/8), cos(pi/8))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , Label (LabelAutoPoint (AngleRef "B" "C" "A") (PlainLabel "C"))
                ]
        case main result of
          [RenderLabel (Vec2 bx by) (Vec2 ox oy) lbl pos _] -> do
            lbl @?= PlainLabel "C"
            pos @?= Center
            let cospi8 = cos (pi / 8)
                sinpi8 = sin (pi / 8)
            assertApprox "bx" 0 bx
            assertApprox "by" 4 by
            assertApprox "ox" ((-sinpi8) * 0.50) ox
            assertApprox "oy" (cospi8 * 0.50) oy
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "labelDist affects LabelAutoPoint" $ do
        -- Same triangle as isosceles right test but with custom labelDist 1.5
        -- auto-point distance = 0.50 * 1.5 = 0.75
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , ModifierBlock
                    (EnvMod (SetLabelDist 1.5))
                    [Label (LabelAutoPoint (AngleRef "C" "A" "B") (PlainLabel "A"))]
                ]
        case main result of
          [RenderLabel (Vec2 bx by) (Vec2 ox oy) lbl pos _] -> do
            lbl @?= PlainLabel "A"
            let invSqrt2 = 1 / sqrt 2
                dist = 0.50 * 1.5
            assertApprox "bx" 0 bx
            assertApprox "by" 0 by
            assertApprox "ox" ((-invSqrt2) * dist) ox
            assertApprox "oy" ((-invSqrt2) * dist) oy
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "labelDist affects LabelAngle" $ do
        -- A=(1,0), B=(0,0), C=(0,1) with custom labelDist 1.5
        -- angle distance = 0.75 * 1.5 = 1.125
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , ModifierBlock
                    (EnvMod (SetLabelDist 1.5))
                    [Label (LabelAngle (AngleRef "A" "B" "C") (PlainLabel "a") Nothing)]
                ]
        case main result of
          [RenderLabel (Vec2 bx by) (Vec2 ox oy) lbl pos _] -> do
            lbl @?= PlainLabel "a"
            let invSqrt2 = 1 / sqrt 2
                dist = 0.75 * 1.5
            -- base is the vertex (0,0)
            assertApprox "bx" 0 bx
            assertApprox "by" 0 by
            -- offset is bisector * dist
            assertApprox "ox" (invSqrt2 * dist) ox
            assertApprox "oy" (invSqrt2 * dist) oy
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "extractMathLabels — LabelAutoPoint with MathLabel" $
        extractMathLabels [Label (LabelAutoPoint (AngleRef "A" "B" "C") (MathLabel "\\beta"))]
          @?= ["\\beta"]
    , testCase "extractMathLabels — LabelAutoPoint with PlainLabel" $
        extractMathLabels [Label (LabelAutoPoint (AngleRef "A" "B" "C") (PlainLabel "B"))]
          @?= []
    ]

-- -----------------------------------------------------------------
-- Scale transform tests
-- -----------------------------------------------------------------

scaleTransformGroup :: TestTree
scaleTransformGroup =
  testGroup
    "Scale Transform"
    [ -- Parser tests
      parsesTo
        "@scale parser — auto-centroid"
        "@scale 0.95 {\n  drawPoint A\n}"
        [ModifierBlock (TransformMod (Scale 0.95 Nothing)) [Draw (DrawPoint "A")]]
    , parsesTo
        "@scale parser — explicit center"
        "@scale 2.0 M {\n  drawPoint A\n}"
        [ModifierBlock (TransformMod (Scale 2.0 (Just "M"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@scale comma-separated with fillColor"
        "@fillColor blue, @scale 0.95 {\n  drawPoly A -- B -- C\n}"
        [ ModifierBlock
            (EnvMod (SetFillColor (NamedColor "blue")))
            [ ModifierBlock
                (TransformMod (Scale 0.95 Nothing))
                -- drawPoly desugars into fill + segments
                [Draw (DrawFilledPolygon ["A", "B", "C"])
                , Draw (DrawSegment (SegInline "A" "B"))
                , Draw (DrawSegment (SegInline "B" "C"))
                , Draw (DrawSegment (SegInline "C" "A"))
                ]
            ]
        ]
    , -- Eval tests: identity scale
      testCase "scale 1.0 is identity" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 2)
                , DefPoint "B" (Vec2 3 4)
                , ModifierBlock
                    (TransformMod (Scale 1.0 Nothing))
                    [ Draw (DrawPoint "A")
                    , Draw (DrawPoint "B")
                    ]
                ]
        case main result of
          [RenderDot (Vec2 ax ay) _, RenderDot (Vec2 bx by) _] -> do
            assertApprox "ax" 1 ax
            assertApprox "ay" 2 ay
            assertApprox "bx" 3 bx
            assertApprox "by" 4 by
          other -> assertFailure $ "Expected two dots, got: " <> show other
    , testCase "scale 0.5 auto-centroid — two points" $ do
        -- Points at (0,0) and (4,0), centroid = (2,0)
        -- After scale 0.5: (0,0) -> (2 + 0.5*(0-2), 0) = (1,0)
        --                  (4,0) -> (2 + 0.5*(4-2), 0) = (3,0)
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , ModifierBlock
                    (TransformMod (Scale 0.5 Nothing))
                    [ Draw (DrawPoint "A")
                    , Draw (DrawPoint "B")
                    ]
                ]
        case main result of
          [RenderDot (Vec2 ax ay) _, RenderDot (Vec2 bx by) _] -> do
            assertApprox "ax" 1 ax
            assertApprox "ay" 0 ay
            assertApprox "bx" 3 bx
            assertApprox "by" 0 by
          other -> assertFailure $ "Expected two dots, got: " <> show other
    , testCase "scale 2.0 with explicit center" $ do
        -- Center at M=(1,1), point A=(2,2)
        -- After scale 2.0: (1 + 2*(2-1), 1 + 2*(2-1)) = (3,3)
        let result =
              evalScene
                [ DefPoint "M" (Vec2 1 1)
                , DefPoint "A" (Vec2 2 2)
                , ModifierBlock
                    (TransformMod (Scale 2.0 (Just "M")))
                    [Draw (DrawPoint "A")]
                ]
        case main result of
          [RenderDot (Vec2 ax ay) _] -> do
            assertApprox "ax" 3 ax
            assertApprox "ay" 3 ay
          other -> assertFailure $ "Expected one dot, got: " <> show other
    , testCase "scale applies to segments" $ do
        -- Points at (0,0) and (4,0), centroid = (1,0) for segment
        -- After scale 0.5 around centroid (1,0):
        --   (0,0) -> (1 + 0.5*(-1), 0) = (0.5, 0)
        --   (4,0) -> (1 + 0.5*(3), 0) = (2.5, 0)
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , ModifierBlock
                    (TransformMod (Scale 0.5 Nothing))
                    [Draw (DrawSegment (SegInline "A" "B"))]
                ]
        case main result of
          [RenderSegment (Vec2 ax ay) (Vec2 bx by) _] -> do
            assertApprox "ax" 1 ax
            assertApprox "ay" 0 ay
            assertApprox "bx" 3 bx
            assertApprox "by" 0 by
          other -> assertFailure $ "Expected one segment, got: " <> show other
    , testCase "scale applies to filled polygon" $ do
        -- Triangle (0,0), (6,0), (0,6), centroid = (2,2)
        -- Scale 0.5 around centroid:
        --   (0,0) -> (2+0.5*(-2), 2+0.5*(-2)) = (1,1)
        --   (6,0) -> (2+0.5*(4), 2+0.5*(-2)) = (4,1)
        --   (0,6) -> (2+0.5*(-2), 2+0.5*(4)) = (1,4)
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 6 0)
                , DefPoint "C" (Vec2 0 6)
                , ModifierBlock
                    (EnvMod (SetFillColor (NamedColor "blue")))
                    [ ModifierBlock
                        (TransformMod (Scale 0.5 Nothing))
                        [Draw (DrawFilledPolygon ["A", "B", "C"])]
                    ]
                ]
        case background result of
          [RenderFilledPolygon [Vec2 ax ay, Vec2 bx by, Vec2 cx cy] _] -> do
            assertApprox "ax" 1 ax
            assertApprox "ay" 1 ay
            assertApprox "bx" 4 bx
            assertApprox "by" 1 by
            assertApprox "cx" 1 cx
            assertApprox "cy" 4 cy
          other -> assertFailure $ "Expected filled polygon, got: " <> show other
    , testCase "scale nesting composes" $ do
        -- Point at (4,0). Outer scale 0.5 auto, inner scale 0.5 auto.
        -- Inner: centroid = (4,0), scale 0.5 -> (4,0) (single point stays)
        -- Outer: centroid of result = (4,0), scale 0.5 -> (4,0)
        -- With two points:
        -- Points at (0,0) and (4,0)
        -- Inner scale 0.5: centroid = (2,0), (0,0)->(1,0), (4,0)->(3,0)
        -- Outer scale 0.5: centroid = (2,0), (1,0)->(1.5,0), (3,0)->(2.5,0)
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , ModifierBlock
                    (TransformMod (Scale 0.5 Nothing))
                    [ ModifierBlock
                        (TransformMod (Scale 0.5 Nothing))
                        [ Draw (DrawPoint "A")
                        , Draw (DrawPoint "B")
                        ]
                    ]
                ]
        case main result of
          [RenderDot (Vec2 ax ay) _, RenderDot (Vec2 bx by) _] -> do
            assertApprox "ax" 1.5 ax
            assertApprox "ay" 0 ay
            assertApprox "bx" 2.5 bx
            assertApprox "by" 0 by
          other -> assertFailure $ "Expected two dots, got: " <> show other
    , testCase "scale does not affect angle arc radius" $ do
        -- Right triangle with edges of length 1: A=(1,0), B=(0,0), C=(0,1)
        -- Angle arc radius: min(1.0, 0.5*1) = 0.5
        -- Scale 2.0 around origin (B): radius should stay 0.5 (presentation param)
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , ModifierBlock
                    (TransformMod (Scale 2.0 (Just "B")))
                    [Draw (DrawAngle (AngleRef "A" "B" "C"))]
                ]
        case main result of
          [RenderAngleArc (Vec2 vx vy) _ _ radius _] -> do
            assertApprox "vx" 0 vx
            assertApprox "vy" 0 vy
            assertApprox "radius" 0.5 radius
          other -> assertFailure $ "Expected angle arc, got: " <> show other
    , testCase "scale does not affect points outside block" $ do
        -- Point A defined, drawn inside scale block and outside
        let result =
              evalScene
                [ DefPoint "A" (Vec2 2 0)
                , DefPoint "B" (Vec2 4 0)
                , ModifierBlock
                    (TransformMod (Scale 0.5 Nothing))
                    [Draw (DrawPoint "A")]
                , Draw (DrawPoint "A")
                ]
        case main result of
          [RenderDot (Vec2 scaledX _) _, RenderDot (Vec2 normalX _) _] -> do
            -- Inside block: centroid of single point (2,0), scale doesn't move it
            assertApprox "scaledX" 2 scaledX
            -- Outside block: unaffected
            assertApprox "normalX" 2 normalX
          other -> assertFailure $ "Expected two dots, got: " <> show other
    , testCase "scale preserves label offset (does not scale presentation params)" $ do
        -- Point at (1,0), labeled above. Scale 2.0 around origin.
        -- Base should scale: (1,0) -> (2,0)
        -- Offset should NOT scale (font-size-dependent).
        -- Compare with unscaled version to verify offset is identical.
        let unscaled =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , Label (LabelAtPoint "A" (PlainLabel "A") Above)
                ]
            scaled =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "O" (Vec2 0 0)
                , ModifierBlock
                    (TransformMod (Scale 2.0 (Just "O")))
                    [Label (LabelAtPoint "A" (PlainLabel "A") Above)]
                ]
        case (main unscaled, main scaled) of
          ( [RenderLabel (Vec2 ubx uby) (Vec2 uox uoy) _ _ _]
            , [RenderLabel (Vec2 sbx sby) (Vec2 sox soy) _ _ _]
            ) -> do
              -- Base scales: (1,0) * 2 around origin = (2,0)
              assertApprox "unscaled base x" 1 ubx
              assertApprox "unscaled base y" 0 uby
              assertApprox "scaled base x" 2 sbx
              assertApprox "scaled base y" 0 sby
              -- Offset stays the same
              assertApprox "offset x unchanged" uox sox
              assertApprox "offset y unchanged" uoy soy
          other -> assertFailure $ "Expected labels, got: " <> show other
    , testCase "scale does not affect right angle radius" $ do
        -- Right triangle: A=(1,0), B=(0,0), C=(0,1)
        -- Right angle radius: min(0.7, 0.5*1) = 0.5
        -- Scale 2.0 around origin: radius should stay 0.5
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , ModifierBlock
                    (TransformMod (Scale 2.0 (Just "B")))
                    [Draw (DrawRightAngle (AngleRef "A" "B" "C"))]
                ]
        case main result of
          [RenderRightAngle (Vec2 vx vy) _ _ radius _] -> do
            assertApprox "vx" 0 vx
            assertApprox "vy" 0 vy
            assertApprox "radius" 0.5 radius
          other -> assertFailure $ "Expected right angle, got: " <> show other
    ]

-- -----------------------------------------------------------------
-- Palette tests
-- -----------------------------------------------------------------

paletteGroup :: TestTree
paletteGroup =
  testGroup
    "Palette"
    [ -- Resolve functions
      testCase "resolveStrokeColor — palette entry" $
        resolveStrokeColor (NamedColor "red") @?= "var(--color-red-600)"
    , testCase "resolveFillColor — palette entry" $
        resolveFillColor (NamedColor "red") @?= "var(--color-red-100)"
    , testCase "resolveStrokeColor — all palette entries" $ do
        resolveStrokeColor (NamedColor "blue") @?= "var(--color-blue-600)"
        resolveStrokeColor (NamedColor "green") @?= "var(--color-green-600)"
        resolveStrokeColor (NamedColor "orange") @?= "var(--color-orange-600)"
        resolveStrokeColor (NamedColor "purple") @?= "var(--color-purple-600)"
    , testCase "resolveFillColor — all palette entries" $ do
        resolveFillColor (NamedColor "blue") @?= "var(--color-blue-100)"
        resolveFillColor (NamedColor "green") @?= "var(--color-green-100)"
        resolveFillColor (NamedColor "orange") @?= "var(--color-orange-100)"
        resolveFillColor (NamedColor "purple") @?= "var(--color-purple-100)"
    , testCase "resolveStrokeColor — non-palette passthrough" $
        resolveStrokeColor (NamedColor "gray") @?= "gray"
    , testCase "resolveFillColor — non-palette passthrough" $
        resolveFillColor (NamedColor "lightgray") @?= "lightgray"
    , testCase "resolveStrokeColor — CurrentColor" $
        resolveStrokeColor CurrentColor @?= "currentColor"
    , testCase "resolveFillColor — CurrentColor" $
        resolveFillColor CurrentColor @?= "currentColor"
    , -- Parser: all 5 palette names accepted
      parsesTo
        "@color red"
        "@color red {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetColor (NamedColor "red"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@color blue"
        "@color blue {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetColor (NamedColor "blue"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@color green"
        "@color green {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetColor (NamedColor "green"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@color orange"
        "@color orange {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetColor (NamedColor "orange"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@color purple"
        "@color purple {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetColor (NamedColor "purple"))) [Draw (DrawPoint "A")]]
    , -- Parser: unknown name rejected
      testCase "unknown palette color rejected" $ do
        let result = parseGeometry "@color magenta {\n  drawPoint A\n}"
        assertBool "should fail" (isLeft result)
    , -- Parser: new keywords
      parsesTo
        "@lineColor red"
        "@lineColor red {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetLineColor (NamedColor "red"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@textColor blue"
        "@textColor blue {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetTextColor (NamedColor "blue"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@fillColor green"
        "@fillColor green {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetFillColor (NamedColor "green"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@figure orange"
        "@figure orange {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetFigure (NamedColor "orange"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "@palette purple"
        "@palette purple {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetPalette (NamedColor "purple"))) [Draw (DrawPoint "A")]]
    , -- Eval: SetColor sets both lineColor and textColor
      testCase "SetColor sets lineColor + textColor" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , ModifierBlock
                    (EnvMod (SetColor (NamedColor "red")))
                    [Draw (DrawPoint "A")]
                ]
        case main result of
          [RenderDot _ env] -> do
            lineColor env @?= NamedColor "red"
            textColor env @?= NamedColor "red"
          other -> assertFailure $ "Expected dot, got: " <> show other
    , -- Eval: SetPalette sets all three
      testCase "SetPalette sets lineColor + textColor + fillColor" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , ModifierBlock
                    (EnvMod (SetPalette (NamedColor "blue")))
                    [Draw (DrawPoint "A")]
                ]
        case main result of
          [RenderDot _ env] -> do
            lineColor env @?= NamedColor "blue"
            textColor env @?= NamedColor "blue"
            fillColor env @?= Just (NamedColor "blue")
          other -> assertFailure $ "Expected dot, got: " <> show other
    , -- Eval: SetFigure sets lineColor + fillColor
      testCase "SetFigure sets lineColor + fillColor" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , ModifierBlock
                    (EnvMod (SetFigure (NamedColor "green")))
                    [Draw (DrawPoint "A")]
                ]
        case main result of
          [RenderDot _ env] -> do
            lineColor env @?= NamedColor "green"
            textColor env @?= CurrentColor
            fillColor env @?= Just (NamedColor "green")
          other -> assertFailure $ "Expected dot, got: " <> show other
    , -- Eval: SetLineColor only changes lineColor
      testCase "SetLineColor only changes lineColor" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , ModifierBlock
                    (EnvMod (SetLineColor (NamedColor "red")))
                    [Draw (DrawPoint "A")]
                ]
        case main result of
          [RenderDot _ env] -> do
            lineColor env @?= NamedColor "red"
            textColor env @?= CurrentColor
          other -> assertFailure $ "Expected dot, got: " <> show other
    , -- Eval: SetTextColor only changes textColor
      testCase "SetTextColor only changes textColor" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , ModifierBlock
                    (EnvMod (SetTextColor (NamedColor "blue")))
                    [Draw (DrawPoint "A")]
                ]
        case main result of
          [RenderDot _ env] -> do
            lineColor env @?= CurrentColor
            textColor env @?= NamedColor "blue"
          other -> assertFailure $ "Expected dot, got: " <> show other
    ]

-- -----------------------------------------------------------------
-- Version tests
-- -----------------------------------------------------------------

versionGroup :: TestTree
versionGroup =
  testGroup
    "Version"
    [ testCase "currentGeometryVersion is (1, 0)" $
        currentGeometryVersion @?= (1, 0)
    , testCase "parseGeometryVersion V1.0" $
        parseGeometryVersion "V1.0" @?= Just (1, 0)
    , testCase "parseGeometryVersion V2.3" $
        parseGeometryVersion "V2.3" @?= Just (2, 3)
    , testCase "parseGeometryVersion invalid — no V prefix" $
        parseGeometryVersion "1.0" @?= Nothing
    , testCase "parseGeometryVersion invalid — no dot" $
        parseGeometryVersion "V10" @?= Nothing
    , testCase "parseGeometryVersion invalid — not a number" $
        parseGeometryVersion "Vx.y" @?= Nothing
    , testCase "isGeometryInfo \"geometry\"" $
        isGeometryInfo "geometry" @?= True
    , testCase "isGeometryInfo \"geometry V1.0\"" $
        isGeometryInfo "geometry V1.0" @?= True
    , testCase "isGeometryInfo \"python\"" $
        isGeometryInfo "python" @?= False
    , testCase "isGeometryInfo \"geometryX\"" $
        isGeometryInfo "geometryX" @?= False
    , testCase "geometryVersionText \"geometry\"" $
        geometryVersionText "geometry" @?= Nothing
    , testCase "geometryVersionText \"geometry V1.0\"" $
        geometryVersionText "geometry V1.0" @?= Just "V1.0"
    , testCase "geometryVersionText \"geometry  V2.1\"" $
        geometryVersionText "geometry  V2.1" @?= Just "V2.1"
    ]

-- -----------------------------------------------------------------
-- Helpers
-- -----------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

assertApprox :: String -> Double -> Double -> Assertion
assertApprox msg expected actual =
  assertBool
    (msg <> ": expected " <> show expected <> " but got " <> show actual)
    (abs (expected - actual) < 1e-10)
