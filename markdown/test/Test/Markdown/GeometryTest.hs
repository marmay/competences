module Test.Markdown.GeometryTest (geometryTests) where

import Competences.Markdown.Geometry.AST
import Competences.Markdown.Geometry.Eval (evalScene, extractMathLabels)
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
        "dashed {\n  drawSegment A -- B\n}"
        [ModifierBlock (EnvMod SetDashed) [Draw (DrawSegment (SegInline "A" "B"))]]
    , parsesTo
        "modifier block color"
        "color red {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetColor (NamedColor "red"))) [Draw (DrawPoint "A")]]
    , parsesTo
        "modifier block axes"
        "axes {\n  drawPoint A\n}"
        [ModifierBlock (AutoDec Axes) [Draw (DrawPoint "A")]]
    , parsesTo
        "modifier block labelAll"
        "labelAll above {\n  drawPoint A\n  drawPoint B\n}"
        [ ModifierBlock
            (AutoDec (LabelAll Above))
            [Draw (DrawPoint "A"), Draw (DrawPoint "B")]
        ]
    , parsesTo
        "nested modifiers"
        "axes {\n  dashed {\n    drawSegment A -- B\n  }\n}"
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
        "background {\n  drawSegment A -- B\n}"
        [ModifierBlock (LayerMod Background) [Draw (DrawSegment (SegInline "A" "B"))]]
    , parsesTo
        "comma-separated modifiers"
        "axes, grid {\n  drawPoint A\n}"
        [ ModifierBlock
            (AutoDec Axes)
            [ ModifierBlock
                (AutoDec Grid)
                [Draw (DrawPoint "A")]
            ]
        ]
    , parsesTo
        "comma-separated three modifiers"
        "axes, grid, dashed {\n  drawSegment A -- B\n}"
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
        "color red, thick {\n  drawPoint A\n}"
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
        "labelDist 0.5 {\n  drawPoint A\n}"
        [ModifierBlock (EnvMod (SetLabelDist 0.5)) [Draw (DrawPoint "A")]]
    , parsesTo
        "labelDist comma-separated"
        "labelDist 0.6, color red {\n  drawPoint A\n}"
        [ ModifierBlock
            (EnvMod (SetLabelDist 0.6))
            [ ModifierBlock
                (EnvMod (SetColor (NamedColor "red")))
                [Draw (DrawPoint "A")]
            ]
        ]
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
          [RenderLabel _ lbl pos _] -> do
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
          [RenderDot _ env] ->
            color env @?= NamedColor "red"
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
        -- bisector at B = normalize((1,0)+(0,1)) = (1,1)/sqrt(2) → into the angle
        -- labelDist = 0.75 (default)
        -- label at (0.75/sqrt(2), 0.75/sqrt(2))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , Label (LabelAngle (AngleRef "A" "B" "C") (PlainLabel "a") Nothing)
                ]
        case main result of
          [RenderLabel (Vec2 lx ly) lbl pos _env] -> do
            lbl @?= PlainLabel "a"
            let dist = 0.75 :: Double
                invSqrt2 = 1 / sqrt 2
            assertApprox "lx" (invSqrt2 * dist) lx
            assertApprox "ly" (invSqrt2 * dist) ly
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
          [RenderSegment _ (Vec2 ex ey) _, RenderLabel (Vec2 lx ly) lbl _ _] -> do
            lbl @?= MathLabel "\\alpha"
            assertApprox "external x" 1.5 ex
            assertApprox "external y" 1.5 ey
            assertApprox "label x" 1.5 lx
            assertApprox "label y" 1.5 ly
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
          [RenderLabel _ lbl _ _] ->
            lbl @?= PlainLabel "a"
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
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
        -- Label offset: 0.75 along outward → (-0.75/sqrt(2), -0.75/sqrt(2))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 3)
                , Label (LabelAutoPoint (AngleRef "C" "A" "B") (PlainLabel "A"))
                ]
        case main result of
          [RenderLabel (Vec2 lx ly) lbl pos _] -> do
            lbl @?= PlainLabel "A"
            let invSqrt2 = 1 / sqrt 2
            assertApprox "lx" ((-invSqrt2) * 0.75) lx
            assertApprox "ly" ((-invSqrt2) * 0.75) ly
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "LabelAutoPoint at top vertex — centered" $ do
        -- Equilateral-ish: A=(0,0), B=(4,0), C=(2,3)
        -- At vertex C, prev=B=(4,0), succ=A=(0,0)
        -- Edge B→C: dir=(-2,3), outward normal = (3,2)/sqrt(13)
        -- Edge C→A: dir=(-2,-3), outward normal = (-3,2)/sqrt(13)
        -- Average: (0,4)/sqrt(13) → Above
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 2 3)
                , Label (LabelAutoPoint (AngleRef "B" "C" "A") (PlainLabel "C"))
                ]
        case main result of
          [RenderLabel (Vec2 _lx ly) _ pos _] -> do
            pos @?= Center
            -- Label should be offset upward from vertex
            assertBool "ly > 3" (ly > 3)
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "DrawFilledPolygon with fill active — renders to background" $ do
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 3)
                , ModifierBlock
                    (EnvMod (SetFill (NamedColor "red")))
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
                    (EnvMod (SetFill (NamedColor "blue")))
                    [Draw (DrawFilledPolygon ["A", "B", "C"])]
                , Draw (DrawFilledPolygon ["A", "B", "C"])
                ]
        -- First fill goes to background, second is a no-op
        length (background result) @?= 1
    , testCase "fill block parser" $ do
        case parseGeometry "fill red {\n  drawPoly A -- B -- C\n}" of
          Left err -> assertFailure $ "Parse failed: " <> show err
          Right cmds -> do
            -- Should be wrapped in ModifierBlock with SetFill
            case cmds of
              [ModifierBlock (EnvMod (SetFill (NamedColor "red"))) _children] -> pure ()
              other -> assertFailure $ "Expected fill modifier block, got: " <> show other
    , testCase "LabelAutoPoint isosceles right — vertex A=(0,0) 90°" $ do
        -- Triangle A=(0,0), B=(4,0), C=(0,4), right angle at A
        -- AngleRef "C" "A" "B": vertex A, arms toward C and B
        -- Bisector = (1/√2, 1/√2) → outward = (-1/√2, -1/√2)
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , Label (LabelAutoPoint (AngleRef "C" "A" "B") (PlainLabel "A"))
                ]
        case main result of
          [RenderLabel (Vec2 lx ly) lbl pos _] -> do
            lbl @?= PlainLabel "A"
            let invSqrt2 = 1 / sqrt 2
            assertApprox "lx" ((-invSqrt2) * 0.75) lx
            assertApprox "ly" ((-invSqrt2) * 0.75) ly
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "LabelAutoPoint isosceles right — vertex B=(4,0) 45°" $ do
        -- AngleRef "A" "B" "C": vertex B, arms toward A and C
        -- Bisector = (-cos(π/8), sin(π/8)) → outward = (cos(π/8), -sin(π/8))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , Label (LabelAutoPoint (AngleRef "A" "B" "C") (PlainLabel "B"))
                ]
        case main result of
          [RenderLabel (Vec2 lx ly) lbl pos _] -> do
            lbl @?= PlainLabel "B"
            pos @?= Center
            let cospi8 = cos (pi / 8)
                sinpi8 = sin (pi / 8)
            assertApprox "lx" (4 + cospi8 * 0.75) lx
            assertApprox "ly" ((-sinpi8) * 0.75) ly
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "LabelAutoPoint isosceles right — vertex C=(0,4) 45°" $ do
        -- AngleRef "B" "C" "A": vertex C, arms toward B and A
        -- Bisector = (sin(π/8), -cos(π/8)) → outward = (-sin(π/8), cos(π/8))
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , Label (LabelAutoPoint (AngleRef "B" "C" "A") (PlainLabel "C"))
                ]
        case main result of
          [RenderLabel (Vec2 lx ly) lbl pos _] -> do
            lbl @?= PlainLabel "C"
            pos @?= Center
            let cospi8 = cos (pi / 8)
                sinpi8 = sin (pi / 8)
            assertApprox "lx" ((-sinpi8) * 0.75) lx
            assertApprox "ly" (4 + cospi8 * 0.75) ly
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "labelDist affects LabelAutoPoint" $ do
        -- Same triangle as isosceles right test but with custom labelDist 0.6
        let result =
              evalScene
                [ DefPoint "A" (Vec2 0 0)
                , DefPoint "B" (Vec2 4 0)
                , DefPoint "C" (Vec2 0 4)
                , ModifierBlock
                    (EnvMod (SetLabelDist 0.6))
                    [Label (LabelAutoPoint (AngleRef "C" "A" "B") (PlainLabel "A"))]
                ]
        case main result of
          [RenderLabel (Vec2 lx ly) lbl pos _] -> do
            lbl @?= PlainLabel "A"
            let invSqrt2 = 1 / sqrt 2
            assertApprox "lx" ((-invSqrt2) * 0.6) lx
            assertApprox "ly" ((-invSqrt2) * 0.6) ly
            pos @?= Center
          other -> assertFailure $ "Expected [RenderLabel], got: " <> show other
    , testCase "labelDist affects LabelAngle" $ do
        -- A=(1,0), B=(0,0), C=(0,1) with custom labelDist 0.8
        let result =
              evalScene
                [ DefPoint "A" (Vec2 1 0)
                , DefPoint "B" (Vec2 0 0)
                , DefPoint "C" (Vec2 0 1)
                , ModifierBlock
                    (EnvMod (SetLabelDist 0.8))
                    [Label (LabelAngle (AngleRef "A" "B" "C") (PlainLabel "a") Nothing)]
                ]
        case main result of
          [RenderLabel (Vec2 lx ly) lbl pos _] -> do
            lbl @?= PlainLabel "a"
            let invSqrt2 = 1 / sqrt 2
            assertApprox "lx" (invSqrt2 * 0.8) lx
            assertApprox "ly" (invSqrt2 * 0.8) ly
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
