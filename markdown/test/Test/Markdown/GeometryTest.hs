module Test.Markdown.GeometryTest (geometryTests) where

import Competences.Markdown.Geometry.AST
import Competences.Markdown.Geometry.Eval (evalScene)
import Competences.Markdown.Geometry.Parser (parseGeometry)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

geometryTests :: TestTree
geometryTests =
  testGroup
    "Geometry DSL"
    [ parserGroup
    , evalGroup
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
        , Label (LabelAtPoint "A" "A" BelowLeft)
        ]
    , parsesTo "drawSegment by name" "drawSegment c" [Draw (DrawSegment (SegByName "c"))]
    , parsesTo "drawSegment inline" "drawSegment A -- B" [Draw (DrawSegment (SegInline "A" "B"))]
    , parsesTo
        "drawSegment labeled"
        "drawSegment A -- B labeled \"c\" below 0.4"
        [ Draw (DrawSegment (SegInline "A" "B"))
        , Label (LabelOnSegment (SegInline "A" "B") "c" SegBelow 0.4)
        ]
    , parsesTo
        "drawSegment labeled default fraction"
        "drawSegment c labeled \"c\" above"
        [ Draw (DrawSegment (SegByName "c"))
        , Label (LabelOnSegment (SegByName "c") "c" SegAbove 0.5)
        ]
    , parsesTo "labelPoint" "labelPoint A \"A\" above-right" [Label (LabelAtPoint "A" "A" AboveRight)]
    , parsesTo
        "labelSegment by name"
        "labelSegment c \"c\" below"
        [Label (LabelOnSegment (SegByName "c") "c" SegBelow 0.5)]
    , parsesTo
        "labelSegment inline with fraction"
        "labelSegment A -- B \"ab\" above 0.3"
        [Label (LabelOnSegment (SegInline "A" "B") "ab" SegAbove 0.3)]
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
    , testCase "parse error" $ do
        let result = parseGeometry "unknownCommand A B"
        assertBool "should fail" (isLeft result)
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
                , Label (LabelAtPoint "A" "A" Above)
                ]
        case main result of
          [RenderLabel _ txt pos _] -> do
            txt @?= "A"
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
