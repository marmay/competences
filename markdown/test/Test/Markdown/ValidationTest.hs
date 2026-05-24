module Test.Markdown.ValidationTest (validationTests) where

import Competences.Markdown.Validation (ValidationError (..), validateMarkdown)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

validationTests :: TestTree
validationTests =
  testGroup
    "Validation"
    [ testCase "valid markdown — no errors" $ do
        validateMarkdown "# Hello\n\nA paragraph." @?= []
    , testCase "invalid markdown — reports error" $ do
        -- An unterminated math block is a parse error
        let errors = validateMarkdown "$$\nx\n"
        assertBool "should have errors" (not $ null errors)
        assertBool "context should be Markdown" $
          any (\e -> context e == "Markdown") errors
    , testCase "valid geometry block — no errors" $ do
        validateMarkdown "```geometry\ndefPoint A (0, 0)\n```" @?= []
    , testCase "valid geometry block with version — no errors" $ do
        validateMarkdown "```geometry V1.0\ndefPoint A (0, 0)\n```" @?= []
    , testCase "invalid geometry block — reports error" $ do
        let errors = validateMarkdown "```geometry\nunknownCommand\n```"
        assertBool "should have errors" (not $ null errors)
        assertBool "context should mention Geometrie-Block 1" $
          any (\e -> context e == "Geometrie-Block 1") errors
    , testCase "unsupported geometry version — reports error" $ do
        let errors = validateMarkdown "```geometry V99.0\ndefPoint A (0, 0)\n```"
        assertBool "should have errors" (not $ null errors)
        assertBool "context should mention Geometrie-Block 1" $
          any (\e -> context e == "Geometrie-Block 1") errors
    , testCase "multiple geometry blocks — independent numbering" $ do
        let input =
              "```geometry\ndefPoint A (0, 0)\n```\n\n"
                <> "```geometry\nunknownCommand\n```"
        let errors = validateMarkdown input
        assertBool "should have exactly one error" (length errors == 1)
        assertBool "error should be in block 2" $
          any (\e -> context e == "Geometrie-Block 2") errors
    , testCase "non-geometry code blocks are ignored" $ do
        validateMarkdown "```python\nimport os\n```" @?= []
    , testCase "empty geometry block — no errors" $ do
        validateMarkdown "```geometry\n\n```" @?= []
    , testCase "well-formed table — no errors" $ do
        validateMarkdown "| x | y |\n|---|---|\n| 1 | 2 |" @?= []
    , testCase "table with wrong row width — reports row index" $ do
        let errors = validateMarkdown "| x | y |\n|---|---|\n| 1 | 2 | 3 |"
        assertBool "should have errors" (not $ null errors)
        assertBool "context should mention row 1" $
          any (\e -> context e == "Tabelle, Zeile 1") errors
    , testCase "valid columns block — no errors" $ do
        validateMarkdown "```columns 1:1\nLeft\n+++\nRight\n```" @?= []
    , testCase "geometry inside columns is validated" $ do
        let input =
              "```columns 1:1\n"
                <> "```geometry\nunknownCommand\n```\n"
                <> "+++\nRight\n```"
        let errors = validateMarkdown input
        assertBool "should have errors" (not $ null errors)
        assertBool "context should mention Geometrie-Block 1" $
          any (\e -> T.isPrefixOf "Geometrie-Block" (context e)) errors
    ]
