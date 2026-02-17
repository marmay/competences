module Test.Markdown.ValidationTest (validationTests) where

import Competences.Markdown.Validation (ValidationError (..), validateMarkdown)
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
    ]
