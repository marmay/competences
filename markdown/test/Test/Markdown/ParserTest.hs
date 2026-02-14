module Test.Markdown.ParserTest (parserTests) where

import Competences.Markdown.AST
import Competences.Markdown.Parser (parseMarkdown)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

parserTests :: TestTree
parserTests =
  testGroup
    "Parser"
    [ testGroup "Empty input" emptyTests
    , testGroup "Paragraphs" paragraphTests
    , testGroup "Headings" headingTests
    , testGroup "Emphasis and strong" emphTests
    , testGroup "Inline math" mathInlineTests
    , testGroup "Math blocks" mathBlockTests
    , testGroup "Code spans" codeSpanTests
    , testGroup "Fenced code blocks" fencedCodeTests
    , testGroup "Links" linkTests
    , testGroup "Ordered lists" orderedListTests
    , testGroup "Lettered lists" letteredListTests
    , testGroup "Thematic breaks" thematicBreakTests
    , testGroup "Line breaks" lineBreakTests
    , testGroup "Admonitions" admonitionTests
    , testGroup "Backward compatibility" backwardCompatTests
    ]

-- | Helper: assert successful parse matches expected AST
assertParse :: String -> Document -> String -> Assertion
assertParse label expected input =
  case parseMarkdown (T.pack input) of
    Left err -> assertFailure $ label <> ": parse failed: " <> show err
    Right actual -> assertEqual label expected actual

-- | Helper: assert parse produces a specific number of blocks
assertBlockCount :: String -> Int -> String -> Assertion
assertBlockCount label n input =
  case parseMarkdown (T.pack input) of
    Left err -> assertFailure $ label <> ": parse failed: " <> show err
    Right (Document blocks) -> assertEqual (label <> " block count") n (length blocks)

emptyTests :: [TestTree]
emptyTests =
  [ testCase "empty string" $
      assertParse "empty" (Document []) ""
  , testCase "whitespace only" $
      assertParse "whitespace" (Document []) "   \n  \n  "
  ]

paragraphTests :: [TestTree]
paragraphTests =
  [ testCase "single paragraph" $
      assertParse "single" (Document [Paragraph [Plain "Hello world"]]) "Hello world"
  , testCase "two paragraphs separated by blank line" $
      assertBlockCount "two paras" 2 "First paragraph\n\nSecond paragraph"
  ]

headingTests :: [TestTree]
headingTests =
  [ testCase "h1" $
      assertParse "h1" (Document [Heading 1 [Plain "Title"]]) "# Title"
  , testCase "h2" $
      assertParse "h2" (Document [Heading 2 [Plain "Subtitle"]]) "## Subtitle"
  , testCase "h3" $
      assertParse "h3" (Document [Heading 3 [Plain "Section"]]) "### Section"
  , testCase "heading with emphasis" $
      assertParse
        "heading+emph"
        (Document [Heading 1 [Plain "Hello ", Emph [Plain "world"]]])
        "# Hello *world*"
  ]

emphTests :: [TestTree]
emphTests =
  [ testCase "emphasis" $
      assertParse
        "emph"
        (Document [Paragraph [Plain "Hello ", Emph [Plain "world"]]])
        "Hello *world*"
  , testCase "strong" $
      assertParse
        "strong"
        (Document [Paragraph [Plain "Hello ", Strong [Plain "world"]]])
        "Hello **world**"
  , testCase "nested emphasis in strong" $
      assertParse
        "nested"
        (Document [Paragraph [Strong [Plain "bold ", Emph [Plain "and italic"]]]])
        "**bold *and italic***"
  ]

mathInlineTests :: [TestTree]
mathInlineTests =
  [ testCase "inline math" $
      assertParse
        "inline math"
        (Document [Paragraph [Plain "Solve ", MathInline "x + 1 = 2"]])
        "Solve $x + 1 = 2$"
  , testCase "multiple inline math" $
      assertParse
        "multi math"
        (Document [Paragraph [Plain "Use ", MathInline "a", Plain " and ", MathInline "b"]])
        "Use $a$ and $b$"
  ]

mathBlockTests :: [TestTree]
mathBlockTests =
  [ testCase "dollar math block" $
      assertParse
        "dollar block"
        (Document [MathBlock "x^2 + y^2 = z^2"])
        "$$x^2 + y^2 = z^2$$"
  , testCase "bracket math block" $
      assertParse
        "bracket block"
        (Document [MathBlock "E = mc^2"])
        "\\[E = mc^2\\]"
  ]

codeSpanTests :: [TestTree]
codeSpanTests =
  [ testCase "inline code" $
      assertParse
        "code"
        (Document [Paragraph [Plain "Use ", Code "map", Plain " function"]])
        "Use `map` function"
  ]

fencedCodeTests :: [TestTree]
fencedCodeTests =
  [ testCase "basic code block" $
      assertParse
        "basic fenced"
        (Document [FencedCodeBlock Nothing "hello"])
        "```\nhello\n```"
  , testCase "code block with info string" $
      assertParse
        "info string"
        (Document [FencedCodeBlock (Just "haskell") "main = putStrLn \"hi\""])
        "```haskell\nmain = putStrLn \"hi\"\n```"
  ]

linkTests :: [TestTree]
linkTests =
  [ testCase "basic link" $
      assertParse
        "link"
        (Document [Paragraph [Link "https://example.com" [Plain "Example"] Nothing]])
        "[Example](https://example.com)"
  , testCase "link with title" $
      assertParse
        "link+title"
        (Document [Paragraph [Link "https://example.com" [Plain "Example"] (Just "A site")]])
        "[Example](https://example.com \"A site\")"
  ]

orderedListTests :: [TestTree]
orderedListTests =
  [ testCase "simple ordered list" $
      assertParse
        "ordered"
        (Document [OrderedList 1 [[Paragraph [Plain "First"]], [Paragraph [Plain "Second"]]]])
        "1. First\n2. Second"
  ]

letteredListTests :: [TestTree]
letteredListTests =
  [ testCase "simple lettered list" $
      assertParse
        "lettered"
        (Document [LetterList [[Paragraph [Plain "Alpha"]], [Paragraph [Plain "Beta"]]]])
        "a. Alpha\nb. Beta"
  , testCase "lettered list with emphasis" $
      assertParse
        "lettered+emph"
        (Document [LetterList [[Paragraph [Plain "Do ", Emph [Plain "this"]]], [Paragraph [Plain "And that"]]]])
        "a. Do *this*\nb. And that"
  ]

thematicBreakTests :: [TestTree]
thematicBreakTests =
  [ testCase "dashes" $
      assertParse "dashes" (Document [ThematicBreak]) "---"
  , testCase "asterisks" $
      assertParse "asterisks" (Document [ThematicBreak]) "***"
  ]

lineBreakTests :: [TestTree]
lineBreakTests =
  [ testCase "hard break with backslash" $
      assertParse
        "hard break"
        (Document [Paragraph [Plain "Line one", HardLineBreak, Plain "Line two"]])
        "Line one\\\nLine two"
  ]

admonitionTests :: [TestTree]
admonitionTests =
  [ testCase "definition with title" $
      assertParse
        "def"
        (Document [Admonition Definition (Just [Plain "Primzahl"]) [Paragraph [Plain "Eine Primzahl ist..."]]])
        "> [!definition] Primzahl\n> Eine Primzahl ist..."
  , testCase "theorem without title" $
      assertParse
        "thm"
        (Document [Admonition Theorem Nothing [Paragraph [Plain "Es gilt..."]]])
        "> [!theorem]\n> Es gilt..."
  , testCase "proof with math" $
      assertParse
        "proof"
        (Document [Admonition Proof Nothing [Paragraph [Plain "Sei ", MathInline "p", Plain " eine Primzahl."]]])
        "> [!proof]\n> Sei $p$ eine Primzahl."
  , testCase "german type alias" $
      assertParse
        "satz"
        (Document [Admonition Theorem (Just [Plain "Euklid"]) [Paragraph [Plain "Body"]]])
        "> [!satz] Euklid\n> Body"
  , testCase "multi-paragraph body" $
      assertBlockCount "multi-para admonition" 1
        "> [!remark]\n> First paragraph.\n>\n> Second paragraph."
  , testCase "case insensitive type" $
      assertParse
        "case"
        (Document [Admonition Definition Nothing [Paragraph [Plain "Body"]]])
        "> [!Definition]\n> Body"
  , testCase "admonition after paragraph" $
      assertBlockCount "para then admonition" 2
        "Some text.\n\n> [!theorem]\n> Body"
  ]

-- | Tests for backward compatibility with existing TaskContent markup
backwardCompatTests :: [TestTree]
backwardCompatTests =
  [ testCase "task with math and subtasks" $ do
      let input = "Solve the equation:\n\n$$x^2 + 2x + 1 = 0$$\n\na. Factorize\nb. Explain *why* this **matters**\nc. Check your answer using $x = -1$"
      case parseMarkdown (T.pack input) of
        Left err -> assertFailure $ "parse failed: " <> show err
        Right (Document blocks) -> do
          assertEqual "block count" 3 (length blocks)
          case blocks of
            [Paragraph _, MathBlock _, LetterList items] -> do
              assertEqual "subtask count" 3 (length items)
            other -> assertFailure $ "unexpected structure: " <> show other
  , testCase "heading followed by paragraph" $ do
      let input = "# Main Title\n\nSome description text."
      case parseMarkdown (T.pack input) of
        Left err -> assertFailure $ "parse failed: " <> show err
        Right (Document blocks) ->
          assertEqual "block count" 2 (length blocks)
  ]
