module Test.Markdown.ParserTest (parserTests) where

import Competences.Markdown.AST (AdmonitionType (..), Block (..), Document (..), Inline (..), ThumbSize (..))
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
    , testGroup "File embeds" fileEmbedTests
    , testGroup "Links" linkTests
    , testGroup "Ordered lists" orderedListTests
    , testGroup "Bullet lists" bulletListTests
    , testGroup "Lettered lists" letteredListTests
    , testGroup "Thematic breaks" thematicBreakTests
    , testGroup "Line breaks" lineBreakTests
    , testGroup "Admonitions" admonitionTests
    , testGroup "Notes grid" notesGridTests
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
  , testCase "heading does not absorb next line" $
      assertParse
        "heading+para"
        (Document [Heading 3 [Plain "Angabe"], Paragraph [Plain "Something"]])
        "### Angabe\nSomething"
  , testCase "heading followed by paragraph without blank line" $
      assertParse
        "heading+para no blank"
        (Document [Heading 4 [Plain "Angabe"], Paragraph [Plain "Solve the equation."]])
        "#### Angabe\nSolve the equation."
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

fileEmbedTests :: [TestTree]
fileEmbedTests =
  [ testCase "basic file embed" $
      assertParse
        "file embed"
        (Document [Paragraph [FileEmbed "file:photo.jpg" [Plain "alt"] Nothing Nothing]])
        "![alt](file:photo.jpg)"
  , testCase "file embed by index" $
      assertParse
        "fileIdx"
        (Document [Paragraph [FileEmbed "fileIdx:0" [] Nothing Nothing]])
        "![](fileIdx:0)"
  , testCase "file embed with title" $
      assertParse
        "file+title"
        (Document [Paragraph [FileEmbed "file:x.png" [Plain "cap"] (Just "title") Nothing]])
        "![cap](file:x.png \"title\")"
  , testCase "file embed with nested inline" $
      assertParse
        "file+bold"
        (Document [Paragraph [FileEmbed "file:y.jpg" [Strong [Plain "bold"]] Nothing Nothing]])
        "![**bold**](file:y.jpg)"
  , testCase "exclamation mark without bracket is plain text" $
      assertParse
        "plain bang"
        (Document [Paragraph [Plain "Hello", Plain "!", Plain " World"]])
        "Hello! World"
  , testCase "file embed with thumb=small" $
      assertParse
        "thumb small"
        (Document [Paragraph [FileEmbed "file:photo.jpg" [Plain "alt"] Nothing (Just ThumbSmall)]])
        "![alt](file:photo.jpg){thumb=small}"
  , testCase "file embed with thumb=medium" $
      assertParse
        "thumb medium"
        (Document [Paragraph [FileEmbed "file:photo.jpg" [Plain "alt"] Nothing (Just ThumbMedium)]])
        "![alt](file:photo.jpg){thumb=medium}"
  , testCase "file embed with title and thumb=large" $
      assertParse
        "thumb+title"
        (Document [Paragraph [FileEmbed "file:photo.jpg" [Plain "cap"] (Just "title") (Just ThumbLarge)]])
        "![cap](file:photo.jpg \"title\"){thumb=large}"
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

bulletListTests :: [TestTree]
bulletListTests =
  [ testCase "simple bullet list with dash" $
      assertParse
        "dash bullets"
        (Document [BulletList [[Paragraph [Plain "Alpha"]], [Paragraph [Plain "Beta"]]]])
        "- Alpha\n- Beta"
  , testCase "bullet list with asterisk" $
      assertParse
        "asterisk bullets"
        (Document [BulletList [[Paragraph [Plain "One"]], [Paragraph [Plain "Two"]]]])
        "* One\n* Two"
  , testCase "bullet list with plus" $
      assertParse
        "plus bullets"
        (Document [BulletList [[Paragraph [Plain "X"]], [Paragraph [Plain "Y"]]]])
        "+ X\n+ Y"
  , testCase "bullet list with inline formatting" $
      assertParse
        "bullets+emph"
        (Document [BulletList [[Paragraph [Emph [Plain "bold"]]], [Paragraph [Plain "plain"]]]])
        "- *bold*\n- plain"
  , testCase "bullet list not confused with thematic break" $
      assertParse
        "dash vs thematic"
        (Document [ThematicBreak])
        "---"
  , testCase "mixed markers" $
      assertParse
        "mixed"
        (Document [BulletList [[Paragraph [Plain "A"]], [Paragraph [Plain "B"]], [Paragraph [Plain "C"]]]])
        "- A\n* B\n+ C"
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

notesGridTests :: [TestTree]
notesGridTests =
  [ testCase "basic 4-cell grid" $
      assertParse
        "4-cell"
        ( Document
            [ NotesGrid
                [Paragraph [Plain "Cell 1"]]
                [Paragraph [Plain "Cell 2"]]
                [Paragraph [Plain "Cell 3"]]
                [Paragraph [Plain "Cell 4"]]
            ]
        )
        "```btc:notes-grid\nCell 1\n---\nCell 2\n---\nCell 3\n---\nCell 4\n```"
  , testCase "grid with inline formatting and math" $
      assertParse
        "formatting+math"
        ( Document
            [ NotesGrid
                [Paragraph [Strong [Plain "Bold"], Plain " and ", MathInline "x^2"]]
                [Paragraph [Emph [Plain "italic"]]]
                [Paragraph [Plain "plain"]]
                [Paragraph [Code "code"]]
            ]
        )
        "```btc:notes-grid\n**Bold** and $x^2$\n---\n*italic*\n---\nplain\n---\n`code`\n```"
  , testCase "grid with fewer than 4 cells pads with empty" $
      assertParse
        "2-cell pad"
        ( Document
            [ NotesGrid
                [Paragraph [Plain "A"]]
                [Paragraph [Plain "B"]]
                []
                []
            ]
        )
        "```btc:notes-grid\nA\n---\nB\n```"
  , testCase "grid with block-level content in cells" $
      assertParse
        "block content"
        ( Document
            [ NotesGrid
                [BulletList [[Paragraph [Plain "item 1"]], [Paragraph [Plain "item 2"]]]]
                [Paragraph [Plain "text"]]
                [Heading 2 [Plain "Title"]]
                []
            ]
        )
        "```btc:notes-grid\n- item 1\n- item 2\n---\ntext\n---\n## Title\n```"
  , testCase "grid followed by paragraph" $
      assertBlockCount "grid+para" 2
        "```btc:notes-grid\nA\n---\nB\n---\nC\n---\nD\n```\n\nFollowing paragraph"
  , testCase "grid with nested fenced code block" $
      assertParse
        "nested fence"
        ( Document
            [ NotesGrid
                [FencedCodeBlock (Just "geometry") "defPoint A 0 0"]
                [Paragraph [Plain "Cell 2"]]
                []
                []
            ]
        )
        "```btc:notes-grid\n```geometry\ndefPoint A 0 0\n```\n---\nCell 2\n```"
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
