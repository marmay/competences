module Test.SearchTest (tests) where

import Competences.Search
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Search"
    [ parsingTests
    , matchingTests
    , typedFilterTests
    ]

-- ============================================================================
-- Parsing tests
-- ============================================================================

parsingTests :: TestTree
parsingTests =
  testGroup
    "parseQuery"
    [ testCase "empty string → no clauses" $
        parseQuery "" @?= []
    , testCase "single text term" $
        parseQuery "Iso" @?= [[TextTerm "Iso"]]
    , testCase "comma-separated OR" $
        parseQuery "Iso, Trist" @?= [[TextTerm "Iso"], [TextTerm "Trist"]]
    , testCase "pipe-separated OR" $
        parseQuery "Iso | Trist" @?= [[TextTerm "Iso"], [TextTerm "Trist"]]
    , testCase "single meta term" $
        parseQuery "@06.03" @?= [[MetaTerm "06.03"]]
    , testCase "multiple meta terms AND together" $
        parseQuery "@06.03 @hü" @?= [[MetaTerm "06.03", MetaTerm "hü"]]
    , testCase "text AND meta with &" $
        parseQuery "Iso & @m" @?= [[TextTerm "Iso", MetaTerm "m"]]
    , testCase "mixed AND/OR" $
        parseQuery "Iso & @m, Trist"
          @?= [[TextTerm "Iso", MetaTerm "m"], [TextTerm "Trist"]]
    , testCase "whitespace trimmed" $
        parseQuery "  Iso  " @?= [[TextTerm "Iso"]]
    , testCase "empty clauses ignored" $
        parseQuery ",," @?= []
    , testCase "bare @ ignored" $
        parseQuery "@" @?= []
    , testCase "@ mid-word splits into text + meta" $
        parseQuery "foo@bar" @?= [[TextTerm "foo", MetaTerm "bar"]]
    ]

-- ============================================================================
-- Matching tests
-- ============================================================================

matchingTests :: TestTree
matchingTests =
  testGroup
    "matchItem"
    [ testCase "text substring match" $
        matchItem id (const []) [[TextTerm "Iso"]] "Isolde" @?= True
    , testCase "text no match" $
        matchItem id (const []) [[TextTerm "Tri"]] "Isolde" @?= False
    , testCase "case-insensitive text" $
        matchItem id (const []) [[TextTerm "iso"]] "Isolde" @?= True
    , testCase "meta match" $
        matchItem (const "HÜ 1") (const ["06.03.2026"]) [[MetaTerm "06.03"]] ()
          @?= True
    , testCase "meta AND (both match)" $
        matchItem
          (const "HÜ 1")
          (const ["06.03.2026", "HÜ"])
          [[MetaTerm "06.03", MetaTerm "hü"]]
          ()
          @?= True
    , testCase "meta no match" $
        matchItem (const "HÜ 1") (const ["06.03.2026"]) [[MetaTerm "07.03"]] ()
          @?= False
    , testCase "OR: first clause matches" $
        matchItem id (const []) [[TextTerm "Iso"], [TextTerm "Tri"]] "Isolde"
          @?= True
    , testCase "OR: second clause matches" $
        matchItem id (const []) [[TextTerm "Iso"], [TextTerm "Tri"]] "Tristan"
          @?= True
    , testCase "OR: neither matches" $
        matchItem id (const []) [[TextTerm "Iso"], [TextTerm "Tri"]] "Hugo"
          @?= False
    , testCase "empty query matches everything" $
        matchItem id (const []) [] "anything" @?= True
    , testCase "text + meta AND in same clause" $
        matchItem id (const ["math"]) [[TextTerm "Iso", MetaTerm "ma"]] "Isolde"
          @?= True
    , testCase "text + meta AND fails when text doesn't match" $
        matchItem id (const ["math"]) [[TextTerm "Tri", MetaTerm "ma"]] "Isolde"
          @?= False
    ]

-- ============================================================================
-- Typed filter tests
-- ============================================================================

-- Test data type
data Item = Item
  { iLabel :: !Text
  , iColor :: !Text
  , iSize :: !Text
  }

-- Sample filters for testing
colorFilter :: Text -> Maybe (Item -> Bool)
colorFilter t
  | T.toLower t == "rot" = Just (\i -> i.iColor == "red")
  | T.toLower t == "blau" = Just (\i -> i.iColor == "blue")
  | otherwise = Nothing

sizeFilter :: Text -> Maybe (Item -> Bool)
sizeFilter t
  | T.toLower t == "groß" = Just (\i -> i.iSize == "large")
  | T.toLower t == "klein" = Just (\i -> i.iSize == "small")
  | otherwise = Nothing

testFilters :: [Text -> Maybe (Item -> Bool)]
testFilters = [colorFilter, sizeFilter]

redLarge :: Item
redLarge = Item "Apfel" "red" "large"

blueSmall :: Item
blueSmall = Item "Beere" "blue" "small"

redSmall :: Item
redSmall = Item "Kirsche" "red" "small"

typedFilterTests :: TestTree
typedFilterTests =
  testGroup
    "matchItemWithFilters"
    [ testCase "@rot resolves and matches red item" $
        matchItemWithFilters (.iLabel) testFilters [[MetaTerm "rot"]] redLarge @?= True
    , testCase "@rot does not match blue item" $
        matchItemWithFilters (.iLabel) testFilters [[MetaTerm "rot"]] blueSmall @?= False
    , testCase "@r does not resolve → clause dropped → matches everything" $
        matchItemWithFilters (.iLabel) testFilters [[MetaTerm "r"]] blueSmall @?= True
    , testCase "@rot @groß → both resolve, AND them → red+large matches" $
        matchItemWithFilters (.iLabel) testFilters [[MetaTerm "rot", MetaTerm "groß"]] redLarge @?= True
    , testCase "@rot @groß → AND → red+small does not match" $
        matchItemWithFilters (.iLabel) testFilters [[MetaTerm "rot", MetaTerm "groß"]] redSmall @?= False
    , testCase "@rot @x → @x unresolved → entire clause invalid → matches everything" $
        matchItemWithFilters (.iLabel) testFilters [[MetaTerm "rot", MetaTerm "x"]] blueSmall @?= True
    , testCase "text term still works" $
        matchItemWithFilters (.iLabel) testFilters [[TextTerm "Apf"]] redLarge @?= True
    , testCase "text + meta AND" $
        matchItemWithFilters (.iLabel) testFilters [[TextTerm "Apf", MetaTerm "rot"]] redLarge @?= True
    , testCase "text + meta AND fails when text doesn't match" $
        matchItemWithFilters (.iLabel) testFilters [[TextTerm "Bee", MetaTerm "rot"]] redLarge @?= False
    , testCase "OR: first clause valid, second invalid → first used" $
        matchItemWithFilters (.iLabel) testFilters [[MetaTerm "blau"], [MetaTerm "x"]] redLarge @?= False
    , testCase "empty query matches everything" $
        matchItemWithFilters (.iLabel) testFilters [] redLarge @?= True
    , testCase "unresolvedTerms finds unresolved terms" $
        unresolvedTerms testFilters [[MetaTerm "rot", MetaTerm "x"], [MetaTerm "y"]] @?= ["x", "y"]
    , testCase "unresolvedTerms empty when all resolve" $
        unresolvedTerms testFilters [[MetaTerm "rot", MetaTerm "groß"]] @?= []
    ]
