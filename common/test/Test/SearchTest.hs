module Test.SearchTest (tests) where

import Competences.Search
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Search"
    [ parsingTests
    , matchingTests
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
