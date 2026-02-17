module Main (main) where

import Test.Markdown.GeometryTest (geometryTests)
import Test.Markdown.ParserTest (parserTests)
import Test.Markdown.ValidationTest (validationTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Markdown" [parserTests, geometryTests, validationTests]
