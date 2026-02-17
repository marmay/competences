module Main (main) where

import Test.Markdown.GeometryTest (geometryTests)
import Test.Markdown.ParserTest (parserTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Markdown" [parserTests, geometryTests]
