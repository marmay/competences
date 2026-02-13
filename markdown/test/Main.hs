module Main (main) where

import Test.Markdown.ParserTest (parserTests)
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Markdown" [parserTests]
