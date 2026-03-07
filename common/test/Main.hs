module Main (main) where

import Test.Query.MasteryTest qualified as Mastery
import Test.SearchTest qualified as Search
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "competences-common" [Mastery.tests, Search.tests]
