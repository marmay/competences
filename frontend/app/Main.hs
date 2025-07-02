module Main where

import Language.Javascript.JSaddle.Warp (run)

import MyLib (app)

main :: IO ()
main = run 8081 app

