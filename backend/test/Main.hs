module Main where

import Data.Function ((&))
import Effectful (Eff, IOE, runEff)
import Effectful.Process (Process, runProcess)
import Effectful.Temporary (Temporary, runTemporary)
import Util.Eff.TestDb (withTestDb, defaultTestDbSettings)

runTestM :: Eff '[Temporary, Process, IOE] a -> IO a
runTestM a = a & runTemporary & runProcess & runEff

main :: IO ()
main = runTestM $
  withTestDb (defaultTestDbSettings "schema.sql") $ do
    pure ()
