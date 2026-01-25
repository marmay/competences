-- |
-- Module      : Competences.Frontend.Logging
-- Description : Conditional logging utilities
--
-- Provides logging functions that respect the DEBUG flag.
-- Debug messages only print when window.COMPETENCES_DEBUG = true.
-- Warnings and errors always print.
module Competences.Frontend.Logging
  ( logDebug
  , logInfo
  , logWarn
  , logError
  , isDebugEnabled
  ) where

import Control.Monad (when)
import Miso qualified as M
import Miso.DSL (fromJSVal, jsg, (!))

-- | Check if debug mode is enabled via window.COMPETENCES_DEBUG
isDebugEnabled :: IO Bool
isDebugEnabled = do
  result <- jsg "window" ! "COMPETENCES_DEBUG" >>= fromJSVal @Bool
  pure $ result == Just True

-- | Debug log - only prints when DEBUG = true
logDebug :: M.MisoString -> IO ()
logDebug msg = do
  enabled <- isDebugEnabled
  when enabled $ M.consoleLog msg

-- | Info log - only prints when DEBUG = true (same as debug)
logInfo :: M.MisoString -> IO ()
logInfo = logDebug

-- | Warning log - always prints
logWarn :: M.MisoString -> IO ()
logWarn = M.consoleWarn

-- | Error log - always prints
logError :: M.MisoString -> IO ()
logError = M.consoleError
