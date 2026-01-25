{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Clipboard
-- Description : Clipboard utilities for copy/paste operations
--
-- Provides functions for interacting with the browser clipboard API.
module Competences.Frontend.Clipboard
  ( copyToClipboard
  )
where

import Data.Text (Text)
import Miso.DSL (jsg, (!), (#))
import Miso.String (MisoString, ms)

-- | Copy text to the system clipboard
-- Uses the browser's navigator.clipboard.writeText API
copyToClipboard :: Text -> IO ()
copyToClipboard text = do
  navigator <- jsg ("navigator" :: MisoString)
  clipboard <- navigator ! ("clipboard" :: MisoString)
  -- Call writeText method with the text to copy
  _ <- clipboard # ("writeText" :: MisoString) $ [ms text]
  pure ()
