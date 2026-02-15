module Competences.Backend.BuildInfo
  ( backendVersion
  )
where

import Data.Text (Text)

-- | Backend version, kept in sync with competences-backend.cabal
backendVersion :: Text
backendVersion = "0.10.0.0"
