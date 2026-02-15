module Competences.Frontend.BuildInfo
  ( frontendVersion
  )
where

import Data.Text (Text)

-- | Frontend version, kept in sync with competences-frontend.cabal
frontendVersion :: Text
frontendVersion = "0.10.0.0"
