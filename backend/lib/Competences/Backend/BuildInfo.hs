module Competences.Backend.BuildInfo
  ( backendVersion
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Paths_competences_backend (version)

-- | Backend version, derived from competences-backend.cabal
backendVersion :: Text
backendVersion = T.pack (showVersion version)
