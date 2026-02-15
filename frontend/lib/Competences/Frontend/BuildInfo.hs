module Competences.Frontend.BuildInfo
  ( frontendVersion
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Paths_competences_frontend (version)

-- | Frontend version, derived from competences-frontend.cabal
frontendVersion :: Text
frontendVersion = T.pack (showVersion version)
