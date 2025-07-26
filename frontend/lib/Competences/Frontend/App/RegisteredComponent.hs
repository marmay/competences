module Competences.Frontend.App.RegisteredComponent
  ( RegisteredComponent (..)
  )
where

import Competences.Model.Competence (Competence)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | A RegisteredComponent can be spawned as a modal dialog or within the
-- main view. It also can register itself with the ComponentRegistry. In
-- that case, it will receive Model updates via a fast by-pass channel.
data RegisteredComponent
  = MainGrid
  | CompetenceEditor !Competence
  deriving (Eq, Show, Generic)

instance ToJSON RegisteredComponent

instance FromJSON RegisteredComponent
