{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan FromMisoString/ToMisoString instances for 'Id'.
-- Needed for route captures. Kept in a dedicated module to
-- contain the orphan warning suppression.
module Competences.Frontend.Common.MisoId () where

import Competences.Document.Id (Id, idToText, mkId)
import Miso.String (FromMisoString (..), ToMisoString (..), fromMisoString)

instance FromMisoString (Id a) where
  fromMisoStringEither s =
    case mkId (fromMisoString s) of
      Just i -> Right i
      Nothing -> Left "Invalid UUID"

instance ToMisoString (Id a) where
  toMisoString = toMisoString . idToText
