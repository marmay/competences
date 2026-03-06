{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains orphan instances for Binary.
-- Import this module to bring these instances into scope.
module Competences.Common.BinaryOrphans () where

import Data.Binary (Binary (..))
import Data.Time (Day (ModifiedJulianDay), DiffTime, UTCTime (..), toModifiedJulianDay)

-- | Orphan instance for Day from time package.
-- Needed for serializing Assignment and Evidence which contain Day fields.
-- Serializes as the modified julian day number (Integer).
instance Binary Day where
  put = put . toModifiedJulianDay
  get = ModifiedJulianDay <$> get

-- | Orphan instance for DiffTime.
-- Serializes as a Rational (picoseconds).
instance Binary DiffTime where
  put = put . toRational
  get = fromRational <$> get

-- | Orphan instance for UTCTime.
-- Needed for serializing Submission which contains UTCTime fields.
instance Binary UTCTime where
  put t = put t.utctDay >> put t.utctDayTime
  get = UTCTime <$> get <*> get
