{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shared Arbitrary instances for property-based testing.
module Test.Generators
  ( -- * Re-exports (orphan instances defined here)
  )
where

import Competences.Document.ActivityType (ActivityType)
import Competences.Document.Competence (Level, allLevels)
import Competences.Document.Evidence (Ability, SocialForm)
import Competences.Query.Mastery (AbilityBounds (..), EvidenceQuality (..), LevelObservation (..), MasteryStatus)
import Test.QuickCheck

-- ============================================================================
-- Enums (all Bounded + Enum)
-- ============================================================================

instance Arbitrary Ability where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SocialForm where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Level where
  arbitrary = elements allLevels

instance Arbitrary ActivityType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MasteryStatus where
  arbitrary = arbitraryBoundedEnum

-- ============================================================================
-- EvidenceQuality
-- ============================================================================

instance Arbitrary EvidenceQuality where
  arbitrary = EvidenceQuality <$> arbitrary <*> arbitrary
  shrink (EvidenceQuality i a) =
    [EvidenceQuality i' a | i' <- shrink i]
      ++ [EvidenceQuality i a' | a' <- shrink a]

-- ============================================================================
-- LevelObservation
-- ============================================================================

instance Arbitrary LevelObservation where
  arbitrary = LevelObservation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (LevelObservation l a s at) =
    [LevelObservation l' a s at | l' <- shrink l]
      ++ [LevelObservation l a' s at | a' <- shrink a]
      ++ [LevelObservation l a s' at | s' <- shrink s]
      ++ [LevelObservation l a s at' | at' <- shrink at]

-- ============================================================================
-- AbilityBounds
-- ============================================================================

instance Arbitrary AbilityBounds where
  arbitrary =
    oneof
      [ FromAbove <$> arbitrary <*> arbitrary
      , FromBelow <$> arbitrary
      , FromBoth <$> arbitrary <*> arbitrary <*> arbitrary
      ]
  shrink (FromAbove a q) =
    [FromAbove a' q | a' <- shrink a]
      ++ [FromAbove a q' | q' <- shrink q]
  shrink (FromBelow a) =
    [FromBelow a' | a' <- shrink a]
  shrink (FromBoth f c q) =
    [FromAbove f q, FromBelow c]
      ++ [FromBoth f' c q | f' <- shrink f]
      ++ [FromBoth f c' q | c' <- shrink c]
      ++ [FromBoth f c q' | q' <- shrink q]
