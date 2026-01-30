{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shared Arbitrary instances for property-based testing.
module Test.Generators
  ( -- * Re-exports (orphan instances defined here)
  )
where

import Competences.Document.Competence (Level, allLevels)
import Competences.Document.Evidence (Ability, SocialForm)
import Competences.Query.Mastery (AbilityBounds (..), LevelObservation (..), MasteryStatus)
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

instance Arbitrary MasteryStatus where
  arbitrary = arbitraryBoundedEnum

-- ============================================================================
-- LevelObservation
-- ============================================================================

instance Arbitrary LevelObservation where
  arbitrary = LevelObservation <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (LevelObservation l a s) =
    [LevelObservation l' a s | l' <- shrink l]
      ++ [LevelObservation l a' s | a' <- shrink a]
      ++ [LevelObservation l a s' | s' <- shrink s]

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
  shrink (FromAbove a b) =
    [FromAbove a' b | a' <- shrink a]
      ++ [FromAbove a b' | b' <- shrink b]
  shrink (FromBelow a) =
    [FromBelow a' | a' <- shrink a]
  shrink (FromBoth f c b) =
    [FromAbove f b, FromBelow c]
      ++ [FromBoth f' c b | f' <- shrink f]
      ++ [FromBoth f c' b | c' <- shrink c]
      ++ [FromBoth f c b' | b' <- shrink b]
