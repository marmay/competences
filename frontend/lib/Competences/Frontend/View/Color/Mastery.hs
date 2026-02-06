-- | Mastery status color palette.
--
-- Maps 'MasteryStatus' values to semantic color palettes using CSS variables
-- defined in @input.css@. The colors follow a graduated pattern:
--
-- * 'StreakTwoAssessed' - deep green (verified mastery)
-- * 'StreakTwoPlus' - medium green (strong mastery)
-- * 'OneSuccess' - light green (emerging mastery)
-- * 'OnlySillyMistakes' - yellow-green (understanding with errors)
-- * 'MasteryNotYet' - yellow (not yet achieved)
-- * 'NotTried' - no color (no data)
module Competences.Frontend.View.Color.Mastery
  ( masteryPalette
  )
where

import Competences.Frontend.View.Color (PaletteName (..))
import Competences.Query.Mastery (MasteryStatus (..))

-- | Get the color palette name for a mastery status.
-- Returns 'Nothing' for 'NotTried' (no visual indicator needed).
masteryPalette :: MasteryStatus -> Maybe PaletteName
masteryPalette StreakTwoAssessed = Just $ PaletteName "mastery-assessed"
masteryPalette StreakTwoPlus = Just $ PaletteName "mastery-strong"
masteryPalette OneSuccess = Just $ PaletteName "mastery-emerging"
masteryPalette OnlySillyMistakes = Just $ PaletteName "mastery-partial"
masteryPalette MasteryNotYet = Just $ PaletteName "mastery-not-yet"
masteryPalette NotTried = Nothing
