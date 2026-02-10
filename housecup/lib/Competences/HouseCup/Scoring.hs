module Competences.HouseCup.Scoring
  ( ScoreTable
  , computePoints
  )
where

import Competences.Document (Document)
import Competences.HouseCup.Config (ResolvedConfig (..))
import Data.Text (Text)

type ScoreTable = [(Text, Integer)]

-- | Compute house points by diffing two document states.
-- TODO: implement actual scoring rules.
computePoints :: ResolvedConfig -> Document -> Document -> ScoreTable
computePoints (ResolvedConfig houses) _docBefore _docAfter =
  [(name, 0) | (name, _userIds) <- houses]
