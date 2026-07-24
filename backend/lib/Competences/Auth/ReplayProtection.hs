-- | Provides a simple, ephemeral data structure that stops a single
-- IdentityAssertion being consumed multiple times for a single service.
module Competences.Auth.ReplayProtection
  ( ConsumedLog
  , mkConsumedLog
  , ensureUnconsumed
  )
  where

import Control.Concurrent.STM (TVar, stateTVar, atomically, newTVarIO)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)

newtype ConsumedLog = ConsumedLog (TVar [(UUID, UTCTime)])

-- | Creates a new ConsumedLog.
mkConsumedLog :: IO ConsumedLog
mkConsumedLog = ConsumedLog <$> newTVarIO []

-- | Manages the ConsumedLog and returns, whether that assertion has not been consumed yet.
--
-- `assertionId` refers to the id of the IdentityAssertion and
-- `validUntil` to the latest point in time, when that assertion
-- would be successfully consumed. That includes the allowed clock
-- skew.
ensureUnconsumed :: UUID -> UTCTime -> ConsumedLog -> IO Bool
ensureUnconsumed assertionId validUntil (ConsumedLog consumed) = do
  now <- getCurrentTime
  atomically $ stateTVar consumed $ \consumed' ->
    let stillConsumed = filter ((>= now) . snd) consumed'
     in if assertionId `elem` map fst stillConsumed
           then (False, stillConsumed)
           else (True, (assertionId, validUntil) : stillConsumed)
