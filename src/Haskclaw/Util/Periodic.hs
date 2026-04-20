module Haskclaw.Util.Periodic
  ( withPeriodic
  ) where

import Relude

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)

-- | Run @action@ while invoking @tick@ every @intervalMicros@ microseconds in a
--   background thread. The tick thread is killed when @action@ returns or throws.
--   @tick@ fires once immediately at start, then on each interval.
withPeriodic :: Int -> IO () -> IO a -> IO a
withPeriodic intervalMicros tick action = bracket
  (forkIO $ forever $ do
     tick
     threadDelay intervalMicros)
  killThread
  (const action)
