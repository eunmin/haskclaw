module Haskclaw.Unit.Util.PeriodicSpec (spec) where

import Relude

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), try)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Haskclaw.Util.Periodic (withPeriodic)

spec :: Spec
spec = describe "Util.Periodic.withPeriodic" $ do
  it "invokes tick periodically while the action runs" $ do
    counter <- newIORef (0 :: Int)
    -- 50ms interval over 220ms — expect ~5 ticks at 0/50/100/150/200ms.
    -- Allow scheduler jitter; we only assert at least 3 ticks.
    _ <- withPeriodic 50000 (modifyIORef' counter (+ 1)) (threadDelay 220000)
    n <- readIORef counter
    n `shouldSatisfy` (>= 3)

  it "stops the tick thread once the action finishes" $ do
    counter <- newIORef (0 :: Int)
    _ <- withPeriodic 20000 (modifyIORef' counter (+ 1)) (threadDelay 50000)
    after1 <- readIORef counter
    threadDelay 100000
    after2 <- readIORef counter
    after2 `shouldBe` after1

  it "cleans up the tick thread even when the action throws" $ do
    counter <- newIORef (0 :: Int)
    result <- try @ErrorCall $
      withPeriodic 20000 (modifyIORef' counter (+ 1)) (error "boom")
    case result of
      Left _ -> pure ()
      Right () -> expectationFailure "expected the exception to propagate"
    after1 <- readIORef counter
    threadDelay 100000
    after2 <- readIORef counter
    after2 `shouldBe` after1
