module Haskclaw.Unit.Scheduler.CronSpec (spec) where

import Relude

import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Haskclaw.Scheduler.Cron (matchesTick, parseSchedule)

-- | Build a UTC tick at YYYY-MM-DD HH:MM.
tick :: Integer -> Int -> Int -> Int -> Int -> UTCTime
tick y m d h mi = UTCTime (fromGregorian y m d)
  (secondsToDiffTime (fromIntegral (h * 3600 + mi * 60)))

spec :: Spec
spec = do
  describe "parseSchedule" $ do
    it "accepts a valid 5-field expression" $
      parseSchedule "0 8 * * *" `shouldSatisfy` isRight

    it "rejects a malformed expression" $
      parseSchedule "not a cron" `shouldSatisfy` isLeft

    it "trims surrounding whitespace" $
      parseSchedule "  */5 * * * *  " `shouldSatisfy` isRight

  describe "matchesTick" $ do
    it "matches 0 8 * * * at 08:00" $ do
      let Right sc = parseSchedule "0 8 * * *"
      matchesTick sc (tick 2026 4 20 8 0) `shouldBe` True

    it "does not match 0 8 * * * at 08:01" $ do
      let Right sc = parseSchedule "0 8 * * *"
      matchesTick sc (tick 2026 4 20 8 1) `shouldBe` False

    it "matches */5 * * * * every 5 minutes" $ do
      let Right sc = parseSchedule "*/5 * * * *"
      matchesTick sc (tick 2026 4 20 13 5)  `shouldBe` True
      matchesTick sc (tick 2026 4 20 13 10) `shouldBe` True
      matchesTick sc (tick 2026 4 20 13 11) `shouldBe` False
