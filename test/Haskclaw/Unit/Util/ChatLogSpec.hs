module Haskclaw.Unit.Util.ChatLogSpec (spec) where

import Relude

import Control.Exception (finally)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import System.Directory (doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Environment (setEnv, unsetEnv)
import qualified System.Environment as Env
import System.FilePath ((</>))
import Test.Hspec (Spec, around_, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Haskclaw.Infra.Paths (ensureHaskclawDirs)
import Haskclaw.Telegram.Command.Domain.Types (ChatId (..))
import Haskclaw.Util.ChatLog (chatLogPath, formatChatLogLine, logChat)

withTempHome :: IO () -> IO ()
withTempHome io = do
  tmp <- getTemporaryDirectory
  let fakeHome = tmp </> "haskclaw-chatlog-spec"
  removePathForcibly fakeHome
  original <- Env.lookupEnv "HOME"
  setEnv "HOME" fakeHome
  io `finally` do
    removePathForcibly fakeHome
    case original of
      Just v -> setEnv "HOME" v
      Nothing -> unsetEnv "HOME"

spec :: Spec
spec = do
  describe "formatChatLogLine" $ do
    it "prepends an ISO-style timestamp" $ do
      let now = UTCTime (fromGregorian 2026 4 19) (secondsToDiffTime 0)
      formatChatLogLine now "hello" `shouldBe` "[2026-04-19 00:00:00] hello"

  around_ withTempHome $ describe "logChat" $ do
    it "appends messages to <chatDir>/bot.log" $ do
      ensureHaskclawDirs
      logChat (ChatId 42) "first line"
      logChat (ChatId 42) "second line"
      path <- chatLogPath (ChatId 42)
      doesFileExist path `shouldReturn` True
      contents <- TIO.readFile path
      contents `shouldSatisfy` T.isInfixOf "first line"
      contents `shouldSatisfy` T.isInfixOf "second line"

    it "writes to a separate file per chat_id" $ do
      ensureHaskclawDirs
      logChat (ChatId 1) "only one"
      logChat (ChatId 2) "only two"
      p1 <- chatLogPath (ChatId 1)
      p2 <- chatLogPath (ChatId 2)
      c1 <- TIO.readFile p1
      c2 <- TIO.readFile p2
      c1 `shouldSatisfy` T.isInfixOf "only one"
      c1 `shouldSatisfy` (not . T.isInfixOf "only two")
      c2 `shouldSatisfy` T.isInfixOf "only two"
      c2 `shouldSatisfy` (not . T.isInfixOf "only one")
