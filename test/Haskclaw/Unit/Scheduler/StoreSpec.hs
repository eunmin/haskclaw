module Haskclaw.Unit.Scheduler.StoreSpec (spec) where

import Relude

import Control.Exception (finally)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Environment (setEnv, unsetEnv)
import qualified System.Environment as Env
import System.FilePath ((</>))
import Test.Hspec (Spec, around_, describe, it, shouldBe, shouldReturn)

import Haskclaw.Infra.Paths (ensureChatDir, ensureHaskclawDirs)
import Haskclaw.Scheduler.Store
  ( addTask
  , listAllChatIds
  , loadTasks
  , removeTask
  , updateTask
  )
import Haskclaw.Scheduler.Types (ScheduledTask (..), TaskId (..), TaskStatus (..))
import Haskclaw.Telegram.Command.Domain.Types (ChatId (..))

withTempHome :: IO () -> IO ()
withTempHome io = do
  tmp <- getTemporaryDirectory
  let fakeHome = tmp </> "haskclaw-store-spec"
  removePathForcibly fakeHome
  original <- Env.lookupEnv "HOME"
  setEnv "HOME" fakeHome
  io `finally` do
    removePathForcibly fakeHome
    case original of
      Just v  -> setEnv "HOME" v
      Nothing -> unsetEnv "HOME"

sampleTask :: TaskId -> Text -> ScheduledTask
sampleTask tid p = ScheduledTask
  { taskId    = tid
  , cron      = "0 8 * * *"
  , prompt    = p
  , label     = Nothing
  , status    = TaskActive
  , createdAt = UTCTime (fromGregorian 2026 4 20) (secondsToDiffTime 0)
  , lastRunAt = Nothing
  }

spec :: Spec
spec = around_ withTempHome $ do
  describe "Scheduler.Store" $ do
    it "loadTasks returns [] when the file is missing" $ do
      ensureHaskclawDirs
      loadTasks (ChatId 1) `shouldReturn` []

    it "addTask then loadTasks round-trips" $ do
      ensureHaskclawDirs
      addTask (ChatId 1) (sampleTask (TaskId "t1") "run weather")
      ts <- loadTasks (ChatId 1)
      fmap prompt ts `shouldBe` ["run weather"]

    it "removeTask deletes an existing task and reports True" $ do
      ensureHaskclawDirs
      addTask (ChatId 1) (sampleTask (TaskId "t1") "a")
      addTask (ChatId 1) (sampleTask (TaskId "t2") "b")
      removeTask (ChatId 1) (TaskId "t1") `shouldReturn` True
      ts <- loadTasks (ChatId 1)
      fmap taskId ts `shouldBe` [TaskId "t2"]

    it "removeTask reports False for an unknown id" $ do
      ensureHaskclawDirs
      addTask (ChatId 1) (sampleTask (TaskId "t1") "a")
      removeTask (ChatId 1) (TaskId "missing") `shouldReturn` False

    it "updateTask applies the modifier" $ do
      ensureHaskclawDirs
      addTask (ChatId 1) (sampleTask (TaskId "t1") "old")
      _ <- updateTask (ChatId 1) (TaskId "t1") (\t -> t { prompt = "new" })
      ts <- loadTasks (ChatId 1)
      fmap prompt ts `shouldBe` ["new"]

    it "listAllChatIds returns every chat directory seen" $ do
      ensureHaskclawDirs
      _ <- ensureChatDir (ChatId 1)
      _ <- ensureChatDir (ChatId (-99))
      cids <- listAllChatIds
      sort cids `shouldBe` sort [ChatId 1, ChatId (-99)]
