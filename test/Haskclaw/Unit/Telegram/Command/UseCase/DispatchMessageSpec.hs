module Haskclaw.Unit.Telegram.Command.UseCase.DispatchMessageSpec (spec) where

import Relude

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (readTChan)
import Test.Hspec (Spec, describe, it, shouldBe)

import Haskclaw.Telegram.Command.Domain.Types
  ( BotState (..)
  , ChatId (..)
  , ChatTask (..)
  , Message (..)
  , newBotState
  )
import Haskclaw.Telegram.Command.UseCase.DispatchMessage (dispatch, dispatchScheduled)

mkMessage :: ChatId -> Text -> Message
mkMessage cid txt = Message
  { messageId = 1
  , chatId = cid
  , text = Just txt
  , fromUsername = Nothing
  }

spec :: Spec
spec = describe "DispatchMessage.dispatch" $ do
  it "spawns a worker and enqueues the message for a new chat" $ do
    botState <- newBotState
    spawnedRef <- newIORef []
    let handler cid _chan = modifyIORef' spawnedRef (cid :)
    dispatch botState handler (mkMessage (ChatId 1) "hello")
    spawned <- readIORef spawnedRef
    spawned `shouldBe` [ChatId 1]
    wmap <- readTVarIO botState.workers
    length wmap `shouldBe` 1

  it "reuses the existing worker for a known chat" $ do
    botState <- newBotState
    spawnCount <- newIORef (0 :: Int)
    let handler _cid _chan = modifyIORef' spawnCount (+ 1)
    dispatch botState handler (mkMessage (ChatId 1) "first")
    dispatch botState handler (mkMessage (ChatId 1) "second")
    count <- readIORef spawnCount
    count `shouldBe` 1

  it "spawns a separate worker per chat" $ do
    botState <- newBotState
    spawnedRef <- newIORef []
    let handler cid _chan = modifyIORef' spawnedRef (cid :)
    dispatch botState handler (mkMessage (ChatId 1) "a")
    dispatch botState handler (mkMessage (ChatId 2) "b")
    spawned <- readIORef spawnedRef
    sort spawned `shouldBe` [ChatId 1, ChatId 2]

  it "enqueues the message onto the correct channel" $ do
    botState <- newBotState
    received <- newIORef []
    let handler _cid chan = void $ forkIO $ do
          task <- atomically $ readTChan chan
          case task of
            UserMsg msg ->
              modifyIORef' received ((msg.chatId, fromMaybe "" msg.text) :)
            _ -> pure ()
    dispatch botState handler (mkMessage (ChatId 1) "hello")
    threadDelay 10000
    msgs <- readIORef received
    msgs `shouldBe` [(ChatId 1, "hello")]

  describe "dispatchScheduled" $ do
    it "enqueues the scheduled task and marks it in-flight" $ do
      botState <- newBotState
      spawned <- newIORef (0 :: Int)
      let handler _cid _chan = modifyIORef' spawned (+ 1)
      ok <- dispatchScheduled botState handler (ChatId 1) "task-1" (Just "daily") "run it"
      ok `shouldBe` True
      count <- readIORef spawned
      count `shouldBe` 1

    it "skips re-enqueuing the same task id while it is in flight" $ do
      botState <- newBotState
      let handler _cid _chan = pure ()
      _ <- dispatchScheduled botState handler (ChatId 1) "task-1" Nothing "p"
      second <- dispatchScheduled botState handler (ChatId 1) "task-1" Nothing "p"
      second `shouldBe` False
