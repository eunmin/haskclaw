module Haskclaw.Unit.Telegram.Command.UseCase.DispatchMessageSpec (spec) where

import Relude

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTChan, readTChan, writeTChan)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Haskclaw.Telegram.Command.Domain.Types
  ( BotState (..)
  , ChatId (..)
  , ChatTask (..)
  , Message (..)
  , newBotState
  )
import Haskclaw.Telegram.Command.UseCase.DispatchMessage
  ( coalesceUserMsgs
  , dispatch
  , dispatchScheduled
  , mergeMessages
  , runInterruptible
  )

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

  describe "coalesceUserMsgs" $ do
    it "returns [] when the channel is empty" $ do
      chan <- atomically newTChan
      result <- atomically $ coalesceUserMsgs chan
      result `shouldBe` []

    it "drains all consecutive UserMsgs in order" $ do
      chan <- atomically newTChan
      let msgs = [mkMessage (ChatId 1) ("m" <> show (i :: Int)) | i <- [1..3]]
      atomically $ mapM_ (writeTChan chan . UserMsg) msgs
      result <- atomically $ coalesceUserMsgs chan
      map (.text) result `shouldBe` [Just "m1", Just "m2", Just "m3"]

    it "stops at a ScheduledRun and leaves it at the head" $ do
      chan <- atomically newTChan
      atomically $ do
        writeTChan chan (UserMsg (mkMessage (ChatId 1) "a"))
        writeTChan chan (ScheduledRun "t" Nothing "p")
        writeTChan chan (UserMsg (mkMessage (ChatId 1) "b"))
      result <- atomically $ coalesceUserMsgs chan
      map (.text) result `shouldBe` [Just "a"]
      next <- atomically $ readTChan chan
      case next of
        ScheduledRun tid _ _ -> tid `shouldBe` "t"
        _ -> expectationFailure "expected ScheduledRun at head"

    it "returns [] when a ScheduledRun is at the head" $ do
      chan <- atomically newTChan
      atomically $ do
        writeTChan chan (ScheduledRun "t" Nothing "p")
        writeTChan chan (UserMsg (mkMessage (ChatId 1) "a"))
      result <- atomically $ coalesceUserMsgs chan
      result `shouldBe` []

  describe "mergeMessages" $ do
    it "returns the message unchanged with no extras" $ do
      let m = mkMessage (ChatId 1) "hi"
      mergeMessages m [] `shouldBe` m

    it "joins texts with newlines, keeps first message's metadata" $ do
      let m1 = (mkMessage (ChatId 1) "a") { messageId = 10, fromUsername = Just "alice" }
          m2 = (mkMessage (ChatId 1) "b") { messageId = 11 }
          m3 = (mkMessage (ChatId 1) "c") { messageId = 12 }
          merged = mergeMessages m1 [m2, m3]
      merged.text `shouldBe` Just "a\nb\nc"
      merged.messageId `shouldBe` 10
      merged.fromUsername `shouldBe` Just "alice"

    it "skips messages with no text" $ do
      let m1 = mkMessage (ChatId 1) "a"
          m2 = m1 { text = Nothing }
          m3 = mkMessage (ChatId 1) "c"
          merged = mergeMessages m1 [m2, m3]
      merged.text `shouldBe` Just "a\nc"

  describe "runInterruptible" $ do
    it "returns Right when the action completes first" $ do
      chan <- atomically newTChan
      result <- runInterruptible chan (pure (42 :: Int))
      result `shouldBe` Right 42

    it "returns Left when a UserMsg arrives during the action" $ do
      chan <- atomically newTChan
      gate <- newEmptyMVar
      resVar <- newEmptyMVar
      _ <- forkIO $
        runInterruptible chan (takeMVar gate >> pure (1 :: Int)) >>= putMVar resVar
      threadDelay 20000
      atomically $ writeTChan chan (UserMsg (mkMessage (ChatId 1) "incoming"))
      r <- takeMVar resVar
      r `shouldBe` Left ()

    it "does not interrupt on a ScheduledRun alone" $ do
      chan <- atomically newTChan
      gate <- newEmptyMVar
      resVar <- newEmptyMVar
      _ <- forkIO $
        runInterruptible chan (takeMVar gate >> pure (7 :: Int)) >>= putMVar resVar
      threadDelay 20000
      atomically $ writeTChan chan (ScheduledRun "t" Nothing "p")
      threadDelay 20000
      putMVar gate ()
      r <- takeMVar resVar
      r `shouldBe` Right 7

    it "interrupts on UserMsg even when a ScheduledRun is ahead in the queue" $ do
      chan <- atomically newTChan
      atomically $ writeTChan chan (ScheduledRun "t" Nothing "p")
      gate <- newEmptyMVar
      resVar <- newEmptyMVar
      _ <- forkIO $
        runInterruptible chan (takeMVar gate >> pure (99 :: Int)) >>= putMVar resVar
      threadDelay 20000
      atomically $ writeTChan chan (UserMsg (mkMessage (ChatId 1) "new"))
      r <- takeMVar resVar
      r `shouldBe` Left ()
