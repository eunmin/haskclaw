module Haskclaw.Telegram.Command.UseCase.DispatchMessage
  ( dispatch
  , dispatchScheduled
  , coalesceUserMsgs
  , mergeMessages
  , runInterruptible
  , TaskHandler
  ) where

import Relude

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
  ( TChan
  , newTChan
  , orElse
  , retry
  , tryReadTChan
  , unGetTChan
  , writeTChan
  )
import Control.Exception (bracket, throwIO, try)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Haskclaw.Telegram.Command.Domain.Types
  ( BotState (..)
  , ChatId
  , ChatTask (..)
  , Message (..)
  )

-- | Per-chat task handler. Invoked once when a new worker is spawned.
type TaskHandler = ChatId -> TChan ChatTask -> IO ()

-- | Enqueue a user message for its chat. Spawns a worker if none exists.
dispatch :: BotState -> TaskHandler -> Message -> IO ()
dispatch botState spawnWorker msg = do
  let cid = msg.chatId
  (chan, isNew) <- atomically $ enqueue botState cid (UserMsg msg)
  when isNew $ spawnWorker cid chan

-- | Enqueue a scheduled run. Skips when the same task id is already queued or
--   running for this chat (dedup across tick + execution window). Returns True
--   when the task was actually enqueued.
dispatchScheduled :: BotState -> TaskHandler -> ChatId -> Text -> Maybe Text -> Text -> IO Bool
dispatchScheduled botState spawnWorker cid tid mLabel promptTxt = do
  mResult <- atomically $ do
    inflight <- readTVar botState.inFlight
    let currently = Map.findWithDefault mempty cid inflight
    if Set.member tid currently
      then pure Nothing
      else do
        modifyTVar' botState.inFlight
          (Map.insert cid (Set.insert tid currently))
        result <- enqueue botState cid (ScheduledRun tid mLabel promptTxt)
        pure (Just result)
  case mResult of
    Nothing -> pure False
    Just (chan, isNew) -> do
      when isNew $ spawnWorker cid chan
      pure True

-- | Insert a task into the chat's TChan, creating the channel if needed.
--   Returns (chan, True) when a new TChan was created.
enqueue :: BotState -> ChatId -> ChatTask -> STM (TChan ChatTask, Bool)
enqueue botState cid task = do
  wmap <- readTVar botState.workers
  case Map.lookup cid wmap of
    Just ch -> do
      writeTChan ch task
      pure (ch, False)
    Nothing -> do
      ch <- newTChan
      writeTChan ch task
      modifyTVar' botState.workers (Map.insert cid ch)
      pure (ch, True)

-- | Drain consecutive UserMsg from the head; stop at the first non-UserMsg
--   (which is put back via unGetTChan) or when the channel is empty.
coalesceUserMsgs :: TChan ChatTask -> STM [Message]
coalesceUserMsgs chan = go []
  where
    go acc = do
      mt <- tryReadTChan chan
      case mt of
        Nothing -> pure (reverse acc)
        Just (UserMsg m) -> go (m : acc)
        Just other -> do
          unGetTChan chan other
          pure (reverse acc)

-- | Merge messages into one by joining texts with "\n". First message's
--   metadata (messageId, chatId, fromUsername) is kept.
mergeMessages :: Message -> [Message] -> Message
mergeMessages m [] = m
mergeMessages m rest = m { text = merged }
  where
    texts = mapMaybe (.text) (m : rest)
    merged = case texts of
      [] -> Nothing
      xs -> Just (T.intercalate "\n" xs)

-- | Run @action@ while watching @chan@ for a newly-arriving UserMsg.
--   Returns @Right@ when the action completes, @Left ()@ when interrupted.
--   The interrupting UserMsg is left in the queue for the caller to handle.
--   Rethrows any synchronous exception raised by @action@.
runInterruptible :: TChan ChatTask -> IO a -> IO (Either () a)
runInterruptible chan action = do
  resultVar <- newEmptyTMVarIO
  bracket
    (forkIO $ do
       r <- try @SomeException action
       atomically $ putTMVar resultVar r)
    killThread
    (\_tid -> do
       winner <- atomically $
         (Right <$> takeTMVar resultVar)
         `orElse`
         (peekUserMsgArrival chan >> pure (Left ()))
       case winner of
         Right (Right val) -> pure (Right val)
         Right (Left exc)  -> throwIO exc
         Left ()           -> pure (Left ()))

-- | STM action that succeeds once a UserMsg is present in @chan@. Drains and
--   restores the queue on every attempt so the transaction subscribes to tail
--   writes as well — @peekTChan@ alone only sees head changes and would miss a
--   UserMsg appended behind a ScheduledRun.
peekUserMsgArrival :: TChan ChatTask -> STM ()
peekUserMsgArrival chan = do
  tasks <- drainRestore chan
  unless (any isUserTask tasks) retry
  where
    isUserTask (UserMsg _) = True
    isUserTask _ = False

drainRestore :: TChan ChatTask -> STM [ChatTask]
drainRestore chan = do
  ts <- go []
  mapM_ (unGetTChan chan) ts
  pure (reverse ts)
  where
    go acc = do
      mt <- tryReadTChan chan
      case mt of
        Nothing -> pure acc
        Just t  -> go (t : acc)
