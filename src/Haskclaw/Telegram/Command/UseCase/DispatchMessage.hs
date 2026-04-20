module Haskclaw.Telegram.Command.UseCase.DispatchMessage
  ( dispatch
  , dispatchScheduled
  , TaskHandler
  ) where

import Relude

import Control.Concurrent.STM (TChan, newTChan, writeTChan)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
