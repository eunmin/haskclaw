module Haskclaw.Scheduler.Loop
  ( runLoop
  , tickOnce
  , clearInFlight
  ) where

import Relude

import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (TimeOfDay (..), UTCTime, getZonedTime, timeToTimeOfDay, utctDayTime)

import Haskclaw.Scheduler.Cron (localTickFromZoned, matchesTick, parseSchedule)
import Haskclaw.Scheduler.Store (listAllChatIds, loadTasks)
import Haskclaw.Scheduler.Types (ScheduledTask (..), TaskId (..), TaskStatus (..))
import Haskclaw.Telegram.Command.Domain.Types (BotState (..), ChatId)
import Haskclaw.Telegram.Command.UseCase.DispatchMessage (TaskHandler, dispatchScheduled)
import Haskclaw.Util.ChatLog (logChat)

-- | Forever loop. Wakes up once per minute on the wall-clock minute boundary,
--   scans every chat directory for due active tasks, and enqueues each one.
runLoop :: BotState -> TaskHandler -> IO ()
runLoop botState spawnWorker = forever $ do
  sleepUntilNextMinute
  tickOnce botState spawnWorker

-- | Run a single tick. Exported for testing.
tickOnce :: BotState -> TaskHandler -> IO ()
tickOnce botState spawnWorker = do
  zt <- getZonedTime
  let tick = localTickFromZoned zt
  cids <- listAllChatIds
  forM_ cids $ \cid -> do
    tasks <- loadTasks cid
    let due = filter (isDue tick) tasks
    forM_ due $ \t -> do
      ok <- dispatchScheduled botState spawnWorker cid
              (unTaskId t.taskId) t.label t.prompt
      unless ok $
        logChat cid $ "scheduler: skip (in-flight) " <> unTaskId t.taskId

isDue :: UTCTime -> ScheduledTask -> Bool
isDue tick t = case t.status of
  TaskPaused -> False
  TaskActive -> case parseSchedule t.cron of
    Left _   -> False
    Right sc -> matchesTick sc tick

-- | Remove a scheduled-task id from the in-flight set. Worker must call this
--   after finishing a 'ScheduledRun' so the next tick can enqueue the task
--   again. No-op on tasks that aren't tracked.
clearInFlight :: BotState -> ChatId -> Text -> IO ()
clearInFlight botState cid tid = atomically $ do
  modifyTVar' botState.inFlight $ \m ->
    case Map.lookup cid m of
      Nothing -> m
      Just s  ->
        let s' = Set.delete tid s
        in if Set.null s' then Map.delete cid m else Map.insert cid s' m

-- | Sleep until the next wall-clock minute (plus a tiny buffer). Minute
--   precision is sufficient since cron fields are minute-level.
sleepUntilNextMinute :: IO ()
sleepUntilNextMinute = do
  zt <- getZonedTime
  let tod = timeToTimeOfDay (utctDayTime (localTickFromZoned zt))
      secondsIntoMinute = realToFrac (todSec tod) :: Double
      -- Wait for the rest of this minute + 500ms buffer.
      waitMicros :: Int
      waitMicros = max 1 (round ((60.0 - secondsIntoMinute) * 1000000)) + 500000
  threadDelay waitMicros
