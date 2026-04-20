module Haskclaw.Scheduler.Types
  ( TaskId (..)
  , TaskStatus (..)
  , ScheduledTask (..)
  , newTaskId
  ) where

import Relude

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Numeric (showHex)
import System.Random (randomRIO)

newtype TaskId = TaskId { unTaskId :: Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

data TaskStatus = TaskActive | TaskPaused
  deriving stock (Show, Eq)

instance ToJSON TaskStatus where
  toJSON TaskActive = String "active"
  toJSON TaskPaused = String "paused"

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \case
    "active" -> pure TaskActive
    "paused" -> pure TaskPaused
    other    -> fail ("unknown TaskStatus: " <> toString other)

data ScheduledTask = ScheduledTask
  { taskId    :: TaskId
  , cron      :: Text       -- 5-field cron expression, local time
  , prompt    :: Text
  , label     :: Maybe Text
  , status    :: TaskStatus
  , createdAt :: UTCTime
  , lastRunAt :: Maybe UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Short id: "task-<unix-seconds>-<4 hex>". The provided time seeds the stamp;
--   callers typically pass the current time to avoid a duplicate call.
newTaskId :: UTCTime -> IO TaskId
newTaskId now = do
  n <- randomRIO (0 :: Int, 0xFFFF)
  let secs = floor @Double @Integer (realToFrac (utcTimeToPOSIXSeconds now))
      suffix = pad4 (showHex n "")
  pure $ TaskId $ "task-" <> show secs <> "-" <> toText suffix
  where
    pad4 s = replicate (4 - length s) '0' <> s
