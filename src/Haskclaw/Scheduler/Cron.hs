module Haskclaw.Scheduler.Cron
  ( Schedule
  , parseSchedule
  , matchesTick
  , localTickFromZoned
  ) where

import Relude

import qualified Data.Text as T
import Data.Time
  ( LocalTime
  , UTCTime
  , ZonedTime
  , localTimeToUTC
  , utc
  , zonedTimeToLocalTime
  )
import qualified System.Cron as Cron

-- | Opaque wrapper around 'Cron.CronSchedule'.
newtype Schedule = Schedule Cron.CronSchedule
  deriving stock (Show)

-- | Parse a 5-field cron expression ("m h dom mon dow").
parseSchedule :: Text -> Either Text Schedule
parseSchedule txt = case Cron.parseCronSchedule (T.strip txt) of
  Left err -> Left (toText err)
  Right cs -> Right (Schedule cs)

-- | Does the schedule match the given tick (minute precision)?
--   The tick is expected to already represent local wall-clock time as UTC;
--   use 'localTickFromZoned' to derive it.
matchesTick :: Schedule -> UTCTime -> Bool
matchesTick (Schedule cs) = Cron.scheduleMatches cs

-- | Convert the local wall-clock portion of a 'ZonedTime' into a UTC tick
--   suitable for 'matchesTick'. This treats local hh:mm as if it were UTC so
--   cron fields are interpreted in local time, matching common cron behaviour.
localTickFromZoned :: ZonedTime -> UTCTime
localTickFromZoned zt =
  let local :: LocalTime
      local = zonedTimeToLocalTime zt
  in localTimeToUTC utc local
