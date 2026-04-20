module Haskclaw.Util.ChatLog
  ( logChat
  , chatLogPath
  , formatChatLogLine
  ) where

import Relude

import Control.Exception (bracket)
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.FilePath ((</>))
import System.IO (hClose, openFile)

import Haskclaw.Infra.Paths (ensureChatDir)
import Haskclaw.Telegram.Command.Domain.Types (ChatId)

-- | Path to <chatDir>/bot.log (this function does ensure the chat dir exists).
chatLogPath :: ChatId -> IO FilePath
chatLogPath cid = do
  dir <- ensureChatDir cid
  pure (dir </> "bot.log")

-- | Format a single log line with a timestamp prefix (pure; for tests).
formatChatLogLine :: UTCTime -> Text -> Text
formatChatLogLine now msg =
  let ts = toText $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  in "[" <> ts <> "] " <> msg

-- | Emit a chat-scoped log line to both stdout and <chatDir>/bot.log.
logChat :: ChatId -> Text -> IO ()
logChat cid msg = do
  now <- getCurrentTime
  let line = formatChatLogLine now msg
  putTextLn line
  path <- chatLogPath cid
  -- Re-opens the file per call; cheap enough for our write volume.
  bracket (openFile path AppendMode) hClose $ \h ->
    TIO.hPutStrLn h line
