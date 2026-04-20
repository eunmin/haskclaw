module Haskclaw.Scheduler.Store
  ( schedulesPath
  , loadTasks
  , saveTasks
  , listAllChatIds
  , addTask
  , removeTask
  , updateTask
  ) where

import Relude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  , renameFile
  )
import System.FilePath ((</>))

import Haskclaw.Infra.Paths (chatsDir, ensureChatDir)
import Haskclaw.Scheduler.Types (ScheduledTask (..), TaskId)
import Haskclaw.Telegram.Command.Domain.Types (ChatId (..))

-- | Path to the schedule file for a chat: ~/.haskclaw/chats/<slug>/schedules.json.
--   Ensures the parent directory exists.
schedulesPath :: ChatId -> IO FilePath
schedulesPath cid = do
  dir <- ensureChatDir cid
  pure (dir </> "schedules.json")

-- | Load the list of tasks for a chat. Missing file yields an empty list.
loadTasks :: ChatId -> IO [ScheduledTask]
loadTasks cid = do
  path <- schedulesPath cid
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      bytes <- LBS.readFile path
      case Aeson.eitherDecode bytes of
        Right ts -> pure ts
        Left _   -> pure []

-- | Atomically replace the schedules file with the given tasks.
saveTasks :: ChatId -> [ScheduledTask] -> IO ()
saveTasks cid tasks = do
  path <- schedulesPath cid
  let tmp = path <> ".tmp"
  LBS.writeFile tmp (Aeson.encode tasks)
  renameFile tmp path

-- | Append a task to the chat's schedules.
addTask :: ChatId -> ScheduledTask -> IO ()
addTask cid task = do
  existing <- loadTasks cid
  saveTasks cid (existing <> [task])

-- | Remove a task by id. Returns True when a task was removed.
removeTask :: ChatId -> TaskId -> IO Bool
removeTask cid tid = do
  existing <- loadTasks cid
  let (kept, dropped) = List.partition ((/= tid) . taskId) existing
  if null dropped
    then pure False
    else do
      saveTasks cid kept
      pure True

-- | Replace a task with a modifier. Returns True when a matching id existed.
updateTask :: ChatId -> TaskId -> (ScheduledTask -> ScheduledTask) -> IO Bool
updateTask cid tid f = do
  existing <- loadTasks cid
  let (updated, anyMatch) = foldr step ([], False) existing
      step t (acc, found)
        | taskId t == tid = (f t : acc, True)
        | otherwise       = (t : acc, found)
  if anyMatch
    then saveTasks cid updated >> pure True
    else pure False

-- | List every chat id that currently has a directory under ~/.haskclaw/chats.
--   Used by the scheduler loop to scan all chats each tick.
listAllChatIds :: IO [ChatId]
listAllChatIds = do
  root <- chatsDir
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- listDirectory root
      pure $ mapMaybe parseSlug entries
  where
    parseSlug :: FilePath -> Maybe ChatId
    parseSlug s = case s of
      'n' : 'e' : 'g' : '_' : rest -> ChatId . negate <$> readMaybe rest
      digits                       -> ChatId <$> readMaybe digits
