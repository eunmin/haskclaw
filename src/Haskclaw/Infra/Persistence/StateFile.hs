module Haskclaw.Infra.Persistence.StateFile
  ( loadSessions
  , saveSessions
  ) where

import Relude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import System.Directory (doesFileExist)

import Haskclaw.Infra.Paths (stateFilePath)
import Haskclaw.Telegram.Command.Domain.Types (ChatId, SessionId)

loadSessions :: IO (Map ChatId SessionId)
loadSessions = do
  path <- stateFilePath
  exists <- doesFileExist path
  if exists
    then do
      bytes <- LBS.readFile path
      case Aeson.eitherDecode bytes of
        Right sessions -> do
          putTextLn $ "Loaded " <> show (length sessions) <> " sessions from " <> toText path
          pure sessions
        Left err -> do
          putTextLn $ "Failed to parse " <> toText path <> ": " <> toText err
          pure mempty
    else do
      putTextLn $ toText path <> " not found, starting fresh"
      pure mempty

saveSessions :: Map ChatId SessionId -> IO ()
saveSessions sessions = do
  path <- stateFilePath
  LBS.writeFile path (Aeson.encode sessions)
