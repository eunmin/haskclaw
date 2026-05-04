module Haskclaw.Infra.Persistence.StateFile
  ( loadSessions
  , saveSessions
  , SessionStore
  , migrateLegacySessions
  ) where

import Relude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)

import Haskclaw.Infra.Paths (stateFilePath)
import Haskclaw.Telegram.Command.Domain.Types (AssistantProvider (..), ChatId, SessionId)

type SessionStore = Map ChatId (Map AssistantProvider SessionId)

loadSessions :: IO SessionStore
loadSessions = do
  path <- stateFilePath
  exists <- doesFileExist path
  if exists
    then do
      bytes <- LBS.readFile path
      case (Aeson.eitherDecode bytes :: Either String SessionStore) of
        Right sessions -> do
          putTextLn $ "Loaded " <> show (length sessions) <> " sessions from " <> toText path
          pure sessions
        Left newErr -> case (Aeson.eitherDecode bytes :: Either String (Map ChatId SessionId)) of
          Right legacySessions -> do
            let migrated = migrateLegacySessions legacySessions
            putTextLn $ "Migrated " <> show (length migrated)
              <> " legacy sessions from " <> toText path <> " as claude sessions"
            saveSessions migrated
            pure migrated
          Left legacyErr -> do
            putTextLn $ "Failed to parse " <> toText path <> ": "
              <> toText newErr <> "; legacy parse also failed: " <> toText legacyErr
            pure mempty
    else do
      putTextLn $ toText path <> " not found, starting fresh"
      pure mempty

saveSessions :: SessionStore -> IO ()
saveSessions sessions = do
  path <- stateFilePath
  LBS.writeFile path (Aeson.encode sessions)

migrateLegacySessions :: Map ChatId SessionId -> SessionStore
migrateLegacySessions =
  fmap (Map.singleton Claude)
