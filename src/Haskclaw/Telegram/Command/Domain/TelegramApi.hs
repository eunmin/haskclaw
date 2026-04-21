module Haskclaw.Telegram.Command.Domain.TelegramApi
  ( TelegramApi (..)
  , getUpdates
  , sendMessage
  , sendPhoto
  ) where

import Relude

import Effectful (Effect)
import Effectful.TH (makeEffect)

import Haskclaw.Telegram.Command.Domain.Types (ChatId, Update, UpdateId)

data TelegramApi :: Effect where
  GetUpdates :: Maybe UpdateId -> TelegramApi m [Update]
  SendMessage :: ChatId -> Text -> TelegramApi m ()
  SendPhoto :: ChatId -> FilePath -> Maybe Text -> TelegramApi m ()

makeEffect ''TelegramApi
