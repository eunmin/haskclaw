module Haskclaw.Telegram.Command.UseCase.PollUpdates
  ( pollOnce
  ) where

import Relude

import Effectful (Eff, (:>))

import Haskclaw.Telegram.Command.Domain.TelegramApi (TelegramApi, getUpdates)
import Data.List (maximum)

import Haskclaw.Telegram.Command.Domain.Types (Message, Update (..), UpdateId (..))

-- | Poll once and return the fetched messages along with the next offset.
pollOnce
  :: (TelegramApi :> es)
  => Maybe UpdateId
  -> Eff es ([Message], Maybe UpdateId)
pollOnce offset = do
  updates <- getUpdates offset
  let messages = mapMaybe (.message) updates
      nextOffset = case updates of
        [] -> offset
        _  -> Just $ let UpdateId lastId = maximum (map (.updateId) updates)
                     in UpdateId (lastId + 1)
  pure (messages, nextOffset)
