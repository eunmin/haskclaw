module Haskclaw.Telegram.Command.UseCase.ProcessMessage
  ( processMessage
  ) where

import Relude

import Effectful (Eff, (:>))

import Haskclaw.Telegram.Command.Domain.ClaudeService (ClaudeService, askClaude)
import Haskclaw.Telegram.Command.Domain.Types (Message (..), SessionId)

-- | Forward the message text to Claude and return the response plus the
--   new session id (if any).
processMessage
  :: (ClaudeService :> es)
  => Maybe SessionId
  -> Message
  -> Eff es (Maybe (Text, Maybe SessionId))
processMessage mSessionId msg = case msg.text of
  Nothing -> pure Nothing
  Just txt -> Just <$> askClaude msg.chatId mSessionId txt
