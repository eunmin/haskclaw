module Haskclaw.Telegram.Command.UseCase.ProcessMessage
  ( processMessage
  ) where

import Relude

import Effectful (Eff, (:>))

import Haskclaw.Telegram.Command.Domain.AssistantService (AssistantService, askAssistant)
import Haskclaw.Telegram.Command.Domain.Types (Message (..), SessionId)

-- | Forward the message text to the configured assistant. Assistant text is streamed through
--   the interpreter's sink; we only report the resulting session id back to
--   the caller. Outer 'Nothing' means the user message had no text and was
--   ignored; inner 'Nothing' means the assistant did not yield a session.
processMessage
  :: (AssistantService :> es)
  => Maybe SessionId
  -> Message
  -> Eff es (Maybe (Maybe SessionId))
processMessage mSessionId msg = case msg.text of
  Nothing -> pure Nothing
  Just txt -> Just <$> askAssistant msg.chatId mSessionId txt
