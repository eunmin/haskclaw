module Haskclaw.Telegram.Command.UseCase.ProcessMessage
  ( processMessage
  ) where

import Relude

import Effectful (Eff, (:>))

import Haskclaw.Telegram.Command.Domain.ClaudeService (ClaudeService, askClaude)
import Haskclaw.Telegram.Command.Domain.Types (Message (..), SessionId)

-- | Forward the message text to Claude. Assistant text is streamed through
--   the interpreter's sink; we only report the resulting session id back to
--   the caller. Outer 'Nothing' means the user message had no text and was
--   ignored; inner 'Nothing' means Claude did not yield a session.
processMessage
  :: (ClaudeService :> es)
  => Maybe SessionId
  -> Message
  -> Eff es (Maybe (Maybe SessionId))
processMessage mSessionId msg = case msg.text of
  Nothing -> pure Nothing
  Just txt -> Just <$> askClaude msg.chatId mSessionId txt
