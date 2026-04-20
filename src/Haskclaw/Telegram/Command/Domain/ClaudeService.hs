module Haskclaw.Telegram.Command.Domain.ClaudeService
  ( ClaudeService (..)
  , askClaude
  ) where

import Relude

import Effectful (Effect)
import Effectful.TH (makeEffect)

import Haskclaw.Telegram.Command.Domain.Types (ChatId, SessionId)

-- | Returns Claude's response together with the new session id.
--   SessionId is Nothing when no session was created (e.g. on error).
data ClaudeService :: Effect where
  AskClaude :: ChatId -> Maybe SessionId -> Text -> ClaudeService m (Text, Maybe SessionId)

makeEffect ''ClaudeService
