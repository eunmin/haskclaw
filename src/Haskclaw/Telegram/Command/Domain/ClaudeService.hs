module Haskclaw.Telegram.Command.Domain.ClaudeService
  ( ClaudeService (..)
  , askClaude
  ) where

import Relude

import Effectful (Effect)
import Effectful.TH (makeEffect)

import Haskclaw.Telegram.Command.Domain.Types (ChatId, SessionId)

-- | Submit the user's input to Claude. Assistant-facing text is delivered
--   through the sink registered when interpreting this effect; the handler
--   only returns the new session id (Nothing on error or a missing session).
data ClaudeService :: Effect where
  AskClaude :: ChatId -> Maybe SessionId -> Text -> ClaudeService m (Maybe SessionId)

makeEffect ''ClaudeService
