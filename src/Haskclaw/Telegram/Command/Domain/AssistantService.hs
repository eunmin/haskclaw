module Haskclaw.Telegram.Command.Domain.AssistantService
  ( AssistantService (..)
  , askAssistant
  ) where

import Relude

import Effectful (Effect)
import Effectful.TH (makeEffect)

import Haskclaw.Telegram.Command.Domain.Types (ChatId, SessionId)

-- | Submit the user's input to the configured assistant. Assistant-facing text is delivered
--   through the sink registered when interpreting this effect; the handler
--   only returns the new session id (Nothing on error or a missing session).
data AssistantService :: Effect where
  AskAssistant :: ChatId -> Maybe SessionId -> Text -> AssistantService m (Maybe SessionId)

makeEffect ''AssistantService
