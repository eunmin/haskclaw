module Haskclaw.Unit.Telegram.Command.InMemoryClaudeService
  ( run
  ) where

import Relude

import Effectful (Eff)
import Effectful.Dispatch.Dynamic (interpret)

import Haskclaw.Telegram.Command.Domain.ClaudeService (ClaudeService (..))
import Haskclaw.Telegram.Command.Domain.Types (SessionId (..))

-- | In-memory implementation that echoes the input with an "echo: " prefix
--   and returns a fixed session id.
run
  :: Eff (ClaudeService : es) a
  -> Eff es a
run = interpret $ \_ -> \case
  AskClaude _cid _mSessionId input ->
    pure ("echo: " <> input, Just (SessionId "test-session-id"))
