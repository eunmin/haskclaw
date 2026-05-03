module Haskclaw.Unit.Telegram.Command.InMemoryAssistantService
  ( run
  , runWithSink
  ) where

import Relude

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import Haskclaw.Telegram.Command.Domain.AssistantService (AssistantService (..))
import Haskclaw.Telegram.Command.Domain.Types (ChatId, SessionId (..))

-- | In-memory implementation that streams @echo: <input>@ through the
--   provided @sink@ and returns a fixed session id. Useful for exercising
--   downstream sinks in tests.
runWithSink
  :: (IOE :> es)
  => (ChatId -> Text -> IO ())
  -> Eff (AssistantService : es) a
  -> Eff es a
runWithSink sink = interpret $ \_ -> \case
  AskAssistant cid _mSessionId input -> do
    liftIO $ sink cid ("echo: " <> input)
    pure (Just (SessionId "test-session-id"))

-- | Convenience interpreter that discards streamed text. Matches the
--   legacy contract tests relied on before the streaming rewrite.
run
  :: (IOE :> es)
  => Eff (AssistantService : es) a
  -> Eff es a
run = runWithSink (\_ _ -> pure ())
