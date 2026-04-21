module Haskclaw.Telegram.Infra.Interpreter.ClaudeServiceInterpreter
  ( run
  ) where

import Relude

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import Haskclaw.Telegram.Command.Domain.ClaudeService (ClaudeService (..))
import Haskclaw.Telegram.Command.Domain.Types (ChatId)
import qualified Haskclaw.Telegram.Infra.Gateway.ClaudeProcessGateway as Gateway
import Haskclaw.Util.ChatLog (logChat)

-- | Interpret 'ClaudeService' by shelling out to @claude -p@ and delivering
--   every assistant text block through @sink@. Errors are also funnelled
--   through the sink so the caller's transport gets a single stream of
--   user-visible text; the return value is only the resulting session id.
run
  :: (IOE :> es)
  => (ChatId -> Text -> IO ())
  -> Eff (ClaudeService : es) a
  -> Eff es a
run sink = interpret $ \_ -> \case
  AskClaude cid mSessionId input -> do
    result <- liftIO $ Gateway.callClaude (sink cid) cid mSessionId input
    case result of
      Right newSid -> pure (Just newSid)
      Left err -> do
        liftIO $ do
          logChat cid $ "claude error: " <> err
          sink cid ("Error: " <> err)
        pure Nothing
