module Haskclaw.Telegram.Infra.Interpreter.ClaudeServiceInterpreter
  ( run
  ) where

import Relude

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import Haskclaw.Telegram.Command.Domain.ClaudeService (ClaudeService (..))
import qualified Haskclaw.Telegram.Infra.Gateway.ClaudeProcessGateway as Gateway
import Haskclaw.Util.ChatLog (logChat)

run
  :: (IOE :> es)
  => Eff (ClaudeService : es) a
  -> Eff es a
run = interpret $ \_ -> \case
  AskClaude cid mSessionId input -> do
    result <- liftIO $ Gateway.callClaude cid mSessionId input
    case result of
      Right (resp, newSid) -> pure (resp, Just newSid)
      Left err -> do
        liftIO $ logChat cid $ "claude error: " <> err
        pure ("Error: " <> err, Nothing)
