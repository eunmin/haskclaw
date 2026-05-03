module Haskclaw.Telegram.Infra.Interpreter.AssistantServiceInterpreter
  ( run
  ) where

import Relude

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import Haskclaw.Telegram.Command.Domain.AssistantService (AssistantService (..))
import Haskclaw.Telegram.Command.Domain.Types (ChatId)
import Haskclaw.Telegram.Infra.Gateway.AssistantProcessGateway (AssistantOptions)
import qualified Haskclaw.Telegram.Infra.Gateway.AssistantProcessGateway as Gateway
import Haskclaw.Util.ChatLog (logChat)

-- | Interpret 'AssistantService' by shelling out to the configured assistant and delivering
--   every assistant text block through @sink@. Errors are also funnelled
--   through the sink so the caller's transport gets a single stream of
--   user-visible text; the return value is only the resulting session id.
run
  :: (IOE :> es)
  => AssistantOptions
  -> (ChatId -> Text -> IO ())
  -> Eff (AssistantService : es) a
  -> Eff es a
run opts sink = interpret $ \_ -> \case
  AskAssistant cid mSessionId input -> do
    result <- liftIO $ Gateway.callAssistant opts (sink cid) cid mSessionId input
    case result of
      Right newSid -> pure (Just newSid)
      Left err -> do
        liftIO $ do
          logChat cid $ "assistant error: " <> err
          sink cid ("Error: " <> err)
        pure Nothing
