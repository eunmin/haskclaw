module Haskclaw.Telegram.Infra.Interpreter.TelegramApiInterpreter
  ( run
  , TelegramConfig (..)
  ) where

import Relude hiding (Reader, ask)

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader, ask)
import Network.HTTP.Client (Manager)

import Haskclaw.Telegram.Command.Domain.TelegramApi (TelegramApi (..))
import qualified Haskclaw.Telegram.Infra.Gateway.TelegramHttpGateway as Gateway

data TelegramConfig = TelegramConfig
  { token :: Text
  , manager :: Manager
  }

run
  :: (IOE :> es, Reader TelegramConfig :> es)
  => Eff (TelegramApi : es) a
  -> Eff es a
run = interpret $ \_ -> \case
  GetUpdates mOffset -> do
    cfg <- ask @TelegramConfig
    liftIO $ Gateway.fetchUpdates cfg.manager cfg.token mOffset
  SendMessage chatId text -> do
    cfg <- ask @TelegramConfig
    liftIO $ Gateway.postSendMessage cfg.manager cfg.token chatId text
  SendPhoto chatId path mCaption -> do
    cfg <- ask @TelegramConfig
    liftIO $ Gateway.postSendPhoto cfg.manager cfg.token chatId path mCaption
