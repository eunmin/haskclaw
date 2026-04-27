module Haskclaw.Unit.Telegram.Command.InMemoryTelegramApi
  ( run
  ) where

import Relude

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)

import Haskclaw.Telegram.Command.Domain.TelegramApi (TelegramApi (..))
import Haskclaw.Telegram.Command.Domain.Types (Update (..), UpdateId (..))

-- | Return updates stored in an IORef whose updateId is >= the offset.
run
  :: (IOE :> es)
  => IORef [Update]
  -> Eff (TelegramApi : es) a
  -> Eff es a
run ref = interpret $ \_ -> \case
  GetUpdates mOffset -> liftIO $ do
    updates <- readIORef ref
    pure $ case mOffset of
      Nothing -> updates
      Just (UpdateId oid) ->
        filter (\u -> let UpdateId uid = u.updateId in uid >= oid) updates
  GetMe -> pure (Just "testbot")
  SendMessage _chatId _text -> pure ()
  SendPhoto _chatId _path _caption -> pure ()
