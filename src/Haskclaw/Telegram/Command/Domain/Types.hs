module Haskclaw.Telegram.Command.Domain.Types
  ( ChatId (..)
  , UpdateId (..)
  , SessionId (..)
  , AssistantProvider (..)
  , Message (..)
  , MessageEntity (..)
  , Update (..)
  , GetUpdatesResponse (..)
  , GetMeResponse (..)
  , ChatTask (..)
  , BotState (..)
  , newBotState
  , newBotStateWith
  ) where

import Relude

import Control.Concurrent.STM (TChan)
import Data.Aeson
  ( FromJSON (..)
  , FromJSONKey (..)
  , FromJSONKeyFunction (..)
  , ToJSON (..)
  , ToJSONKey (..)
  , ToJSONKeyFunction (..)
  , Value (..)
  , withObject
  , withText
  , (.:)
  , (.:?)
  )
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T

-- | Either a fresh user message or a scheduled prompt. Worker handles both
--   sequentially, so scheduled runs never race with ongoing conversation.
--   ScheduledRun args (in order): task id (for dedup), optional label, prompt.
data ChatTask
  = UserMsg Message
  | ScheduledRun Text (Maybe Text) Text
  deriving stock (Show, Eq)

data BotState = BotState
  { offset   :: TVar (Maybe UpdateId)
  , workers  :: TVar (Map ChatId (TChan ChatTask))
  , sessions :: TVar (Map ChatId (Map AssistantProvider SessionId))
  , inFlight :: TVar (Map ChatId (Set Text))  -- ^ scheduled task ids currently queued or running, per chat
  }

newBotState :: MonadIO m => m BotState
newBotState = newBotStateWith mempty

newBotStateWith :: MonadIO m => Map ChatId (Map AssistantProvider SessionId) -> m BotState
newBotStateWith initialSessions = liftIO $ BotState
  <$> newTVarIO Nothing
  <*> newTVarIO mempty
  <*> newTVarIO initialSessions
  <*> newTVarIO mempty

newtype ChatId = ChatId Int64
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

newtype UpdateId = UpdateId Int64
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, Num)

newtype SessionId = SessionId Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

data AssistantProvider
  = Claude
  | Codex
  deriving stock (Show, Eq, Ord)

assistantProviderText :: AssistantProvider -> Text
assistantProviderText = \case
  Claude -> "claude"
  Codex  -> "codex"

parseAssistantProviderText :: Text -> Maybe AssistantProvider
parseAssistantProviderText t = case T.toLower t of
  "claude" -> Just Claude
  "codex"  -> Just Codex
  _        -> Nothing

instance ToJSON AssistantProvider where
  toJSON = String . assistantProviderText

instance FromJSON AssistantProvider where
  parseJSON = withText "AssistantProvider" $ \t ->
    maybe (fail ("unknown assistant provider: " <> toString t)) pure
      (parseAssistantProviderText t)

instance ToJSONKey AssistantProvider where
  toJSONKey =
    ToJSONKeyText
      (Key.fromText . assistantProviderText)
      (Encoding.text . assistantProviderText)

instance FromJSONKey AssistantProvider where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    maybe (fail ("unknown assistant provider: " <> toString t)) pure
      (parseAssistantProviderText t)

data MessageEntity = MessageEntity
  { entityType :: Text
  , entityOffset :: Int
  , entityLength :: Int
  } deriving stock (Show, Eq)

instance FromJSON MessageEntity where
  parseJSON = withObject "MessageEntity" $ \v -> do
    entityType <- v .: "type"
    entityOffset <- v .: "offset"
    entityLength <- v .: "length"
    pure MessageEntity{..}

data Message = Message
  { messageId :: Int64
  , chatId :: ChatId
  , chatType :: Text
  , text :: Maybe Text
  , fromUsername :: Maybe Text
  , entities :: [MessageEntity]
  , replyToFromUsername :: Maybe Text
  } deriving stock (Show, Eq)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> do
    messageId <- v .: "message_id"
    chat <- v .: "chat"
    chatId <- chat .: "id"
    chatType <- chat .: "type"
    text <- v .:? "text"
    from <- v .:? "from"
    fromUsername <- case from of
      Nothing -> pure Nothing
      Just f -> f .:? "username"
    entities <- fromMaybe [] <$> v .:? "entities"
    replyTo <- v .:? "reply_to_message"
    replyToFromUsername <- case replyTo of
      Nothing -> pure Nothing
      Just rt -> do
        rtFrom <- rt .:? "from"
        case rtFrom of
          Nothing -> pure Nothing
          Just f -> f .:? "username"
    pure Message{..}

data Update = Update
  { updateId :: UpdateId
  , message :: Maybe Message
  } deriving stock (Show, Eq)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \v -> do
    updateId <- v .: "update_id"
    message <- v .:? "message"
    pure Update{..}

newtype GetUpdatesResponse = GetUpdatesResponse
  { result :: [Update]
  } deriving stock (Show, Eq)

instance FromJSON GetUpdatesResponse where
  parseJSON = withObject "GetUpdatesResponse" $ \v -> do
    result <- v .: "result"
    pure GetUpdatesResponse{..}

newtype GetMeResponse = GetMeResponse
  { username :: Maybe Text
  } deriving stock (Show, Eq)

instance FromJSON GetMeResponse where
  parseJSON = withObject "GetMeResponse" $ \v -> do
    res <- v .: "result"
    username <- res .:? "username"
    pure GetMeResponse{..}
