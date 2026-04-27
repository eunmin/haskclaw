module Haskclaw.Telegram.Command.Domain.Types
  ( ChatId (..)
  , UpdateId (..)
  , SessionId (..)
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
import Data.Aeson (FromJSON (..), ToJSON (..), FromJSONKey (..), ToJSONKey (..), withObject, (.:), (.:?))

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
  , sessions :: TVar (Map ChatId SessionId)
  , inFlight :: TVar (Map ChatId (Set Text))  -- ^ scheduled task ids currently queued or running, per chat
  }

newBotState :: MonadIO m => m BotState
newBotState = newBotStateWith mempty

newBotStateWith :: MonadIO m => Map ChatId SessionId -> m BotState
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
