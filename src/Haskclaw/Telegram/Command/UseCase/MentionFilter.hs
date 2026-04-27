module Haskclaw.Telegram.Command.UseCase.MentionFilter
  ( DispatchMode (..)
  , shouldDispatch
  , parseDispatchMode
  ) where

import Relude

import qualified Data.Text as T

import Haskclaw.Telegram.Command.Domain.Types (Message (..), MessageEntity (..))

data DispatchMode
  = AllMessages
  | MentionOnly
  deriving stock (Show, Eq)

-- | Parse dispatch mode from program args. Defaults to MentionOnly.
parseDispatchMode :: [String] -> DispatchMode
parseDispatchMode args
  | "--all" `elem` args = AllMessages
  | "--all-messages" `elem` args = AllMessages
  | otherwise = MentionOnly

-- | Decide whether a message should be forwarded to Claude.
--   Private chats always pass. In group chats with MentionOnly, the message
--   must mention the bot via a `mention` or `bot_command` entity, or be a
--   reply to a message originally sent by the bot.
shouldDispatch :: DispatchMode -> Maybe Text -> Message -> Bool
shouldDispatch AllMessages _ _ = True
shouldDispatch MentionOnly mBotUsername msg
  | msg.chatType == "private" = True
  | otherwise = case mBotUsername of
      Nothing -> False
      Just u  -> mentionsBot u msg

mentionsBot :: Text -> Message -> Bool
mentionsBot botUsername msg
  | msg.replyToFromUsername == Just botUsername = True
  | otherwise = case msg.text of
      Nothing -> False
      Just txt ->
        any (entityMentionsBot botUsername txt) msg.entities

-- | True when the entity is a `mention` or `bot_command` whose slice contains
--   `@<botUsername>` (case-insensitive).
entityMentionsBot :: Text -> Text -> MessageEntity -> Bool
entityMentionsBot botUsername fullText e
  | e.entityType /= "mention" && e.entityType /= "bot_command" = False
  | otherwise =
      let slice = T.take e.entityLength (T.drop e.entityOffset fullText)
          target = T.toLower ("@" <> botUsername)
      in target `T.isInfixOf` T.toLower slice
