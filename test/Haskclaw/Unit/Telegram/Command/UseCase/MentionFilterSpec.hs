module Haskclaw.Unit.Telegram.Command.UseCase.MentionFilterSpec (spec) where

import Relude

import Test.Hspec (Spec, describe, it, shouldBe)

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..), Message (..), MessageEntity (..))
import Haskclaw.Telegram.Command.UseCase.MentionFilter
  ( DispatchMode (..)
  , parseDispatchMode
  , shouldDispatch
  )

mkMsg :: Text -> Maybe Text -> [MessageEntity] -> Maybe Text -> Message
mkMsg ct txt es replyFrom = Message
  { messageId = 1
  , chatId = ChatId 1
  , chatType = ct
  , text = txt
  , fromUsername = Nothing
  , entities = es
  , replyToFromUsername = replyFrom
  }

mention :: Int -> Int -> MessageEntity
mention off len = MessageEntity { entityType = "mention", entityOffset = off, entityLength = len }

botCommand :: Int -> Int -> MessageEntity
botCommand off len = MessageEntity { entityType = "bot_command", entityOffset = off, entityLength = len }

spec :: Spec
spec = do
  describe "parseDispatchMode" $ do
    it "defaults to MentionOnly when no flag is passed" $
      parseDispatchMode [] `shouldBe` MentionOnly

    it "returns AllMessages for --all" $
      parseDispatchMode ["--all"] `shouldBe` AllMessages

    it "returns AllMessages for --all-messages" $
      parseDispatchMode ["--all-messages"] `shouldBe` AllMessages

    it "returns MentionOnly for unrelated args" $
      parseDispatchMode ["--foo"] `shouldBe` MentionOnly

  describe "shouldDispatch AllMessages" $ do
    it "passes any message regardless of mention" $ do
      let msg = mkMsg "group" (Just "hi") [] Nothing
      shouldDispatch AllMessages (Just "bot") msg `shouldBe` True

    it "passes when bot username is unknown" $ do
      let msg = mkMsg "group" (Just "hi") [] Nothing
      shouldDispatch AllMessages Nothing msg `shouldBe` True

  describe "shouldDispatch MentionOnly" $ do
    it "always passes private chats" $ do
      let msg = mkMsg "private" (Just "hi") [] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` True

    it "passes when chat is private even without bot username" $ do
      let msg = mkMsg "private" (Just "hi") [] Nothing
      shouldDispatch MentionOnly Nothing msg `shouldBe` True

    it "drops group messages without entities" $ do
      let msg = mkMsg "group" (Just "hello") [] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` False

    it "passes when text contains @<botUsername> with a mention entity" $ do
      let txt = "@bot please help"
          msg = mkMsg "supergroup" (Just txt) [mention 0 4] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` True

    it "drops when mention entity targets a different bot" $ do
      let txt = "@otherbot please help"
          msg = mkMsg "group" (Just txt) [mention 0 9] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` False

    it "passes for bot_command targeted at the bot" $ do
      let txt = "/start@bot"
          msg = mkMsg "group" (Just txt) [botCommand 0 10] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` True

    it "drops for bot_command targeted at a different bot" $ do
      let txt = "/start@other"
          msg = mkMsg "group" (Just txt) [botCommand 0 12] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` False

    it "drops for bot_command without a username suffix" $ do
      let txt = "/start"
          msg = mkMsg "group" (Just txt) [botCommand 0 6] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` False

    it "passes when message replies to one of the bot's own messages" $ do
      let msg = mkMsg "group" (Just "thanks") [] (Just "bot")
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` True

    it "is case-insensitive on the username match" $ do
      let txt = "@Bot ping"
          msg = mkMsg "group" (Just txt) [mention 0 4] Nothing
      shouldDispatch MentionOnly (Just "bot") msg `shouldBe` True

    it "drops group messages when bot username is unknown" $ do
      let txt = "@bot help"
          msg = mkMsg "group" (Just txt) [mention 0 4] Nothing
      shouldDispatch MentionOnly Nothing msg `shouldBe` False
