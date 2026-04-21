module Haskclaw.Unit.Telegram.Command.UseCase.ProcessMessageSpec (spec) where

import Relude

import Effectful (runEff)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..), Message (..), SessionId (..))
import Haskclaw.Telegram.Command.UseCase.ProcessMessage (processMessage)
import qualified Haskclaw.Unit.Telegram.Command.InMemoryClaudeService as InMemoryClaudeService

spec :: Spec
spec = describe "ProcessMessage.processMessage" $ do
  it "returns the session id produced by Claude for a text message" $ do
    let msg = Message
          { messageId = 1
          , chatId = ChatId 42
          , text = Just "hello"
          , fromUsername = Just "testuser"
          }
    result <- runEff $ InMemoryClaudeService.run $ processMessage Nothing msg
    result `shouldBe` Just (Just (SessionId "test-session-id"))

  it "streams assistant text through the provided sink" $ do
    bufRef <- newIORef ([] :: [(ChatId, Text)])
    let sink cid txt = modifyIORef' bufRef ((cid, txt) :)
        msg = Message
          { messageId = 1
          , chatId = ChatId 42
          , text = Just "hello"
          , fromUsername = Nothing
          }
    _ <- runEff $ InMemoryClaudeService.runWithSink sink $ processMessage Nothing msg
    streamed <- reverse <$> readIORef bufRef
    streamed `shouldBe` [(ChatId 42, "echo: hello")]

  it "returns Nothing for a message without text and skips the sink" $ do
    bufRef <- newIORef ([] :: [(ChatId, Text)])
    let sink cid txt = modifyIORef' bufRef ((cid, txt) :)
        msg = Message
          { messageId = 1
          , chatId = ChatId 42
          , text = Nothing
          , fromUsername = Nothing
          }
    result <- runEff $ InMemoryClaudeService.runWithSink sink $ processMessage Nothing msg
    result `shouldBe` Nothing
    readIORef bufRef `shouldReturn` ([] :: [(ChatId, Text)])

  it "forwards the given session id to Claude" $ do
    let msg = Message
          { messageId = 1
          , chatId = ChatId 42
          , text = Just "follow up"
          , fromUsername = Nothing
          }
    result <- runEff $ InMemoryClaudeService.run $ processMessage (Just (SessionId "prev-session")) msg
    result `shouldBe` Just (Just (SessionId "test-session-id"))
