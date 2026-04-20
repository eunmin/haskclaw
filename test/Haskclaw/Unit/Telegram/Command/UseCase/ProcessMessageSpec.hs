module Haskclaw.Unit.Telegram.Command.UseCase.ProcessMessageSpec (spec) where

import Relude

import Effectful (runEff)
import Test.Hspec (Spec, describe, it, shouldBe)

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..), Message (..), SessionId (..))
import Haskclaw.Telegram.Command.UseCase.ProcessMessage (processMessage)
import qualified Haskclaw.Unit.Telegram.Command.InMemoryClaudeService as InMemoryClaudeService

spec :: Spec
spec = describe "ProcessMessage.processMessage" $ do
  it "returns Claude's response and session id for a text message" $ do
    let msg = Message
          { messageId = 1
          , chatId = ChatId 42
          , text = Just "hello"
          , fromUsername = Just "testuser"
          }
    result <- runEff $ InMemoryClaudeService.run $ processMessage Nothing msg
    result `shouldBe` Just ("echo: hello", Just (SessionId "test-session-id"))

  it "returns Nothing for a message without text" $ do
    let msg = Message
          { messageId = 1
          , chatId = ChatId 42
          , text = Nothing
          , fromUsername = Nothing
          }
    result <- runEff $ InMemoryClaudeService.run $ processMessage Nothing msg
    result `shouldBe` Nothing

  it "forwards the given session id to Claude" $ do
    let msg = Message
          { messageId = 1
          , chatId = ChatId 42
          , text = Just "follow up"
          , fromUsername = Nothing
          }
    result <- runEff $ InMemoryClaudeService.run $ processMessage (Just (SessionId "prev-session")) msg
    result `shouldBe` Just ("echo: follow up", Just (SessionId "test-session-id"))
