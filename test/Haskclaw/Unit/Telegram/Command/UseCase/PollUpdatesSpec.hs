module Haskclaw.Unit.Telegram.Command.UseCase.PollUpdatesSpec (spec) where

import Relude

import Effectful (runEff)
import Test.Hspec (Spec, describe, it, shouldBe)

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..), Message (..), Update (..), UpdateId (..))
import Haskclaw.Telegram.Command.UseCase.PollUpdates (pollOnce)
import qualified Haskclaw.Unit.Telegram.Command.InMemoryTelegramApi as InMemoryTelegramApi

spec :: Spec
spec = describe "PollUpdates.pollOnce" $ do
  it "returns an empty list and the current offset when there are no updates" $ do
    ref <- newIORef []
    (msgs, nextOffset) <- runEff $ InMemoryTelegramApi.run ref $ pollOnce Nothing
    msgs `shouldBe` []
    nextOffset `shouldBe` Nothing

  it "returns updates that carry a message and computes the next offset" $ do
    let updates =
          [ Update
              { updateId = UpdateId 100
              , message = Just Message
                  { messageId = 1
                  , chatId = ChatId 42
                  , text = Just "hello"
                  , fromUsername = Just "testuser"
                  }
              }
          ]
    ref <- newIORef updates
    (msgs, nextOffset) <- runEff $ InMemoryTelegramApi.run ref $ pollOnce Nothing
    length msgs `shouldBe` 1
    (viaNonEmpty head msgs >>= (.text)) `shouldBe` Just "hello"
    nextOffset `shouldBe` Just (UpdateId 101)

  it "ignores updates that do not carry a message" $ do
    let updates =
          [ Update { updateId = UpdateId 200, message = Nothing }
          , Update
              { updateId = UpdateId 201
              , message = Just Message
                  { messageId = 2
                  , chatId = ChatId 42
                  , text = Just "world"
                  , fromUsername = Nothing
                  }
              }
          ]
    ref <- newIORef updates
    (msgs, nextOffset) <- runEff $ InMemoryTelegramApi.run ref $ pollOnce Nothing
    length msgs `shouldBe` 1
    (viaNonEmpty head msgs >>= (.text)) `shouldBe` Just "world"
    nextOffset `shouldBe` Just (UpdateId 202)

  it "returns only updates at or after the given offset" $ do
    let updates =
          [ Update
              { updateId = UpdateId 100
              , message = Just Message
                  { messageId = 1
                  , chatId = ChatId 42
                  , text = Just "old"
                  , fromUsername = Nothing
                  }
              }
          , Update
              { updateId = UpdateId 101
              , message = Just Message
                  { messageId = 2
                  , chatId = ChatId 42
                  , text = Just "new"
                  , fromUsername = Nothing
                  }
              }
          ]
    ref <- newIORef updates
    (msgs, nextOffset) <- runEff $ InMemoryTelegramApi.run ref $ pollOnce (Just (UpdateId 101))
    length msgs `shouldBe` 1
    (viaNonEmpty head msgs >>= (.text)) `shouldBe` Just "new"
    nextOffset `shouldBe` Just (UpdateId 102)
