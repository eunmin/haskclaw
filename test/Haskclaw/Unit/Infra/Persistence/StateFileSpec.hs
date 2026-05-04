module Haskclaw.Unit.Infra.Persistence.StateFileSpec (spec) where

import Relude

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Test.Hspec (Spec, describe, it, shouldBe)

import Haskclaw.Infra.Persistence.StateFile (SessionStore, migrateLegacySessions)
import Haskclaw.Telegram.Command.Domain.Types
  ( AssistantProvider (..)
  , ChatId (..)
  , SessionId (..)
  )

spec :: Spec
spec = describe "StateFile session storage" $ do
  it "round-trips sessions per assistant provider" $ do
    let sessions =
          Map.fromList
            [ ( ChatId 1
              , Map.fromList
                  [ (Claude, SessionId "claude-session")
                  , (Codex, SessionId "codex-thread")
                  ]
              )
            ] :: SessionStore
        encoded = Aeson.encode sessions
    Aeson.eitherDecode encoded `shouldBe` Right sessions

  it "migrates legacy chat sessions as Claude sessions only" $ do
    let legacy =
          Map.fromList
            [ (ChatId 1, SessionId "claude-session")
            , (ChatId 2, SessionId "other-claude-session")
            ]
    migrateLegacySessions legacy
      `shouldBe`
        Map.fromList
          [ (ChatId 1, Map.singleton Claude (SessionId "claude-session"))
          , (ChatId 2, Map.singleton Claude (SessionId "other-claude-session"))
          ]

  it "does not decode legacy shape as the provider-aware shape" $ do
    let legacyJson = LBS.fromStrict "{\"1\":\"old-session\"}"
    (Aeson.eitherDecode legacyJson :: Either String SessionStore)
      `shouldSatisfyLeft` const True

shouldSatisfyLeft :: Show b => Either a b -> (a -> Bool) -> IO ()
shouldSatisfyLeft value predicate = case value of
  Left err -> predicate err `shouldBe` True
  Right decoded -> fail $ "expected Left, got Right: " <> show decoded
