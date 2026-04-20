module Haskclaw.Unit.Telegram.Infra.Gateway.ClaudeProcessGatewaySpec (spec) where

import Relude

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Haskclaw.Telegram.Command.Domain.Types (SessionId (..))
import Haskclaw.Telegram.Infra.Gateway.ClaudeProcessGateway
  ( ContentBlock (..)
  , StreamEvent (..)
  , parseStreamLine
  )

encodeStrict :: Aeson.Value -> ByteString
encodeStrict = LBS.toStrict . Aeson.encode

spec :: Spec
spec = describe "parseStreamLine" $ do
  it "extracts session_id and model from a system/init event" $ do
    let line = encodeStrict $ object
          [ "type" .= ("system" :: Text)
          , "subtype" .= ("init" :: Text)
          , "session_id" .= ("abc-123" :: Text)
          , "model" .= ("claude-opus-4-7" :: Text)
          ]
    parseStreamLine line
      `shouldBe` Right (EvSystemInit (SessionId "abc-123") (Just "claude-opus-4-7"))

  it "parses tool_use blocks inside assistant.message" $ do
    let line = encodeStrict $ object
          [ "type" .= ("assistant" :: Text)
          , "message" .= object
              [ "content" .= [ object
                  [ "type" .= ("tool_use" :: Text)
                  , "name" .= ("Bash" :: Text)
                  , "input" .= object [ "command" .= ("ls" :: Text) ]
                  ]
                ]
              ]
          ]
    case parseStreamLine line of
      Right (EvAssistant [CbToolUse name _input]) -> name `shouldBe` "Bash"
      other -> fail $ "unexpected: " <> show other

  it "parses text blocks inside assistant.message" $ do
    let line = encodeStrict $ object
          [ "type" .= ("assistant" :: Text)
          , "message" .= object
              [ "content" .= [ object
                  [ "type" .= ("text" :: Text), "text" .= ("hello" :: Text) ]
                ]
              ]
          ]
    parseStreamLine line
      `shouldBe` Right (EvAssistant [CbText "hello"])

  it "parses tool_result with a plain string content inside user.message" $ do
    let line = encodeStrict $ object
          [ "type" .= ("user" :: Text)
          , "message" .= object
              [ "content" .= [ object
                  [ "type" .= ("tool_result" :: Text)
                  , "content" .= ("output text" :: Text)
                  ]
                ]
              ]
          ]
    parseStreamLine line
      `shouldBe` Right (EvUser [CbToolResult "output text"])

  it "joins tool_result content arrays of {type:text,text:...} into a single string" $ do
    let line = encodeStrict $ object
          [ "type" .= ("user" :: Text)
          , "message" .= object
              [ "content" .= [ object
                  [ "type" .= ("tool_result" :: Text)
                  , "content" .=
                      [ object [ "type" .= ("text" :: Text), "text" .= ("line1" :: Text) ]
                      , object [ "type" .= ("text" :: Text), "text" .= ("line2" :: Text) ]
                      ]
                  ]
                ]
              ]
          ]
    parseStreamLine line
      `shouldBe` Right (EvUser [CbToolResult "line1\nline2"])

  it "extracts the final text and session_id from a result event" $ do
    let line = encodeStrict $ object
          [ "type" .= ("result" :: Text)
          , "result" .= ("final answer" :: Text)
          , "session_id" .= ("xyz-789" :: Text)
          , "is_error" .= False
          ]
    parseStreamLine line
      `shouldBe` Right (EvResult "final answer" (Just (SessionId "xyz-789")) False)

  it "falls back to EvOther for unknown event types" $ do
    let line = encodeStrict $ object [ "type" .= ("mystery" :: Text) ]
    parseStreamLine line `shouldBe` Right (EvOther "mystery")

  it "fails with Left for non-JSON input" $
    parseStreamLine "not json" `shouldSatisfy` isLeft
