module Haskclaw.Unit.Telegram.Infra.Gateway.AssistantProcessGatewaySpec (spec) where

import Relude

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..), SessionId (..))
import Haskclaw.Telegram.Infra.Gateway.AssistantProcessGateway
  ( AssistantProvider (..)
  , AssistantOptions (..)
  , ContentBlock (..)
  , StreamEvent (..)
  , buildAssistantEnv
  , buildClaudeArgs
  , buildCodexArgs
  , compactBoundaryNotice
  , defaultAssistantOptions
  , formatProcessFailure
  , parseAssistantOptions
  , parseStreamLine
  )

encodeStrict :: Aeson.Value -> ByteString
encodeStrict = LBS.toStrict . Aeson.encode

spec :: Spec
spec = do
  describe "compactBoundaryNotice" $ do
    it "is a non-empty user-facing string" $
      compactBoundaryNotice `shouldSatisfy` (not . T.null)

    it "mentions that earlier conversation is being summarised/compacted" $
      compactBoundaryNotice `shouldSatisfy` \t ->
        let lower = T.toLower t
         in "summaris" `T.isInfixOf` lower || "compact" `T.isInfixOf` lower

  describe "parseAssistantOptions" $ do
    it "defaults provider to Claude" $
      (parseAssistantOptions []).provider `shouldBe` Claude

    it "sets provider to Codex with --assistant codex" $
      (parseAssistantOptions ["--assistant", "codex"]).provider `shouldBe` Codex

    it "sets provider to Codex with --assistant=codex" $
      (parseAssistantOptions ["--assistant=codex"]).provider `shouldBe` Codex

    it "supports --assistant-provider as an alias" $
      (parseAssistantOptions ["--assistant-provider", "codex"]).provider `shouldBe` Codex

    it "keeps the default provider for unknown provider values" $
      parseAssistantOptions ["--assistant", "unknown"] `shouldBe` defaultAssistantOptions

    it "defaults dangerouslySkipPermissions to False" $
      (parseAssistantOptions []).dangerouslySkipPermissions `shouldBe` False

    it "sets dangerouslySkipPermissions when --dangerously-skip-permissions is passed" $
      (parseAssistantOptions ["--dangerously-skip-permissions"]).dangerouslySkipPermissions `shouldBe` True

    it "ignores unrelated flags" $
      parseAssistantOptions ["--all", "--whatever"] `shouldBe` defaultAssistantOptions

  describe "buildClaudeArgs" $ do
    it "produces the base argv with default options and no session" $
      buildClaudeArgs defaultAssistantOptions "/tmp/mcp.json" Nothing
        `shouldBe`
          [ "-p", "--output-format", "stream-json", "--verbose"
          , "--mcp-config", "/tmp/mcp.json", "--strict-mcp-config"
          ]

    it "appends --resume <sid> when a session id is given" $
      buildClaudeArgs defaultAssistantOptions "/tmp/mcp.json" (Just (SessionId "abc"))
        `shouldBe`
          [ "-p", "--output-format", "stream-json", "--verbose"
          , "--mcp-config", "/tmp/mcp.json", "--strict-mcp-config"
          , "--resume", "abc"
          ]

    it "appends --dangerously-skip-permissions when the option is enabled" $ do
      let opts = defaultAssistantOptions { dangerouslySkipPermissions = True }
      buildClaudeArgs opts "/tmp/mcp.json" Nothing
        `shouldBe`
          [ "-p", "--output-format", "stream-json", "--verbose"
          , "--mcp-config", "/tmp/mcp.json", "--strict-mcp-config"
          , "--dangerously-skip-permissions"
          ]

    it "places --dangerously-skip-permissions after --resume" $ do
      let opts = defaultAssistantOptions { dangerouslySkipPermissions = True }
      buildClaudeArgs opts "/tmp/mcp.json" (Just (SessionId "xyz"))
        `shouldBe`
          [ "-p", "--output-format", "stream-json", "--verbose"
          , "--mcp-config", "/tmp/mcp.json", "--strict-mcp-config"
          , "--resume", "xyz"
          , "--dangerously-skip-permissions"
          ]

  describe "buildCodexArgs" $ do
    it "produces codex exec argv with default options and no session" $
      buildCodexArgs defaultAssistantOptions "/tmp/work" "/tmp/haskclaw-mcp" (ChatId 42) "/tmp/mcp.json" Nothing
        `shouldBe`
          [ "exec", "--json", "--skip-git-repo-check"
          , "-C", "/tmp/work"
          , "-c", "mcp_servers.haskclaw.command=\"/tmp/haskclaw-mcp\""
          , "-c", "mcp_servers.haskclaw.args=[]"
          , "-c", "mcp_servers.haskclaw.env.HASKCLAW_CHAT_ID=\"42\""
          ]

    it "produces codex resume argv when a session id is given" $
      buildCodexArgs defaultAssistantOptions "/tmp/work" "/tmp/haskclaw-mcp" (ChatId 42) "/tmp/mcp.json" (Just (SessionId "abc"))
        `shouldBe`
          [ "exec", "resume", "--json", "--skip-git-repo-check"
          , "-c", "mcp_servers.haskclaw.command=\"/tmp/haskclaw-mcp\""
          , "-c", "mcp_servers.haskclaw.args=[]"
          , "-c", "mcp_servers.haskclaw.env.HASKCLAW_CHAT_ID=\"42\""
          , "abc", "-"
          ]

    it "maps dangerouslySkipPermissions to the Codex bypass flag" $ do
      let opts = defaultAssistantOptions { provider = Codex, dangerouslySkipPermissions = True }
      buildCodexArgs opts "/tmp/work" "/tmp/haskclaw-mcp" (ChatId (-7)) "/tmp/mcp.json" Nothing
        `shouldBe`
          [ "exec", "--json", "--skip-git-repo-check"
          , "-C", "/tmp/work"
          , "-c", "mcp_servers.haskclaw.command=\"/tmp/haskclaw-mcp\""
          , "-c", "mcp_servers.haskclaw.args=[]"
          , "-c", "mcp_servers.haskclaw.env.HASKCLAW_CHAT_ID=\"-7\""
          , "--dangerously-bypass-approvals-and-sandbox"
          ]

  describe "buildAssistantEnv" $ do
    it "appends AGENT_BROWSER_SESSION with the chat slug to an empty base" $
      buildAssistantEnv (ChatId 42) []
        `shouldBe` [("AGENT_BROWSER_SESSION", "42")]

    it "prefixes negative chat ids with neg_ to match the chat directory" $
      buildAssistantEnv (ChatId (-7)) []
        `shouldBe` [("AGENT_BROWSER_SESSION", "neg_7")]

    it "preserves unrelated entries in their original order" $
      buildAssistantEnv (ChatId 1) [("HOME", "/h"), ("PATH", "/bin")]
        `shouldBe`
          [ ("HOME", "/h")
          , ("PATH", "/bin")
          , ("AGENT_BROWSER_SESSION", "1")
          ]

    it "overrides an existing AGENT_BROWSER_SESSION instead of duplicating it" $ do
      let result = buildAssistantEnv (ChatId 99)
            [ ("HOME", "/h")
            , ("AGENT_BROWSER_SESSION", "stale")
            , ("PATH", "/bin")
            ]
      result `shouldBe`
        [ ("HOME", "/h")
        , ("PATH", "/bin")
        , ("AGENT_BROWSER_SESSION", "99")
        ]

  describe "formatProcessFailure" $ do
    it "uses stderr when stderr is present" $
      formatProcessFailure Codex 1 "stderr details" ["stdout details"]
        `shouldBe` "codex process failed (exit 1): stderr details"

    it "falls back to stdout parse failures when stderr is empty" $
      formatProcessFailure Codex 1 "" ["ERROR: You've hit your usage limit."]
        `shouldBe` "codex process failed (exit 1): ERROR: You've hit your usage limit."

  describe "parseStreamLine" $ do
    it "extracts session_id and model from a system/init event" $ do
      let line = encodeStrict $ object
            [ "type" .= ("system" :: Text)
            , "subtype" .= ("init" :: Text)
            , "session_id" .= ("abc-123" :: Text)
            , "model" .= ("claude-opus-4-7" :: Text)
            ]
      parseStreamLine line
        `shouldBe` Right (EvSystemInit (SessionId "abc-123") (Just "claude-opus-4-7"))

    it "parses system/compact_boundary events" $ do
      let line = encodeStrict $ object
            [ "type" .= ("system" :: Text)
            , "subtype" .= ("compact_boundary" :: Text)
            ]
      parseStreamLine line `shouldBe` Right EvSystemCompactBoundary

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

    it "extracts the session id from a Codex thread.started event" $ do
      let line = encodeStrict $ object
            [ "type" .= ("thread.started" :: Text)
            , "thread_id" .= ("019ded83-3c1d-7b42-b8a9-56ce41d95efe" :: Text)
            ]
      parseStreamLine line
        `shouldBe` Right (EvThreadStarted (SessionId "019ded83-3c1d-7b42-b8a9-56ce41d95efe"))

    it "extracts agent text from a Codex item.completed event" $ do
      let line = encodeStrict $ object
            [ "type" .= ("item.completed" :: Text)
            , "item" .= object
                [ "id" .= ("item_0" :: Text)
                , "type" .= ("agent_message" :: Text)
                , "text" .= ("hello from codex" :: Text)
                ]
            ]
      parseStreamLine line
        `shouldBe` Right (EvAssistant [CbText "hello from codex"])

    it "falls back to EvOther for unknown event types" $ do
      let line = encodeStrict $ object [ "type" .= ("mystery" :: Text) ]
      parseStreamLine line `shouldBe` Right (EvOther "mystery")

    it "fails with Left for non-JSON input" $
      parseStreamLine "not json" `shouldSatisfy` isLeft
