module Haskclaw.Unit.Infra.PathsSpec (spec) where

import Relude

import Control.Exception (finally)
import Data.Aeson (Value, eitherDecode, encode, object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Environment (setEnv, unsetEnv)
import qualified System.Environment as Env
import System.FilePath ((</>))
import Test.Hspec (Spec, around_, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Haskclaw.Infra.Paths
  ( buildMcpJson
  , chatIdSlug
  , chatMcpJsonPath
  , chatSettingsPath
  , defaultSharedClaudeMd
  , ensureChatConfig
  , ensureChatDir
  , ensureHaskclawDirs
  , haskclawHome
  , managedBeginMarker
  , managedEndMarker
  , mergeHaskclawSettings
  , sharedClaudeMdPath
  , upsertManagedBlock
  )
import Haskclaw.Telegram.Command.Domain.Types (ChatId (..))

withTempHome :: IO () -> IO ()
withTempHome io = do
  tmp <- getTemporaryDirectory
  let fakeHome = tmp </> "haskclaw-paths-spec"
  removePathForcibly fakeHome
  original <- Env.lookupEnv "HOME"
  setEnv "HOME" fakeHome
  io `finally` do
    removePathForcibly fakeHome
    case original of
      Just v  -> setEnv "HOME" v
      Nothing -> unsetEnv "HOME"

renderBytes :: FilePath -> IO Text
renderBytes path = decodeUtf8 @Text . LBS.toStrict <$> LBS.readFile path

spec :: Spec
spec = do
  describe "chatIdSlug" $ do
    it "renders a positive chat_id as its decimal digits" $
      chatIdSlug (ChatId 42) `shouldBe` "42"

    it "prefixes negative chat_ids with neg_ and uses the absolute value" $
      chatIdSlug (ChatId (-1001234567890)) `shouldBe` "neg_1001234567890"

    it "renders 0 as \"0\"" $
      chatIdSlug (ChatId 0) `shouldBe` "0"

  describe "buildMcpJson" $ do
    it "renders mcpServers.haskclaw with the current binary and chat id" $ do
      let s = render (buildMcpJson (ChatId 42) "/bin/haskclaw-mcp")
      all (`T.isInfixOf` s)
        ["mcpServers", "haskclaw", "/bin/haskclaw-mcp", "HASKCLAW_CHAT_ID", "42"]
        `shouldBe` True

  describe "mergeHaskclawSettings" $ do
    it "creates the mcp__haskclaw permission on an empty object" $ do
      let s = render (mergeHaskclawSettings (object []))
      "mcp__haskclaw" `T.isInfixOf` s `shouldBe` True

    it "preserves other top-level keys and existing permissions" $ do
      let Right input = eitherDecode
            "{\"permissions\":{\"allow\":[\"WebSearch\"]},\"custom\":true}"
          s = render (mergeHaskclawSettings input)
      all (`T.isInfixOf` s) ["WebSearch", "mcp__haskclaw", "custom"] `shouldBe` True

    it "does not duplicate mcp__haskclaw when already present" $ do
      let Right input = eitherDecode
            "{\"permissions\":{\"allow\":[\"mcp__haskclaw\"]}}"
          s = render (mergeHaskclawSettings input)
      T.count "mcp__haskclaw" s `shouldBe` 1

    it "adds the Bash(agent-browser:*) permission alongside mcp__haskclaw" $ do
      let s = render (mergeHaskclawSettings (object []))
      all (`T.isInfixOf` s) ["mcp__haskclaw", "Bash(agent-browser:*)"] `shouldBe` True

    it "does not duplicate Bash(agent-browser:*) when already present" $ do
      let Right input = eitherDecode
            "{\"permissions\":{\"allow\":[\"Bash(agent-browser:*)\"]}}"
          s = render (mergeHaskclawSettings input)
      T.count "Bash(agent-browser:*)" s `shouldBe` 1

  describe "upsertManagedBlock" $ do
    let block = "managed body\nsecond line"

    it "wraps the block with markers and writes to an empty doc" $ do
      let out = upsertManagedBlock block ""
      managedBeginMarker `T.isInfixOf` out `shouldBe` True
      managedEndMarker `T.isInfixOf` out `shouldBe` True
      "managed body" `T.isInfixOf` out `shouldBe` True

    it "replaces the block when markers already exist" $ do
      let doc = "head\n\n" <> managedBeginMarker <> "\nold body\n" <> managedEndMarker <> "\n\ntail\n"
          out = upsertManagedBlock block doc
      "old body" `T.isInfixOf` out `shouldBe` False
      "managed body" `T.isInfixOf` out `shouldBe` True
      "head" `T.isInfixOf` out `shouldBe` True
      "tail" `T.isInfixOf` out `shouldBe` True

    it "appends the block when markers are absent" $ do
      let out = upsertManagedBlock block "existing rules\n"
      "existing rules" `T.isInfixOf` out `shouldBe` True
      managedBeginMarker `T.isInfixOf` out `shouldBe` True

    it "is idempotent when applied twice" $ do
      let once  = upsertManagedBlock block ""
          twice = upsertManagedBlock block once
      twice `shouldBe` once

  around_ withTempHome $ describe "ensureHaskclawDirs" $ do
    it "creates ~/.haskclaw and ~/.haskclaw/chats" $ do
      ensureHaskclawDirs
      home <- haskclawHome
      doesDirectoryExist home `shouldReturn` True
      doesDirectoryExist (home </> "chats") `shouldReturn` True

    it "writes the shared CLAUDE.md with scheduler instructions on first run" $ do
      ensureHaskclawDirs
      sharedMd <- sharedClaudeMdPath
      doesFileExist sharedMd `shouldReturn` True
      s <- TIO.readFile sharedMd
      all (`T.isInfixOf` s) ["mcp__haskclaw__schedule_task", "cron", managedBeginMarker]
        `shouldBe` True

    it "leaves pre-existing user content untouched outside the managed block" $ do
      ensureHaskclawDirs
      sharedMd <- sharedClaudeMdPath
      TIO.writeFile sharedMd "# my rules\nbe helpful\n"
      ensureHaskclawDirs
      s <- TIO.readFile sharedMd
      "be helpful" `T.isInfixOf` s `shouldBe` True
      "mcp__haskclaw__schedule_task" `T.isInfixOf` s `shouldBe` True

  around_ withTempHome $ describe "ensureChatDir" $ do
    it "creates the chat directory for a positive chat_id" $ do
      ensureHaskclawDirs
      path <- ensureChatDir (ChatId 42)
      home <- haskclawHome
      path `shouldBe` home </> "chats" </> "42"
      doesDirectoryExist path `shouldReturn` True

  around_ withTempHome $ describe "ensureChatConfig" $ do
    it "writes .mcp.json with the haskclaw server at the chat project root" $ do
      ensureHaskclawDirs
      _ <- ensureChatConfig (ChatId 42)
      mcpJson <- chatMcpJsonPath (ChatId 42)
      doesFileExist mcpJson `shouldReturn` True
      s <- renderBytes mcpJson
      all (`T.isInfixOf` s)
        ["mcpServers", "haskclaw", "haskclaw-mcp", "HASKCLAW_CHAT_ID", "42"]
        `shouldBe` True

    it "seeds settings.json with the mcp__haskclaw permission" $ do
      ensureHaskclawDirs
      _ <- ensureChatConfig (ChatId 42)
      settingsPath <- chatSettingsPath (ChatId 42)
      doesFileExist settingsPath `shouldReturn` True
      s <- renderBytes settingsPath
      "mcp__haskclaw" `T.isInfixOf` s `shouldBe` True

    it "injects the permission into a pre-existing settings.json without losing other keys" $ do
      ensureHaskclawDirs
      _ <- ensureChatConfig (ChatId 42)
      settingsPath <- chatSettingsPath (ChatId 42)
      LBS.writeFile settingsPath "{\"permissions\":{\"allow\":[\"WebSearch\"]},\"custom\":123}"
      _ <- ensureChatConfig (ChatId 42)
      s <- renderBytes settingsPath
      all (`T.isInfixOf` s) ["WebSearch", "mcp__haskclaw", "custom"] `shouldBe` True

    it "keeps the defaultSharedClaudeMd string contentful" $
      defaultSharedClaudeMd
        `shouldSatisfy` \t -> "mcp__haskclaw__schedule_task" `T.isInfixOf` t
  where
    render :: Value -> Text
    render = decodeUtf8 @Text . LBS.toStrict . encode
