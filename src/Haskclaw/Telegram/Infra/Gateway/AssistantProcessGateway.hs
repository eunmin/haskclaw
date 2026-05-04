module Haskclaw.Telegram.Infra.Gateway.AssistantProcessGateway
  ( callAssistant
  , parseStreamLine
  , buildAssistantEnv
  , buildClaudeArgs
  , buildCodexArgs
  , parseAssistantOptions
  , AssistantProvider (..)
  , AssistantOptions (..)
  , defaultAssistantOptions
  , StreamEvent (..)
  , ContentBlock (..)
  , compactBoundaryNotice
  ) where

import Relude

import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict, encode, withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (stripPrefix)
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import qualified System.IO as SIO
import System.Process.Typed
  ( ProcessConfig
  , byteStringInput
  , createPipe
  , getStderr
  , getStdout
  , proc
  , setEnv
  , setStderr
  , setStdin
  , setStdout
  , setWorkingDir
  , waitExitCode
  , withProcessWait
  )

import Haskclaw.Infra.Paths (chatIdSlug, chatMcpJsonPath, ensureChatConfig, ensureChatDir, mcpBinaryPath)
import Haskclaw.Telegram.Command.Domain.Types (AssistantProvider (..), ChatId (..), SessionId (..))
import Haskclaw.Util.ChatLog (logChat)

-- | CLI-flag knobs that control how the assistant subprocess is invoked.
data AssistantOptions = AssistantOptions
  { provider :: AssistantProvider
  , dangerouslySkipPermissions :: Bool
  } deriving stock (Show, Eq)

defaultAssistantOptions :: AssistantOptions
defaultAssistantOptions = AssistantOptions
  { provider = Claude
  , dangerouslySkipPermissions = False
  }

-- | Parse program args into AssistantOptions. Unknown args are ignored so this
--   composes with other parsers (e.g. parseDispatchMode).
parseAssistantOptions :: [String] -> AssistantOptions
parseAssistantOptions args = AssistantOptions
  { provider = parseProvider args
  , dangerouslySkipPermissions = "--dangerously-skip-permissions" `elem` args
  }

parseProvider :: [String] -> AssistantProvider
parseProvider = go
  where
    go [] = Claude
    go ("--assistant":v:rest) = parseProviderValue v (go rest)
    go ("--assistant-provider":v:rest) = parseProviderValue v (go rest)
    go (arg:rest)
      | Just v <- stripPrefix "--assistant=" arg = parseProviderValue v (go rest)
      | Just v <- stripPrefix "--assistant-provider=" arg = parseProviderValue v (go rest)
      | otherwise = go rest

parseProviderValue :: String -> AssistantProvider -> AssistantProvider
parseProviderValue v fallback = case T.toLower (toText v) of
  "claude" -> Claude
  "codex"  -> Codex
  _        -> fallback

-- | Build the argv passed to @claude@. Pure so it can be unit-tested without
--   invoking the subprocess.
buildClaudeArgs :: AssistantOptions -> FilePath -> Maybe SessionId -> [String]
buildClaudeArgs opts mcpJson mSessionId =
  base <> resume <> dangerous
  where
    base = [ "-p", "--output-format", "stream-json", "--verbose"
           , "--mcp-config", mcpJson, "--strict-mcp-config"
           ]
    resume = case mSessionId of
      Nothing -> []
      Just (SessionId sid) -> ["--resume", toString sid]
    dangerous =
      [ "--dangerously-skip-permissions" | opts.dangerouslySkipPermissions ]

buildCodexArgs :: AssistantOptions -> FilePath -> FilePath -> ChatId -> FilePath -> Maybe SessionId -> [String]
buildCodexArgs opts workDir mcpPath cid _mcpJson mSessionId =
  case mSessionId of
    Nothing ->
      ["exec"] <> common <> workspace <> mcp <> dangerous
    Just (SessionId sid) ->
      ["exec", "resume"] <> common <> mcp <> dangerous <> [toString sid, "-"]
  where
    common = ["--json", "--skip-git-repo-check"]
    workspace = ["-C", workDir]
    mcp =
      [ "-c", "mcp_servers.haskclaw.command=" <> show mcpPath
      , "-c", "mcp_servers.haskclaw.args=[]"
      , "-c", "mcp_servers.haskclaw.env.HASKCLAW_CHAT_ID=" <> show (chatIdText cid)
      ]
    dangerous =
      [ "--dangerously-bypass-approvals-and-sandbox" | opts.dangerouslySkipPermissions ]

chatIdText :: ChatId -> Text
chatIdText (ChatId cid) = show cid

-- | Public entry point. The @sink@ callback receives each assistant text
--   block as it streams in from the selected CLI. Callers wire it to their
--   transport (e.g. Telegram sendMessage) to get tool-use narration in
--   real time. On session-missing retry the sink is reused as-is.
callAssistant
  :: AssistantOptions
  -> (Text -> IO ())
  -> ChatId
  -> Maybe SessionId
  -> Text
  -> IO (Either Text SessionId)
callAssistant opts sink cid mSessionId input = do
  workDir <- ensureChatDir cid
  _ <- ensureChatConfig cid
  let effectiveSid = case mSessionId of
        Just (SessionId "") -> Nothing
        other -> other
  firstAttempt <- runStreamingAssistant opts sink cid workDir effectiveSid input
  case firstAttempt of
    Right resp -> pure (Right resp)
    Left err
      | isSessionMissing err, isJust effectiveSid -> do
          logChat cid $ "session not found, retrying fresh: " <> err
          runStreamingAssistant opts sink cid workDir Nothing input
      | otherwise -> pure (Left err)

runStreamingAssistant
  :: AssistantOptions
  -> (Text -> IO ())
  -> ChatId
  -> FilePath
  -> Maybe SessionId
  -> Text
  -> IO (Either Text SessionId)
runStreamingAssistant opts rawSink cid workDir mSessionId input = do
  mcpJson <- chatMcpJsonPath cid
  mcpPath <- mcpBinaryPath
  baseEnv <- getEnvironment
  sentRef <- newIORef False
  let sink t = do
        writeIORef sentRef True
        rawSink t
      (command, args) = case opts.provider of
        Claude -> ("claude", buildClaudeArgs opts mcpJson mSessionId)
        Codex  -> ("codex", buildCodexArgs opts workDir mcpPath cid mcpJson mSessionId)
      process :: ProcessConfig () Handle Handle
      process =
        setStdout createPipe
          $ setStderr createPipe
          $ setWorkingDir workDir
          $ setEnv (buildAssistantEnv cid baseEnv)
          $ setStdin (byteStringInput (encodeUtf8 input))
          $ proc command args
  (exit, mFinal, errText) <- withProcessWait process $ \p -> do
    resultRef <- newIORef Nothing
    readLoop sink cid (getStdout p) resultRef
    errBytes <- BS.hGetContents (getStderr p)
    code <- waitExitCode p
    final <- readIORef resultRef
    pure (code, final, decodeUtf8 errBytes :: Text)
  case (exit, mFinal) of
    (ExitSuccess, Just (txt, sid)) -> do
      sent <- readIORef sentRef
      unless (sent || T.null (T.strip txt)) $ rawSink txt
      pure (Right sid)
    (ExitSuccess, Nothing) -> pure (Left "stream ended without a result event")
    (ExitFailure code, _) -> pure $ Left $
      showProvider opts.provider <> " process failed (exit " <> show code <> "): " <> errText

-- | Read stdout line by line, decode each event, log to console, and capture the final result.
readLoop :: (Text -> IO ()) -> ChatId -> Handle -> IORef (Maybe (Text, SessionId)) -> IO ()
readLoop sink cid h ref = do
  eof <- SIO.hIsEOF h
  unless eof $ do
    line <- BS8.hGetLine h
    handleLine sink cid line ref
    readLoop sink cid h ref

handleLine
  :: (Text -> IO ())
  -> ChatId
  -> ByteString
  -> IORef (Maybe (Text, SessionId))
  -> IO ()
handleLine sink cid line ref = case parseStreamLine line of
  Left err ->
    logChat cid $ "stream parse error: " <> toText err
      <> " line=" <> truncText 200 (decodeUtf8 line)
  Right ev -> renderEvent sink cid ev ref

-- | Augment the inherited process environment with per-chat variables the
--   subprocess expects. Currently sets @AGENT_BROWSER_SESSION@ to the chat's
--   directory slug so agent-browser daemons remain isolated across chats.
--   Any existing entry for the same key is overwritten; all other entries are
--   preserved in their original order.
buildAssistantEnv :: ChatId -> [(String, String)] -> [(String, String)]
buildAssistantEnv cid base =
  let key = "AGENT_BROWSER_SESSION"
      value = chatIdSlug cid
      stripped = filter (\(k, _) -> k /= key) base
  in stripped <> [(key, value)]

-- ===== Stream event model =====

data StreamEvent
  = EvSystemInit SessionId (Maybe Text)     -- session_id, model
  | EvThreadStarted SessionId
  | EvSystemCompactBoundary
  | EvAssistant [ContentBlock]
  | EvUser [ContentBlock]
  | EvResult Text (Maybe SessionId) Bool    -- result text, session_id, is_error
  | EvOther Text
  deriving (Show, Eq)

data ContentBlock
  = CbText Text
  | CbToolUse Text Value   -- name, input
  | CbToolResult Text
  | CbOther Text
  deriving (Show, Eq)

-- | Parse a single NDJSON line into a StreamEvent (pure; unit-testable).
parseStreamLine :: ByteString -> Either String StreamEvent
parseStreamLine = eitherDecodeStrict

instance FromJSON StreamEvent where
  parseJSON = withObject "StreamEvent" $ \v -> do
    ty <- v .: "type" :: Parser Text
    case ty of
      "system" -> do
        sub <- v .:? "subtype" :: Parser (Maybe Text)
        case sub of
          Just "init" -> do
            sid <- v .: "session_id"
            model <- v .:? "model"
            pure (EvSystemInit (SessionId sid) model)
          Just "compact_boundary" -> pure EvSystemCompactBoundary
          _ -> pure (EvOther ("system/" <> fromMaybe "?" sub))
      "assistant" -> do
        msg <- v .: "message"
        content <- withObject "assistant.message" (.: "content") msg
        pure (EvAssistant content)
      "user" -> do
        msg <- v .: "message"
        content <- withObject "user.message" (.: "content") msg
        pure (EvUser content)
      "result" -> do
        txt <- v .:? "result" .!= ""
        sid <- v .:? "session_id"
        isErr <- v .:? "is_error" .!= False
        pure (EvResult txt (fmap SessionId sid) isErr)
      "thread.started" ->
        EvThreadStarted . SessionId <$> v .: "thread_id"
      "item.completed" -> do
        item <- v .: "item"
        itemType <- withObject "item.completed.item" (.: "type") item
        case (itemType :: Text) of
          "agent_message" -> do
            txt <- withObject "item.completed.item" (.: "text") item
            pure (EvAssistant [CbText txt])
          _ -> pure (EvOther ("item.completed/" <> itemType))
      "turn.completed" -> pure (EvOther "turn.completed")
      other -> pure (EvOther other)

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    ty <- v .: "type" :: Parser Text
    case ty of
      "text" -> CbText <$> v .: "text"
      "tool_use" -> CbToolUse <$> v .: "name" <*> v .: "input"
      "tool_result" -> do
        cval <- v .: "content"
        pure (CbToolResult (renderToolResult cval))
      other -> pure (CbOther other)

-- tool_result.content is either a plain string or an array of {type:"text", text:"..."} objects.
renderToolResult :: Value -> Text
renderToolResult (String s) = s
renderToolResult (Array xs) =
  let pieces = [ t | Object o <- toList xs, Just (String t) <- [KM.lookup "text" o] ]
  in T.intercalate "\n" pieces
renderToolResult v = decodeUtf8 (toStrict (encode v))

-- ===== Console rendering =====

renderEvent
  :: (Text -> IO ())
  -> ChatId
  -> StreamEvent
  -> IORef (Maybe (Text, SessionId))
  -> IO ()
renderEvent sink cid ev ref = case ev of
  EvSystemInit sid mModel ->
    logChat cid $ "system.init model=" <> fromMaybe "?" mModel
      <> " session=" <> unSid sid
  EvThreadStarted sid -> do
    logChat cid $ "thread.started session=" <> unSid sid
    writeIORef ref (Just ("", sid))
  EvSystemCompactBoundary -> do
    logChat cid "system.compact_boundary"
    sink compactBoundaryNotice
  EvAssistant blocks -> forM_ blocks (renderAssistantBlock sink cid ref)
  EvUser blocks -> forM_ blocks (renderUserBlock cid)
  EvResult txt mSid isErr -> do
    logChat cid $ "result is_error=" <> show isErr
      <> " text=" <> truncText 300 txt
    whenJust mSid $ \sid -> writeIORef ref (Just (txt, sid))
  EvOther ty -> logChat cid $ "event=" <> ty

compactBoundaryNotice :: Text
compactBoundaryNotice =
  "Summarising our earlier conversation to free up context — one moment, please."

renderAssistantBlock
  :: (Text -> IO ())
  -> ChatId
  -> IORef (Maybe (Text, SessionId))
  -> ContentBlock
  -> IO ()
renderAssistantBlock sink cid ref = \case
  CbText t -> do
    logChat cid $ "assistant: " <> truncText 500 t
    sink t
    current <- readIORef ref
    whenJust current $ \(_, sid) -> writeIORef ref (Just (t, sid))
  CbToolUse name input ->
    logChat cid $ "tool_use " <> name <> " "
      <> truncText 300 (decodeUtf8 (toStrict (encode input)))
  CbToolResult _ -> pure ()
  CbOther ty -> logChat cid $ "assistant.other=" <> ty

renderUserBlock :: ChatId -> ContentBlock -> IO ()
renderUserBlock cid = \case
  CbToolResult t -> logChat cid $ "tool_result " <> truncText 500 t
  CbText t -> logChat cid $ "user.text " <> truncText 200 t
  CbToolUse _ _ -> pure ()
  CbOther ty -> logChat cid $ "user.other=" <> ty

-- ===== Helpers =====

unSid :: SessionId -> Text
unSid (SessionId s) = s

truncText :: Int -> Text -> Text
truncText n t = if T.length t > n then T.take n t <> "…" else t

isSessionMissing :: Text -> Bool
isSessionMissing err = "No conversation found with session ID" `T.isInfixOf` err

showProvider :: AssistantProvider -> Text
showProvider = \case
  Claude -> "claude"
  Codex  -> "codex"
