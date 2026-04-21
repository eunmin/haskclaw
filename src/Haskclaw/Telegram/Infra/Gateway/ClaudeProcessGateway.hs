module Haskclaw.Telegram.Infra.Gateway.ClaudeProcessGateway
  ( callClaude
  , parseStreamLine
  , buildClaudeEnv
  , StreamEvent (..)
  , ContentBlock (..)
  ) where

import Relude

import Data.Aeson (FromJSON (..), Value (..), eitherDecodeStrict, encode, withObject, (.!=), (.:), (.:?))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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

import Haskclaw.Infra.Paths (chatIdSlug, chatMcpJsonPath, ensureChatConfig, ensureChatDir)
import Haskclaw.Telegram.Command.Domain.Types (ChatId, SessionId (..))
import Haskclaw.Util.ChatLog (logChat)

-- | Public entry point. Signature is stable across the streaming rewrite.
callClaude :: ChatId -> Maybe SessionId -> Text -> IO (Either Text (Text, SessionId))
callClaude cid mSessionId input = do
  workDir <- ensureChatDir cid
  _ <- ensureChatConfig cid
  let effectiveSid = case mSessionId of
        Just (SessionId "") -> Nothing
        other -> other
  firstAttempt <- runStreamingClaude cid workDir effectiveSid input
  case firstAttempt of
    Right resp -> pure (Right resp)
    Left err
      | isSessionMissing err, isJust effectiveSid -> do
          logChat cid $ "session not found, retrying fresh: " <> err
          runStreamingClaude cid workDir Nothing input
      | otherwise -> pure (Left err)

runStreamingClaude
  :: ChatId
  -> FilePath
  -> Maybe SessionId
  -> Text
  -> IO (Either Text (Text, SessionId))
runStreamingClaude cid workDir mSessionId input = do
  mcpJson <- chatMcpJsonPath cid
  baseEnv <- getEnvironment
  let args = ["-p", "--output-format", "stream-json", "--verbose"
             , "--mcp-config", mcpJson, "--strict-mcp-config"
             ]
           <> case mSessionId of
                Nothing -> []
                Just (SessionId sid) -> ["--resume", toString sid]
      process :: ProcessConfig () Handle Handle
      process =
        setStdout createPipe
          $ setStderr createPipe
          $ setWorkingDir workDir
          $ setEnv (buildClaudeEnv cid baseEnv)
          $ setStdin (byteStringInput (encodeUtf8 input))
          $ proc "claude" args
  (exit, mFinal, errText) <- withProcessWait process $ \p -> do
    resultRef <- newIORef Nothing
    readLoop cid (getStdout p) resultRef
    errBytes <- BS.hGetContents (getStderr p)
    code <- waitExitCode p
    final <- readIORef resultRef
    pure (code, final, decodeUtf8 errBytes :: Text)
  case (exit, mFinal) of
    (ExitSuccess, Just (txt, sid)) -> pure (Right (txt, sid))
    (ExitSuccess, Nothing) -> pure (Left "stream ended without a result event")
    (ExitFailure code, _) -> pure $ Left $
      "claude process failed (exit " <> show code <> "): " <> errText

-- | Read stdout line by line, decode each event, log to console, and capture the final result.
readLoop :: ChatId -> Handle -> IORef (Maybe (Text, SessionId)) -> IO ()
readLoop cid h ref = do
  eof <- SIO.hIsEOF h
  unless eof $ do
    line <- BS8.hGetLine h
    handleLine cid line ref
    readLoop cid h ref

handleLine :: ChatId -> ByteString -> IORef (Maybe (Text, SessionId)) -> IO ()
handleLine cid line ref = case parseStreamLine line of
  Left err ->
    logChat cid $ "stream parse error: " <> toText err
      <> " line=" <> truncText 200 (decodeUtf8 line)
  Right ev -> renderEvent cid ev ref

-- | Augment the inherited process environment with per-chat variables the
--   subprocess expects. Currently sets @AGENT_BROWSER_SESSION@ to the chat's
--   directory slug so agent-browser daemons remain isolated across chats.
--   Any existing entry for the same key is overwritten; all other entries are
--   preserved in their original order.
buildClaudeEnv :: ChatId -> [(String, String)] -> [(String, String)]
buildClaudeEnv cid base =
  let key = "AGENT_BROWSER_SESSION"
      value = chatIdSlug cid
      stripped = filter (\(k, _) -> k /= key) base
  in stripped <> [(key, value)]

-- ===== Stream event model =====

data StreamEvent
  = EvSystemInit SessionId (Maybe Text)     -- session_id, model
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

renderEvent :: ChatId -> StreamEvent -> IORef (Maybe (Text, SessionId)) -> IO ()
renderEvent cid ev ref = case ev of
  EvSystemInit sid mModel ->
    logChat cid $ "system.init model=" <> fromMaybe "?" mModel
      <> " session=" <> unSid sid
  EvAssistant blocks -> forM_ blocks (renderAssistantBlock cid)
  EvUser blocks -> forM_ blocks (renderUserBlock cid)
  EvResult txt mSid isErr -> do
    logChat cid $ "result is_error=" <> show isErr
      <> " text=" <> truncText 300 txt
    whenJust mSid $ \sid -> writeIORef ref (Just (txt, sid))
  EvOther ty -> logChat cid $ "event=" <> ty

renderAssistantBlock :: ChatId -> ContentBlock -> IO ()
renderAssistantBlock cid = \case
  CbText t -> logChat cid $ "assistant: " <> truncText 500 t
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
