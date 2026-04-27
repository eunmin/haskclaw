module Haskclaw.ServerApp
  ( main
  ) where

import Relude hiding (Reader, runReader)

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM (TChan, readTChan)
import Control.Exception (IOException, try)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Effectful (Eff, IOE, runEff)
import Effectful.Reader.Dynamic (Reader, runReader)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesFileExist)
import System.Environment (getEnv)

import Haskclaw.Cli.Help (helpText, isHelpRequested)
import Haskclaw.Infra.Paths (ensureChatDir, ensureHaskclawDirs)
import Haskclaw.Infra.Persistence.StateFile (loadSessions, saveSessions)
import Haskclaw.Scheduler.Loop (clearInFlight, runLoop)
import Haskclaw.Telegram.Command.Domain.ClaudeService (askClaude)
import Haskclaw.Telegram.Command.Domain.TelegramApi (TelegramApi, getMe)
import Haskclaw.Telegram.Command.UseCase.MentionFilter
  ( DispatchMode (..)
  , parseDispatchMode
  , shouldDispatch
  )
import Haskclaw.Telegram.Infra.Gateway.ClaudeProcessGateway
  ( ClaudeOptions
  , parseClaudeOptions
  )
import Haskclaw.Telegram.Command.Domain.Types
  ( BotState (..)
  , ChatId
  , ChatTask (..)
  , Message (..)
  , newBotStateWith
  )
import Haskclaw.Telegram.Command.UseCase.DispatchMessage
  ( coalesceUserMsgs
  , dispatch
  , mergeMessages
  , runInterruptible
  )
import Haskclaw.Telegram.Command.UseCase.PollUpdates (pollOnce)
import Haskclaw.Telegram.Command.UseCase.ProcessMessage (processMessage)
import Haskclaw.Util.ChatLog (logChat)
import Haskclaw.Util.MarkdownImage (InlineImage (..), extractImages)
import Haskclaw.Util.Periodic (withPeriodic)
import qualified Haskclaw.Telegram.Infra.Gateway.TelegramHttpGateway as TelegramGateway
import qualified Haskclaw.Telegram.Infra.Interpreter.ClaudeServiceInterpreter as ClaudeServiceInterpreter
import Haskclaw.Telegram.Infra.Interpreter.TelegramApiInterpreter (TelegramConfig (..))
import qualified Haskclaw.Telegram.Infra.Interpreter.TelegramApiInterpreter as TelegramApiInterpreter

type App = Eff '[TelegramApi, Reader TelegramConfig, IOE]

runApp :: TelegramConfig -> App a -> IO a
runApp cfg =
  runEff
    . runReader cfg
    . TelegramApiInterpreter.run

main :: IO ()
main = do
  args <- getArgs
  if isHelpRequested args
    then putText helpText
    else runBot args

runBot :: [String] -> IO ()
runBot args = do
  let mode = parseDispatchMode args
      claudeOpts = parseClaudeOptions args
  token <- getEnv "TELEGRAM_BOT_TOKEN" <&> toText
  manager <- newManager tlsManagerSettings
  let cfg = TelegramConfig{token = token, manager = manager}
  ensureHaskclawDirs
  sessions <- loadSessions
  botState <- newBotStateWith sessions
  botUsername <- runApp cfg getMe
  putTextLn $ "haskclaw bot started. mode=" <> show mode
    <> " botUsername=" <> fromMaybe "<unknown>" botUsername
    <> " claudeOpts=" <> show claudeOpts
  putTextLn "Polling for messages..."
  let spawn = chatWorker claudeOpts botState manager token
  void $ forkIO $ runLoop botState spawn
  runApp cfg $ pollLoop claudeOpts botState cfg mode botUsername

pollLoop :: ClaudeOptions -> BotState -> TelegramConfig -> DispatchMode -> Maybe Text -> App ()
pollLoop claudeOpts botState cfg mode botUsername = do
  offset <- liftIO $ readTVarIO botState.offset
  (messages, nextOffset) <- pollOnce offset
  liftIO $ do
    forM_ nextOffset $ \o -> atomically $ writeTVar botState.offset (Just o)
    let allowed = filter (shouldDispatch mode botUsername) messages
    forM_ allowed $
      dispatch botState (chatWorker claudeOpts botState cfg.manager cfg.token)
  pollLoop claudeOpts botState cfg mode botUsername

typingIntervalMicros :: Int
typingIntervalMicros = 4000000

data TurnOutcome = TurnCompleted | TurnInterrupted

chatWorker :: ClaudeOptions -> BotState -> Manager -> Text -> ChatId -> TChan ChatTask -> IO ()
chatWorker claudeOpts botState manager token cid chan = void $ forkIO $ do
  tid <- myThreadId
  workDir <- ensureChatDir cid
  logChat cid $ "worker started thread=" <> show tid <> " cwd=" <> toText workDir
  workerLoop claudeOpts botState manager token cid chan []

workerLoop
  :: ClaudeOptions -> BotState -> Manager -> Text -> ChatId -> TChan ChatTask
  -> [Message] -> IO ()
workerLoop claudeOpts botState manager token cid chan carryover = do
  task <- atomically $ readTChan chan
  case task of
    UserMsg m -> do
      extras <- atomically $ coalesceUserMsgs chan
      let (firstMsg, restMsgs) = case carryover of
            []     -> (m, extras)
            (c:cs) -> (c, cs ++ m : extras)
          allMsgs = firstMsg : restMsgs
          merged  = mergeMessages firstMsg restMsgs
      outcome <- handleUserTurn claudeOpts botState manager token cid chan merged
      case outcome of
        TurnCompleted   -> workerLoop claudeOpts botState manager token cid chan []
        TurnInterrupted -> workerLoop claudeOpts botState manager token cid chan allMsgs
    ScheduledRun taskId' mLbl promptTxt -> do
      handleScheduled claudeOpts botState manager token cid taskId' mLbl promptTxt
      workerLoop claudeOpts botState manager token cid chan carryover

handleUserTurn
  :: ClaudeOptions -> BotState -> Manager -> Text -> ChatId -> TChan ChatTask -> Message
  -> IO TurnOutcome
handleUserTurn claudeOpts botState manager token cid chan msg = do
  let user = fromMaybe "unknown" msg.fromUsername
      content = fromMaybe "" msg.text
  logChat cid $ "received @" <> user <> ": " <> content
  mSessionId <- Map.lookup cid <$> readTVarIO botState.sessions
  let sink = deliverReply manager token
  outcome <- runInterruptible chan $
    withPeriodic typingIntervalMicros
      (TelegramGateway.postSendChatAction manager token cid "typing")
      (runEff $ ClaudeServiceInterpreter.run claudeOpts sink $ processMessage mSessionId msg)
  case outcome of
    Left () -> do
      logChat cid "interrupted by new user message; restarting turn"
      pure TurnInterrupted
    Right Nothing -> pure TurnCompleted
    Right (Just mNewSessionId) -> do
      whenJust mNewSessionId $ \newSessionId -> do
        newSessions <- atomically $ do
          modifyTVar' botState.sessions (Map.insert cid newSessionId)
          readTVar botState.sessions
        saveSessions newSessions
      pure TurnCompleted

-- | Run a scheduled prompt in a fresh session (no --resume). Always clears
--   the in-flight marker afterwards. Failures are delivered to the chat.
--   Unlike user turns, scheduled runs buffer the streamed text and emit a
--   single message at the end so the @[label]@ prefix stays attached to
--   the delivered content.
handleScheduled
  :: ClaudeOptions -> BotState -> Manager -> Text
  -> ChatId -> Text -> Maybe Text -> Text
  -> IO ()
handleScheduled claudeOpts botState manager token cid tid mLabel promptTxt = do
  let labelTag = maybe "" (\l -> "[" <> l <> "] ") mLabel
  logChat cid $ "scheduled run " <> tid <> " " <> labelTag <> "start"
  bufRef <- newIORef ([] :: [Text])
  let sink _ piece = modifyIORef' bufRef (piece :)
  mSid <- withPeriodic typingIntervalMicros
    (TelegramGateway.postSendChatAction manager token cid "typing")
    (runEff $ ClaudeServiceInterpreter.run claudeOpts sink $ askClaude cid Nothing promptTxt)
  body <- T.intercalate "\n\n" . reverse <$> readIORef bufRef
  case mSid of
    Just _ -> do
      logChat cid $ "scheduled run " <> tid <> " ok"
      deliverReply manager token cid (labelTag <> body)
    Nothing -> do
      logChat cid $ "scheduled run " <> tid <> " failed"
      TelegramGateway.postSendMessage manager token cid
        ("[scheduled task failed] " <> labelTag <> body)
  clearInFlight botState cid tid

-- | Split a Claude reply into inline images and residual text. Images are sent
--   via sendPhoto (skipped if the file is missing); leftover text is sent via
--   sendMessage. Either part may be empty — in that case the corresponding
--   call is skipped entirely.
deliverReply :: Manager -> Text -> ChatId -> Text -> IO ()
deliverReply manager token cid reply = do
  let (cleaned, imgs) = extractImages reply
  forM_ imgs $ \img -> do
    exists <- try @IOException (doesFileExist img.path)
    case exists of
      Right True ->
        TelegramGateway.postSendPhoto manager token cid img.path img.caption
      Right False ->
        logChat cid $ "image path missing, skipping: " <> toText img.path
      Left err ->
        logChat cid $ "image path check failed: " <> toText (show err :: String)
  let trimmed = T.strip cleaned
  unless (T.null trimmed) $
    TelegramGateway.postSendMessage manager token cid trimmed
