module Haskclaw.Mcp.Server
  ( runServer
  , handleLine
  ) where

import Relude

import Data.Aeson
  ( FromJSON (..)
  , Value (..)
  , eitherDecodeStrict
  , encode
  , object
  , withObject
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser, parse)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Time (getCurrentTime)
import qualified System.IO as SIO

import Haskclaw.Scheduler.Cron (parseSchedule)
import Haskclaw.Scheduler.Store
  ( addTask
  , loadTasks
  , removeTask
  , updateTask
  )
import Haskclaw.Scheduler.Types
  ( ScheduledTask (..)
  , TaskId (..)
  , TaskStatus (..)
  , newTaskId
  )
import Haskclaw.Telegram.Command.Domain.Types (ChatId (..))

-- =====================================================================
-- Entry point
-- =====================================================================

-- | Stdio loop. One JSON-RPC message per stdin line, one response per stdout
--   line. Chat context comes from the HASKCLAW_CHAT_ID env var.
runServer :: IO ()
runServer = do
  SIO.hSetBuffering SIO.stdin SIO.LineBuffering
  SIO.hSetBuffering SIO.stdout SIO.LineBuffering
  mCid <- readChatIdEnv
  loop mCid
  where
    loop mCid = do
      eof <- SIO.isEOF
      unless eof $ do
        line <- BS8.hGetLine SIO.stdin
        mResp <- handleLine mCid line
        whenJust mResp $ \resp -> do
          LBS.hPutStr SIO.stdout (encode resp)
          BS8.hPutStr SIO.stdout "\n"
          SIO.hFlush SIO.stdout
        loop mCid

readChatIdEnv :: IO (Maybe ChatId)
readChatIdEnv = do
  mv <- lookupEnv "HASKCLAW_CHAT_ID"
  pure $ fmap ChatId (mv >>= readMaybe)

-- =====================================================================
-- JSON-RPC envelope
-- =====================================================================

data RpcRequest = RpcRequest
  { rpcId     :: Maybe Value
  , rpcMethod :: Text
  , rpcParams :: Maybe Value
  } deriving stock (Show)

instance FromJSON RpcRequest where
  parseJSON = withObject "RpcRequest" $ \v -> do
    rpcId     <- v .:? "id"
    rpcMethod <- v .:  "method"
    rpcParams <- v .:? "params"
    pure RpcRequest{..}

rpcResult :: Value -> Value -> Value
rpcResult reqId result =
  object ["jsonrpc" .= ("2.0" :: Text), "id" .= reqId, "result" .= result]

rpcError :: Value -> Int -> Text -> Value
rpcError reqId code msg =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= reqId
    , "error" .= object ["code" .= code, "message" .= msg]
    ]

runParser :: (Value -> Aeson.Parser a) -> Value -> Either Text a
runParser p v = case Aeson.parse p v of
  Aeson.Success x -> Right x
  Aeson.Error e   -> Left (toText e)

-- =====================================================================
-- Top-level line handler
-- =====================================================================

-- | Parse a line and produce the response (if any). Notifications yield
--   Nothing. Exported for unit tests.
handleLine :: Maybe ChatId -> ByteString -> IO (Maybe Value)
handleLine mCid line = case eitherDecodeStrict line of
  Left err -> pure $ Just $ rpcError Null (-32700) ("parse error: " <> toText err)
  Right req -> case rpcId req of
    Nothing  -> pure Nothing
    Just rid -> Just <$> dispatchRequest mCid rid req

dispatchRequest :: Maybe ChatId -> Value -> RpcRequest -> IO Value
dispatchRequest mCid rid req = case rpcMethod req of
  "initialize" -> pure $ rpcResult rid initializeResult
  "tools/list" -> pure $ rpcResult rid toolsListResult
  "tools/call" -> handleToolsCall mCid rid (rpcParams req)
  other        -> pure $ rpcError rid (-32601) ("method not found: " <> other)

-- =====================================================================
-- initialize / tools/list
-- =====================================================================

initializeResult :: Value
initializeResult = object
  [ "protocolVersion" .= ("2024-11-05" :: Text)
  , "capabilities"    .= object ["tools" .= object []]
  , "serverInfo"      .= object
      [ "name"    .= ("haskclaw" :: Text)
      , "version" .= ("0.1.0" :: Text)
      ]
  ]

toolsListResult :: Value
toolsListResult = object ["tools" .= toolDefs]
  where
    toolDefs :: [Value]
    toolDefs =
      [ tool "schedule_task"
          "Register a recurring task. Prompt is sent to the assistant on each cron tick and the response is delivered to the current chat."
          (objSchema ["prompt", "cron"]
            [ ("prompt", "What to ask the assistant when the task fires.")
            , ("cron",   "5-field cron expression in local time, e.g. \"0 8 * * *\".")
            , ("label",  "Optional short label for humans.")
            ])
      , tool "list_tasks"
          "List all scheduled tasks for this chat."
          (objSchema [] [])
      , tool "cancel_task" "Delete a scheduled task."
          (objSchema ["id"] [("id", "Task id.")])
      , tool "pause_task"  "Pause a scheduled task."
          (objSchema ["id"] [("id", "Task id.")])
      , tool "resume_task" "Resume a paused task."
          (objSchema ["id"] [("id", "Task id.")])
      , tool "update_task" "Update a task's prompt, cron, or label."
          (objSchema ["id"]
            [ ("id",     "Task id.")
            , ("prompt", "New prompt (optional).")
            , ("cron",   "New cron expression (optional).")
            , ("label",  "New label (optional).")
            ])
      ]

    tool name desc schema = object
      [ "name" .= (name :: Text)
      , "description" .= (desc :: Text)
      , "inputSchema" .= schema
      ]

    objSchema :: [Text] -> [(Text, Text)] -> Value
    objSchema required props = object
      [ "type"       .= ("object" :: Text)
      , "required"   .= required
      , "properties" .= object [ fromString (toString k) .= propStr d | (k, d) <- props ]
      ]

    propStr :: Text -> Value
    propStr d = object ["type" .= ("string" :: Text), "description" .= d]

-- =====================================================================
-- tools/call
-- =====================================================================

handleToolsCall :: Maybe ChatId -> Value -> Maybe Value -> IO Value
handleToolsCall mCid rid mParams = do
  let params = fromMaybe (object []) mParams
  case runParser callParser params of
    Left err -> pure $ toolErr rid err
    Right (name, args) -> case mCid of
      Nothing -> pure $ toolErr rid "HASKCLAW_CHAT_ID is not set"
      Just cid -> do
        (txt, isErr) <- runTool cid name args
        pure $ toolContent rid txt isErr
  where
    callParser = withObject "tools/call" $ \o -> (,)
      <$> o .: "name"
      <*> (fromMaybe (object []) <$> (o .:? "arguments"))

toolContent :: Value -> Text -> Bool -> Value
toolContent rid txt isErr =
  rpcResult rid $ object
    [ "content" .= [object ["type" .= ("text" :: Text), "text" .= txt]]
    , "isError" .= isErr
    ]

toolErr :: Value -> Text -> Value
toolErr rid msg = toolContent rid msg True

-- =====================================================================
-- Tool handlers
-- =====================================================================

runTool :: ChatId -> Text -> Value -> IO (Text, Bool)
runTool cid name args = case name of
  "schedule_task" -> runScheduleTask cid args
  "list_tasks"    -> runListTasks cid
  "cancel_task"   -> runOnTaskId args (cancelTask cid)
  "pause_task"    -> runOnTaskId args (setStatus cid TaskPaused)
  "resume_task"   -> runOnTaskId args (setStatus cid TaskActive)
  "update_task"   -> runUpdateTask cid args
  other           -> pure ("unknown tool: " <> other, True)

runScheduleTask :: ChatId -> Value -> IO (Text, Bool)
runScheduleTask cid args = case runParser parser args of
  Left err -> pure (err, True)
  Right (promptTxt, cronTxt, mLabel) -> case parseSchedule cronTxt of
    Left e -> pure ("invalid cron: " <> e, True)
    Right _ -> do
      now <- getCurrentTime
      tid <- newTaskId now
      let t = ScheduledTask
            { taskId    = tid
            , cron      = cronTxt
            , prompt    = promptTxt
            , label     = mLabel
            , status    = TaskActive
            , createdAt = now
            , lastRunAt = Nothing
            }
      addTask cid t
      pure ("scheduled " <> unTaskId tid <> " (cron=" <> cronTxt <> ")", False)
  where
    parser = withObject "schedule_task" $ \o -> do
      p <- o .:  "prompt"
      c <- o .:  "cron"
      l <- o .:? "label"
      pure (p, c, l)

runListTasks :: ChatId -> IO (Text, Bool)
runListTasks cid = do
  ts <- loadTasks cid
  if null ts
    then pure ("(no tasks)", False)
    else pure (decodeUtf8 (LBS.toStrict (Aeson.encode ts)), False)

runOnTaskId :: Value -> (TaskId -> IO (Text, Bool)) -> IO (Text, Bool)
runOnTaskId args k = case runParser (withObject "taskId" (fmap TaskId . (.: "id"))) args of
  Left err  -> pure (err, True)
  Right tid -> k tid

cancelTask :: ChatId -> TaskId -> IO (Text, Bool)
cancelTask cid tid = do
  ok <- removeTask cid tid
  pure $ if ok
    then ("cancelled " <> unTaskId tid, False)
    else ("no such task: " <> unTaskId tid, True)

setStatus :: ChatId -> TaskStatus -> TaskId -> IO (Text, Bool)
setStatus cid st tid = do
  ok <- updateTask cid tid (\t -> t { status = st })
  pure $ if ok
    then (verb <> " " <> unTaskId tid, False)
    else ("no such task: " <> unTaskId tid, True)
  where
    verb = case st of
      TaskActive -> "resumed"
      TaskPaused -> "paused"

runUpdateTask :: ChatId -> Value -> IO (Text, Bool)
runUpdateTask cid args = case runParser parser args of
  Left err -> pure (err, True)
  Right (tid, mPrompt, mCron, mLabel) -> case mCron of
    Just c | Left e <- parseSchedule c -> pure ("invalid cron: " <> e, True)
    _ -> do
      ok <- updateTask cid tid $ \t -> t
        { prompt = fromMaybe t.prompt mPrompt
        , cron   = fromMaybe t.cron mCron
        , label  = mLabel <|> t.label
        }
      pure $ if ok
        then ("updated " <> unTaskId tid, False)
        else ("no such task: " <> unTaskId tid, True)
  where
    parser = withObject "update_task" $ \o -> do
      tid <- TaskId <$> o .: "id"
      mp  <- o .:? "prompt"
      mc  <- o .:? "cron"
      ml  <- o .:? "label"
      pure (tid, mp, mc, ml)
