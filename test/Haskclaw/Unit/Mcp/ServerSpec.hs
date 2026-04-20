module Haskclaw.Unit.Mcp.ServerSpec (spec) where

import Relude

import Control.Exception (finally)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Environment (setEnv, unsetEnv)
import qualified System.Environment as Env
import System.FilePath ((</>))
import Test.Hspec (Spec, around_, describe, it, shouldBe, shouldSatisfy)

import Haskclaw.Mcp.Server (handleLine)
import Haskclaw.Scheduler.Store (loadTasks)
import Haskclaw.Telegram.Command.Domain.Types (ChatId (..))

withTempHome :: IO () -> IO ()
withTempHome io = do
  tmp <- getTemporaryDirectory
  let fakeHome = tmp </> "haskclaw-mcp-spec"
  removePathForcibly fakeHome
  original <- Env.lookupEnv "HOME"
  setEnv "HOME" fakeHome
  io `finally` do
    removePathForcibly fakeHome
    case original of
      Just v  -> setEnv "HOME" v
      Nothing -> unsetEnv "HOME"

call :: Maybe ChatId -> LByteString -> IO (Maybe Value)
call cid = handleLine cid . LBS.toStrict

req :: Int -> Text -> Value -> LByteString
req rid method params =
  Aeson.encode $ Aeson.object
    [ "jsonrpc" Aeson..= ("2.0" :: Text)
    , "id" Aeson..= rid
    , "method" Aeson..= method
    , "params" Aeson..= params
    ]

spec :: Spec
spec = around_ withTempHome $ describe "Mcp.Server.handleLine" $ do
  it "responds to initialize" $ do
    mresp <- call (Just (ChatId 1)) (req 1 "initialize" (Aeson.object []))
    mresp `shouldSatisfy` isJust

  it "lists the registered tools" $ do
    Just resp <- call (Just (ChatId 1)) (req 2 "tools/list" (Aeson.object []))
    let s = decodeUtf8 @Text (LBS.toStrict (Aeson.encode resp))
    s `shouldSatisfy` \t -> all (`isInfixOfT` t)
      ["schedule_task", "list_tasks", "cancel_task", "pause_task", "resume_task", "update_task"]

  it "schedule_task registers a task and schedules.json records it" $ do
    let args = Aeson.object
          [ "name" Aeson..= ("schedule_task" :: Text)
          , "arguments" Aeson..= Aeson.object
              [ "prompt" Aeson..= ("weather" :: Text)
              , "cron" Aeson..= ("0 8 * * *" :: Text)
              ]
          ]
    Just resp <- call (Just (ChatId 1)) (req 3 "tools/call" args)
    let s = decodeUtf8 @Text (LBS.toStrict (Aeson.encode resp))
    s `shouldSatisfy` ("scheduled" `isInfixOfT`)
    tasks <- loadTasks (ChatId 1)
    length tasks `shouldBe` 1

  it "reports an error when cron is invalid" $ do
    let args = Aeson.object
          [ "name" Aeson..= ("schedule_task" :: Text)
          , "arguments" Aeson..= Aeson.object
              [ "prompt" Aeson..= ("x" :: Text)
              , "cron" Aeson..= ("not a cron" :: Text)
              ]
          ]
    Just resp <- call (Just (ChatId 1)) (req 4 "tools/call" args)
    let s = decodeUtf8 @Text (LBS.toStrict (Aeson.encode resp))
    s `shouldSatisfy` ("isError\":true" `isInfixOfT`)

  it "errors out when HASKCLAW_CHAT_ID is unknown" $ do
    let args = Aeson.object
          [ "name" Aeson..= ("list_tasks" :: Text)
          , "arguments" Aeson..= Aeson.object []
          ]
    Just resp <- call Nothing (req 5 "tools/call" args)
    let s = decodeUtf8 @Text (LBS.toStrict (Aeson.encode resp))
    s `shouldSatisfy` ("HASKCLAW_CHAT_ID" `isInfixOfT`)

  where
    isInfixOfT :: Text -> Text -> Bool
    isInfixOfT = T.isInfixOf
