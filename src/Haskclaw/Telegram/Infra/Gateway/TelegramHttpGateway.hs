module Haskclaw.Telegram.Infra.Gateway.TelegramHttpGateway
  ( fetchUpdates
  , postSendMessage
  , postSendChatAction
  ) where

import Relude

import Data.Aeson (eitherDecode, encode, object, (.=))
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody (..)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Status (statusIsSuccessful)

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..), GetUpdatesResponse (..), Update, UpdateId (..))

fetchUpdates :: Manager -> Text -> Maybe UpdateId -> IO [Update]
fetchUpdates manager token mOffset = do
  req <- buildGetUpdatesRequest token mOffset
  resp <- httpLbs req manager
  if statusIsSuccessful (responseStatus resp)
    then case eitherDecode @GetUpdatesResponse (responseBody resp) of
      Right r -> pure r.result
      Left err -> do
        putTextLn $ "JSON parse error: " <> toText err
        pure []
    else do
      putTextLn $ "HTTP error: " <> show (responseStatus resp)
      pure []

postSendMessage :: Manager -> Text -> ChatId -> Text -> IO ()
postSendMessage manager token (ChatId cid) text = do
  let url = "https://api.telegram.org/bot" <> toString token <> "/sendMessage"
      body = encode $ object
        [ "chat_id" .= cid
        , "text" .= text
        ]
  req <- parseRequest url
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders = [("Content-Type", "application/json")]
        }
  resp <- httpLbs req' manager
  unless (statusIsSuccessful (responseStatus resp)) $
    putTextLn $ "sendMessage error: " <> show (responseStatus resp)
      <> " " <> decodeUtf8 (toStrict (responseBody resp))

postSendChatAction :: Manager -> Text -> ChatId -> Text -> IO ()
postSendChatAction manager token (ChatId cid) action = do
  let url = "https://api.telegram.org/bot" <> toString token <> "/sendChatAction"
      body = encode $ object
        [ "chat_id" .= cid
        , "action" .= action
        ]
  req <- parseRequest url
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders = [("Content-Type", "application/json")]
        }
  resp <- httpLbs req' manager
  unless (statusIsSuccessful (responseStatus resp)) $
    putTextLn $ "sendChatAction error: " <> show (responseStatus resp)
      <> " " <> decodeUtf8 (toStrict (responseBody resp))

buildGetUpdatesRequest :: Text -> Maybe UpdateId -> IO Request
buildGetUpdatesRequest token mOffset = do
  let baseUrl = "https://api.telegram.org/bot" <> toString token <> "/getUpdates?timeout=30"
      url = case mOffset of
        Nothing -> baseUrl
        Just (UpdateId oid) -> baseUrl <> "&offset=" <> show oid
  req <- parseRequest url
  pure req { responseTimeout = responseTimeoutMicro (60 * 1000000) }
