module Haskclaw.Telegram.Infra.Gateway.TelegramHttpGateway
  ( fetchUpdates
  , fetchMe
  , postSendMessage
  , postSendHtml
  , postSendChatAction
  , postSendPhoto
  , buildMultipartBody
  , photoMimeType
  ) where

import Relude

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecode, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
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
import System.FilePath (takeExtension, takeFileName)
import System.Random (randomIO)

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..), GetMeResponse (..), GetUpdatesResponse (..), Update, UpdateId (..))

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

fetchMe :: Manager -> Text -> IO (Maybe Text)
fetchMe manager token = do
  let url = "https://api.telegram.org/bot" <> toString token <> "/getMe"
  req <- parseRequest url
  resp <- httpLbs req manager
  if statusIsSuccessful (responseStatus resp)
    then case eitherDecode @GetMeResponse (responseBody resp) of
      Right r -> pure r.username
      Left err -> do
        putTextLn $ "getMe parse error: " <> toText err
        pure Nothing
    else do
      putTextLn $ "getMe HTTP error: " <> show (responseStatus resp)
      pure Nothing

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

-- | Send a message with @parse_mode=HTML@. Returns 'True' iff Telegram
--   accepted the request. Errors are logged (so the caller can fall back
--   to plain text on 'False') but not raised.
postSendHtml :: Manager -> Text -> ChatId -> Text -> IO Bool
postSendHtml manager token (ChatId cid) html = do
  let url = "https://api.telegram.org/bot" <> toString token <> "/sendMessage"
      body = encode $ object
        [ "chat_id" .= cid
        , "text" .= html
        , "parse_mode" .= ("HTML" :: Text)
        ]
  req <- parseRequest url
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders = [("Content-Type", "application/json")]
        }
  resp <- httpLbs req' manager
  let ok = statusIsSuccessful (responseStatus resp)
  unless ok $
    putTextLn $ "sendMessage(HTML) error: " <> show (responseStatus resp)
      <> " " <> decodeUtf8 (toStrict (responseBody resp))
  pure ok

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

-- | Send a local image file as a photo. Falls back silently when the file is
--   missing or the upload fails; errors are logged but never raised so the
--   text portion of the reply still reaches the user.
postSendPhoto :: Manager -> Text -> ChatId -> FilePath -> Maybe Text -> IO ()
postSendPhoto manager token (ChatId cid) photoPath mCaption = do
  eBytes <- try @IOException (BS.readFile photoPath)
  case eBytes of
    Left err ->
      putTextLn $ "sendPhoto read error: " <> toText (show err :: String)
    Right bytes -> do
      boundary <- buildBoundary
      let url = "https://api.telegram.org/bot" <> toString token <> "/sendPhoto"
          body = buildMultipartBody boundary (show cid) mCaption photoPath bytes
          ctype = "multipart/form-data; boundary=" <> encodeUtf8 boundary
      req <- parseRequest url
      let req' = req
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = [("Content-Type", ctype)]
            }
      resp <- httpLbs req' manager
      unless (statusIsSuccessful (responseStatus resp)) $
        putTextLn $ "sendPhoto error: " <> show (responseStatus resp)
          <> " " <> decodeUtf8 (toStrict (responseBody resp))

buildBoundary :: IO Text
buildBoundary = do
  n <- randomIO @Word64
  pure $ "haskclaw-boundary-" <> show n

-- | Build a multipart/form-data body for Telegram's sendPhoto. Exposed for
--   unit tests; pure (no file IO).
buildMultipartBody
  :: Text       -- ^ boundary (without leading dashes)
  -> Text       -- ^ chat_id as decimal
  -> Maybe Text -- ^ optional caption
  -> FilePath   -- ^ photo file path (used for filename + MIME)
  -> ByteString -- ^ photo file bytes
  -> LByteString
buildMultipartBody boundary chatIdText mCaption photoPath bytes =
  let b = "--" <> encodeUtf8 boundary
      crlf = "\r\n"
      field name value =
        b <> crlf
          <> "Content-Disposition: form-data; name=\"" <> name <> "\"" <> crlf
          <> crlf
          <> value <> crlf
      filePart =
        b <> crlf
          <> "Content-Disposition: form-data; name=\"photo\"; filename=\""
          <> encodeUtf8 (toText (takeFileName photoPath)) <> "\"" <> crlf
          <> "Content-Type: " <> encodeUtf8 (photoMimeType photoPath) <> crlf
          <> crlf
          <> bytes <> crlf
      trailer = b <> "--" <> crlf
      captionPart = maybe "" (field "caption" . encodeUtf8) mCaption
      head' = field "chat_id" (encodeUtf8 chatIdText)
  in LBS.fromStrict (head' <> captionPart <> filePart <> trailer)

photoMimeType :: FilePath -> Text
photoMimeType p = case T.toLower (toText (takeExtension p)) of
  ".png"  -> "image/png"
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".webp" -> "image/webp"
  ".gif"  -> "image/gif"
  _       -> "application/octet-stream"

buildGetUpdatesRequest :: Text -> Maybe UpdateId -> IO Request
buildGetUpdatesRequest token mOffset = do
  let baseUrl = "https://api.telegram.org/bot" <> toString token <> "/getUpdates?timeout=30"
      url = case mOffset of
        Nothing -> baseUrl
        Just (UpdateId oid) -> baseUrl <> "&offset=" <> show oid
  req <- parseRequest url
  pure req { responseTimeout = responseTimeoutMicro (60 * 1000000) }
