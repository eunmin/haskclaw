module Haskclaw.Unit.Telegram.Infra.Gateway.TelegramHttpGatewaySpec (spec) where

import Relude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Haskclaw.Telegram.Infra.Gateway.TelegramHttpGateway
  ( buildMultipartBody
  , photoMimeType
  )

spec :: Spec
spec = do
  describe "photoMimeType" $ do
    it "maps .png to image/png" $
      photoMimeType "/tmp/a.png" `shouldBe` "image/png"

    it "maps .jpg and .jpeg to image/jpeg" $ do
      photoMimeType "/tmp/a.jpg" `shouldBe` "image/jpeg"
      photoMimeType "/tmp/a.jpeg" `shouldBe` "image/jpeg"

    it "maps .webp and .gif" $ do
      photoMimeType "/tmp/a.webp" `shouldBe` "image/webp"
      photoMimeType "/tmp/a.gif" `shouldBe` "image/gif"

    it "is case-insensitive" $
      photoMimeType "/tmp/A.PNG" `shouldBe` "image/png"

    it "falls back to application/octet-stream" $
      photoMimeType "/tmp/a.xyz" `shouldBe` "application/octet-stream"

  describe "buildMultipartBody" $ do
    let body = buildMultipartBody "B" "42" (Just "hi") "/tmp/pic.png" "PAYLOAD"
        txt = decodeUtf8 (LBS.toStrict body) :: Text

    it "contains the chat_id part" $
      all (`T.isInfixOf` txt) ["name=\"chat_id\"", "42"] `shouldBe` True

    it "contains the caption part when provided" $
      all (`T.isInfixOf` txt) ["name=\"caption\"", "hi"] `shouldBe` True

    it "contains the photo part with filename, content-type, and payload" $
      all (`T.isInfixOf` txt)
        [ "name=\"photo\""
        , "filename=\"pic.png\""
        , "Content-Type: image/png"
        , "PAYLOAD"
        ]
        `shouldBe` True

    it "starts with the boundary marker and ends with the closing marker" $ do
      txt `shouldSatisfy` T.isPrefixOf "--B\r\n"
      txt `shouldSatisfy` T.isSuffixOf "--B--\r\n"

    it "omits the caption part when caption is Nothing" $ do
      let noCap = decodeUtf8 @Text
            (LBS.toStrict (buildMultipartBody "B" "1" Nothing "/x.png" ""))
      "name=\"caption\"" `T.isInfixOf` noCap `shouldBe` False

    it "uses CRLF line endings between parts" $ do
      let marker = "name=\"chat_id\"\r\n\r\n42\r\n"
      marker `T.isInfixOf` txt `shouldBe` True
