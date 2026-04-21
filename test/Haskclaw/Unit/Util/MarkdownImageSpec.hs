module Haskclaw.Unit.Util.MarkdownImageSpec (spec) where

import Relude

import Test.Hspec (Spec, describe, it, shouldBe)

import Haskclaw.Util.MarkdownImage (InlineImage (..), extractImages, isLocalImagePath)

spec :: Spec
spec = do
  describe "isLocalImagePath" $ do
    it "accepts an absolute .png path" $
      isLocalImagePath "/tmp/foo.png" `shouldBe` True

    it "accepts .jpg/.jpeg/.webp/.gif" $
      all isLocalImagePath
        [ "/a/b.jpg", "/a/b.jpeg", "/a/b.webp", "/a/b.gif" ]
        `shouldBe` True

    it "is case-insensitive for extensions" $
      isLocalImagePath "/tmp/Foo.PNG" `shouldBe` True

    it "rejects relative paths" $
      isLocalImagePath "foo.png" `shouldBe` False

    it "rejects http and https URLs" $
      any isLocalImagePath
        [ "http://example.com/x.png"
        , "https://example.com/x.png"
        , "data:image/png;base64,AAA"
        ]
        `shouldBe` False

    it "rejects non-image extensions" $
      isLocalImagePath "/tmp/foo.txt" `shouldBe` False

  describe "extractImages" $ do
    it "returns the input unchanged when there are no images" $
      extractImages "hello world" `shouldBe` ("hello world", [])

    it "extracts a single absolute image and removes the markup" $ do
      let (txt, imgs) = extractImages
            "before ![cap](/tmp/shot.png) after"
      txt `shouldBe` "before after"
      imgs `shouldBe` [InlineImage (Just "cap") "/tmp/shot.png"]

    it "treats an empty alt as a missing caption" $ do
      let (_, imgs) = extractImages "![](/tmp/shot.png)"
      imgs `shouldBe` [InlineImage Nothing "/tmp/shot.png"]

    it "extracts multiple images in order" $ do
      let (_, imgs) = extractImages
            "a ![one](/tmp/a.png) b ![two](/tmp/b.jpg) c"
      imgs `shouldBe`
        [ InlineImage (Just "one") "/tmp/a.png"
        , InlineImage (Just "two") "/tmp/b.jpg"
        ]

    it "leaves remote URLs untouched in the text" $ do
      let input = "see ![](https://example.com/x.png) for details"
          (txt, imgs) = extractImages input
      txt `shouldBe` input
      imgs `shouldBe` []

    it "leaves relative paths untouched in the text" $ do
      let input = "see ![](./x.png) here"
          (txt, imgs) = extractImages input
      txt `shouldBe` input
      imgs `shouldBe` []

    it "does not cross a newline inside the alt or url" $ do
      let input = "![alt\nnope](/tmp/x.png)"
          (txt, imgs) = extractImages input
      imgs `shouldBe` []
      "![alt" `elem` lines txt `shouldBe` True

    it "collapses blank lines when an image stood alone on a line" $ do
      let input = "line one\n\n![](/tmp/x.png)\n\nline two"
          (txt, _) = extractImages input
      txt `shouldBe` "line one\n\nline two"

    it "strips leading/trailing whitespace produced by removal" $ do
      let (txt, _) = extractImages "  ![](/tmp/x.png)  "
      txt `shouldBe` ""
