module Haskclaw.Unit.Cli.HelpSpec (spec) where

import Relude

import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Haskclaw.Cli.Help (helpText, isHelpRequested)

spec :: Spec
spec = do
  describe "isHelpRequested" $ do
    it "is False when no flags are passed" $
      isHelpRequested [] `shouldBe` False

    it "is True for --help" $
      isHelpRequested ["--help"] `shouldBe` True

    it "is True for -h" $
      isHelpRequested ["-h"] `shouldBe` True

    it "is True when --help appears alongside other flags" $
      isHelpRequested ["--all", "--help"] `shouldBe` True

    it "is False for unrelated flags" $
      isHelpRequested ["--all", "--dangerously-skip-permissions"] `shouldBe` False

  describe "helpText" $ do
    it "mentions every supported flag" $ do
      helpText `shouldSatisfy` ("--help" `T.isInfixOf`)
      helpText `shouldSatisfy` ("-h" `T.isInfixOf`)
      helpText `shouldSatisfy` ("--all" `T.isInfixOf`)
      helpText `shouldSatisfy` ("--all-messages" `T.isInfixOf`)
      helpText `shouldSatisfy` ("--assistant" `T.isInfixOf`)
      helpText `shouldSatisfy` ("--dangerously-skip-permissions" `T.isInfixOf`)

    it "documents the TELEGRAM_BOT_TOKEN env var" $
      helpText `shouldSatisfy` ("TELEGRAM_BOT_TOKEN" `T.isInfixOf`)

    it "starts with a Usage line" $
      helpText `shouldSatisfy` ("Usage:" `T.isPrefixOf`)
