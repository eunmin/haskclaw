module Haskclaw.Cli.Help
  ( isHelpRequested
  , helpText
  ) where

import Relude

isHelpRequested :: [String] -> Bool
isHelpRequested args = "--help" `elem` args || "-h" `elem` args

helpText :: Text
helpText = unlines
  [ "Usage: haskclaw-exe [OPTIONS]"
  , ""
  , "Telegram <-> Claude Code bridge bot."
  , ""
  , "Options:"
  , "  -h, --help                      Show this help message and exit"
  , "  --all, --all-messages           Dispatch every message in group chats"
  , "                                  (default: only messages that mention the bot"
  , "                                  or reply to one of its messages)"
  , "  --dangerously-skip-permissions  Forward this flag to the `claude` subprocess"
  , "                                  so it skips its permission prompts"
  , ""
  , "Environment:"
  , "  TELEGRAM_BOT_TOKEN              Required. Telegram Bot API token."
  ]
