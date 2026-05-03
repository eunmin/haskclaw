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
  , "Telegram <-> assistant CLI bridge bot."
  , ""
  , "Options:"
  , "  -h, --help                      Show this help message and exit"
  , "  --all, --all-messages           Dispatch every message in group chats"
  , "                                  (default: only messages that mention the bot"
  , "                                  or reply to one of its messages)"
  , "  --assistant PROVIDER            Assistant CLI to use: claude or codex"
  , "                                  (default: claude)"
  , "  --dangerously-skip-permissions  Forward the provider-specific unsafe"
  , "                                  permission bypass flag to the subprocess"
  , ""
  , "Environment:"
  , "  TELEGRAM_BOT_TOKEN              Required. Telegram Bot API token."
  ]
