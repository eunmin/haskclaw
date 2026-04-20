# haskclaw

Telegram ↔ Claude Code bridge bot with per-chat isolation and a cron scheduler.

## Features

- Long-polling Telegram bot
- Per-chat working directory under `~/.haskclaw/chats/<chat_id>/`
- Session resume via `claude -p --resume`
- Scheduler MCP server (`schedule_task`, `list_tasks`, `cancel_task`, ...) — register recurring tasks in natural language

## Requirements

- [Stack](https://haskellstack.org)
- [Claude Code CLI](https://docs.claude.com/en/docs/claude-code) on `PATH`
- A Telegram bot token ([@BotFather](https://t.me/BotFather))

## Build & Run

```sh
stack build
TELEGRAM_BOT_TOKEN=xxx stack exec haskclaw-exe
```

## Layout

```
~/.haskclaw/
├── CLAUDE.md            # shared instructions (auto-seeded)
├── state.json           # chat → session id
└── chats/<chat_id>/
    ├── .mcp.json        # haskclaw MCP server registration
    ├── .claude/settings.json
    ├── schedules.json   # scheduled tasks
    └── bot.log
```

## Example

Send to the bot in Telegram:

> "Every morning at 8am, give me the weather."

Claude registers a cron task; the bot delivers the response each tick.

## License

BSD-3-Clause.
