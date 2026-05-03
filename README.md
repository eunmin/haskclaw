# haskclaw

Telegram ↔ assistant CLI bridge bot with per-chat isolation and a cron scheduler.

## Features

- Long-polling Telegram bot
- Per-chat working directory under `~/.haskclaw/chats/<chat_id>/`
- Session resume via `claude -p --resume` or `codex exec resume`
- Scheduler MCP server (`schedule_task`, `list_tasks`, `cancel_task`, ...) — register recurring tasks in natural language

## Requirements

- [Stack](https://haskellstack.org)
- [Claude Code CLI](https://docs.claude.com/en/docs/claude-code) or Codex CLI on `PATH`
- A Telegram bot token ([@BotFather](https://t.me/BotFather))

## Build & Run

```sh
stack build
TELEGRAM_BOT_TOKEN=xxx stack exec haskclaw-exe
```

## CLI Options

Pass flags after `--` when running through `stack exec`:

```sh
stack exec haskclaw-exe -- --help
```

| Option | Description |
| --- | --- |
| `-h`, `--help` | Show the help message and exit. |
| `--all`, `--all-messages` | Dispatch every message in group chats. Default behavior is to forward only messages that mention the bot or reply to one of its messages. |
| `--assistant claude\|codex` | Choose the assistant CLI provider. Defaults to `claude`. |
| `--assistant-provider claude\|codex` | Alias for `--assistant`. |
| `--dangerously-skip-permissions` | Forward the provider-specific unsafe permission bypass flag to the subprocess. |

| Environment Variable | Description |
| --- | --- |
| `TELEGRAM_BOT_TOKEN` | Required. Telegram Bot API token from [@BotFather](https://t.me/BotFather). |

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

The assistant registers a cron task; the bot delivers the response each tick.

## License

MIT.
