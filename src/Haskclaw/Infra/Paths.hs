module Haskclaw.Infra.Paths
  ( haskclawHome
  , chatsDir
  , chatDir
  , chatConfigDir
  , chatSettingsPath
  , chatMcpJsonPath
  , sharedClaudeMdPath
  , stateFilePath
  , chatIdSlug
  , defaultSharedClaudeMd
  , mcpBinaryPath
  , mergeHaskclawSettings
  , buildMcpJson
  , upsertManagedBlock
  , managedBeginMarker
  , managedEndMarker
  , ensureHaskclawDirs
  , ensureChatDir
  , ensureChatConfig
  ) where

import Relude

import Data.Aeson (Value (..), eitherDecode, encode, object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory
  , renameFile
  )
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

import Haskclaw.Telegram.Command.Domain.Types (ChatId (..))

-- =====================================================================
-- Paths
-- =====================================================================

haskclawHome :: IO FilePath
haskclawHome = do
  home <- getHomeDirectory
  pure (home </> ".haskclaw")

chatsDir :: IO FilePath
chatsDir = (</> "chats") <$> haskclawHome

chatDir :: ChatId -> IO FilePath
chatDir cid = (</> chatIdSlug cid) <$> chatsDir

chatConfigDir :: ChatId -> IO FilePath
chatConfigDir cid = (</> ".claude") <$> chatDir cid

chatSettingsPath :: ChatId -> IO FilePath
chatSettingsPath cid = (</> "settings.json") <$> chatConfigDir cid

-- | ~/.haskclaw/chats/<slug>/.mcp.json — Claude Code picks up MCP servers
--   from this project-root file (not from settings.json).
chatMcpJsonPath :: ChatId -> IO FilePath
chatMcpJsonPath cid = (</> ".mcp.json") <$> chatDir cid

stateFilePath :: IO FilePath
stateFilePath = (</> "state.json") <$> haskclawHome

sharedClaudeMdPath :: IO FilePath
sharedClaudeMdPath = (</> "CLAUDE.md") <$> haskclawHome

chatIdSlug :: ChatId -> FilePath
chatIdSlug (ChatId n)
  | n < 0 = "neg_" <> show (abs n)
  | otherwise = show n

mcpBinaryPath :: IO FilePath
mcpBinaryPath = do
  self <- getExecutablePath
  pure (takeDirectory self </> "haskclaw-mcp")

-- =====================================================================
-- .mcp.json builder + settings.json permission merge
-- =====================================================================

-- | Full content of the chat's .mcp.json — registers the haskclaw server.
--   Rewritten on every incoming message so the binary path stays current.
buildMcpJson :: ChatId -> FilePath -> Value
buildMcpJson (ChatId n) mcpPath = object
  [ "mcpServers" .= object
      [ "haskclaw" .= object
          [ "command" .= (toText mcpPath :: Text)
          , "args"    .= ([] :: [Text])
          , "env"     .= object [ "HASKCLAW_CHAT_ID" .= show @Text n ]
          ]
      ]
  ]

-- | Idempotent merge: ensures 'permissions.allow' contains the haskclaw-managed
--   entries (MCP tool, agent-browser Bash, Read access to the chat project
--   directory). All other keys in the object are preserved untouched.
mergeHaskclawSettings :: FilePath -> Value -> Value
mergeHaskclawSettings dir input =
  let base = objectKeyMap input
      withMcp = ensureAllowPerm "mcp__haskclaw" base
      withBrowser = ensureAllowPerm "Bash(agent-browser:*)" withMcp
      withRead = ensureAllowPerm (chatReadPerm dir) withBrowser
  in Object withRead

-- | Build the 'Read(//abs/path/**)' permission string Claude Code expects.
chatReadPerm :: FilePath -> Text
chatReadPerm dir = "Read(//" <> toText dir <> "/**)"

objectKeyMap :: Value -> KM.KeyMap Value
objectKeyMap (Object o) = o
objectKeyMap _          = KM.empty

ensureAllowPerm :: Text -> KM.KeyMap Value -> KM.KeyMap Value
ensureAllowPerm perm m =
  let perms   = objectKeyMap (fromMaybe (object []) (KM.lookup "permissions" m))
      current = case KM.lookup "allow" perms of
        Just (Array v) -> toList v
        _              -> []
      strings = [s | String s <- current]
      updated = if perm `elem` strings
                  then current
                  else current <> [String perm]
      newPerms = KM.insert "allow" (toJSON updated) perms
  in KM.insert "permissions" (Object newPerms) m

-- =====================================================================
-- Shared CLAUDE.md managed block
-- =====================================================================

managedBeginMarker :: Text
managedBeginMarker = "<!-- haskclaw:begin -->"

managedEndMarker :: Text
managedEndMarker = "<!-- haskclaw:end -->"

-- | Seed content for the haskclaw-managed block inside the shared CLAUDE.md.
--   Listed with fully-qualified MCP tool names so Claude copies them verbatim.
defaultSharedClaudeMd :: Text
defaultSharedClaudeMd =
  "# haskclaw — scheduler instructions\n\
  \\n\
  \The haskclaw MCP server exposes these tools. Use the exact names below\n\
  \(note the double underscore separators):\n\
  \\n\
  \- `mcp__haskclaw__schedule_task` — register a recurring task.\n\
  \    Arguments: `prompt` (what to ask on each fire), `cron` (5-field expression), `label` (optional).\n\
  \- `mcp__haskclaw__list_tasks` — list this chat's tasks.\n\
  \- `mcp__haskclaw__cancel_task` — delete a task. Argument: `id`.\n\
  \- `mcp__haskclaw__pause_task` / `mcp__haskclaw__resume_task` — toggle status. Argument: `id`.\n\
  \- `mcp__haskclaw__update_task` — modify a task. Arguments: `id`, plus any of `prompt` / `cron` / `label`.\n\
  \\n\
  \Cron expressions are interpreted in the server's local time zone. Example:\n\
  \`0 8 * * *` for every day at 08:00, `*/5 * * * *` for every 5 minutes.\n\
  \\n\
  \When the user asks to schedule, cancel, or inspect a recurring task, call\n\
  \the corresponding tool rather than using Bash loops. Confirm each\n\
  \registration to the user, including the returned task id.\n\
  \\n\
  \Scheduled runs execute in a fresh Claude session (no resume); the response\n\
  \is delivered to this chat automatically.\n\
  \\n\
  \## Sending images back to Telegram\n\
  \\n\
  \To deliver a local image (e.g. a screenshot produced by the agent-browser\n\
  \skill) to the user, include it in your reply using standard Markdown image\n\
  \syntax with an **absolute local file path**:\n\
  \\n\
  \    ![optional caption](/absolute/path/to/image.png)\n\
  \\n\
  \The bot extracts each such reference, sends the file via Telegram's\n\
  \sendPhoto API (the alt text becomes the caption), and strips the markup\n\
  \from the text reply. Supported extensions: .png, .jpg, .jpeg, .webp, .gif.\n\
  \Remote URLs (http/https) are left untouched in the text.\n\
  \\n\
  \## Browser automation — agent-browser\n\
  \\n\
  \The `agent-browser` CLI is available on PATH. **Do not** call it through\n\
  \the Skill tool or a SlashCommand — it is a plain Bash command with\n\
  \subcommands. The bot has already whitelisted `Bash(agent-browser:*)`.\n\
  \\n\
  \Core workflow:\n\
  \\n\
  \1. `agent-browser open <url>` — navigate.\n\
  \2. `agent-browser snapshot -i` — list interactive elements with `@eN` refs.\n\
  \3. Interact: `click @e1`, `fill @e2 \"text\"`, `press Enter`, `hover @e3`,\n\
  \   `select @e4 \"value\"`, `scroll down 500`, `upload @e5 /abs/file`.\n\
  \4. Re-snapshot after navigation or major DOM changes.\n\
  \5. Extract: `get text @e1`, `get attr @e2 href`, `get title`, `get url`.\n\
  \6. Capture: `agent-browser screenshot <abs_path.png>` — then reference the\n\
  \   file in your reply using the Markdown image syntax above so Telegram\n\
  \   delivers it to the user.\n\
  \7. `agent-browser close` — release the browser when done.\n\
  \\n\
  \### Per-chat isolation and persistent login\n\
  \\n\
  \**Do not set `AGENT_BROWSER_SESSION`.** The bot already injects it so\n\
  \every agent-browser invocation in this chat routes to a daemon that is\n\
  \isolated from other chats. Concurrent chats can use the browser without\n\
  \colliding.\n\
  \\n\
  \Cookies and localStorage are persisted in a per-chat JSON file at\n\
  \`./browser-state.json` (relative to the current working directory, which\n\
  \is the chat's project root).\n\
  \\n\
  \**Launching with saved state** — pass `--state` on the first `open` of\n\
  \the turn, and only if the file exists. Close any stale daemon first so\n\
  \the load actually takes effect (agent-browser refuses `--state` on a\n\
  \running browser):\n\
  \\n\
  \    agent-browser close\n\
  \    if [ -f ./browser-state.json ]; then\n\
  \      agent-browser --state ./browser-state.json open <url>\n\
  \    else\n\
  \      agent-browser open <url>\n\
  \    fi\n\
  \\n\
  \**Saving** — only after the user is visibly authenticated (URL has moved\n\
  \past the login screen, the account indicator is present, etc.). Save\n\
  \once per successful login, not on every turn:\n\
  \\n\
  \    agent-browser state save ./browser-state.json\n\
  \\n\
  \**Closing** — at the end of a browser task, release the daemon so the\n\
  \next turn starts from a clean launch:\n\
  \\n\
  \    agent-browser close\n\
  \\n\
  \If the user is not yet logged in, walk them through the flow (they\n\
  \provide credentials; you `fill` and `click`), wait for the post-login\n\
  \URL, then save state.\n\
  \\n\
  \### Tips\n\
  \\n\
  \- Snapshots are accessibility-tree based; prefer `@eN` refs over raw CSS.\n\
  \- Use `agent-browser wait --url \"**/dashboard\"` or `wait --load networkidle`\n\
  \  after clicks that trigger navigation.\n\
  \- Save screenshots under `/tmp/` or the chat directory so the absolute\n\
  \  path is easy to embed in the reply.\n"

-- | Upsert a managed block inside a longer document. The block is delimited
--   by 'managedBeginMarker' and 'managedEndMarker'. Content outside the
--   markers is preserved verbatim. If the markers are missing the block is
--   appended at the end; if only 'managedBeginMarker' is present the existing
--   begin is treated as corrupt and the block is re-appended.
upsertManagedBlock :: Text -> Text -> Text
upsertManagedBlock block existing =
  let wrapped =
        managedBeginMarker <> "\n"
        <> T.stripEnd block <> "\n"
        <> managedEndMarker
      (preBegin, fromBegin) = T.breakOn managedBeginMarker existing
  in if T.null fromBegin
       then appendBlock existing wrapped
       else
         let afterBegin = T.drop (T.length managedBeginMarker) fromBegin
             (_, fromEnd) = T.breakOn managedEndMarker afterBegin
         in if T.null fromEnd
              then appendBlock (T.stripEnd preBegin) wrapped
              else
                let afterEnd = T.drop (T.length managedEndMarker) fromEnd
                in preBegin <> wrapped <> afterEnd
  where
    appendBlock base wrapped =
      if T.null (T.stripEnd base)
        then wrapped <> "\n"
        else T.stripEnd base <> "\n\n" <> wrapped <> "\n"

-- =====================================================================
-- Ensure directories + config
-- =====================================================================

-- | Create ~/.haskclaw and ~/.haskclaw/chats, and ensure the shared CLAUDE.md
--   contains the scheduler managed block.
ensureHaskclawDirs :: IO ()
ensureHaskclawDirs = do
  home  <- haskclawHome
  chats <- chatsDir
  createDirectoryIfMissing True home
  createDirectoryIfMissing True chats
  upsertSharedClaudeMd

upsertSharedClaudeMd :: IO ()
upsertSharedClaudeMd = do
  path <- sharedClaudeMdPath
  existing <- readTextOr "" path
  let updated = upsertManagedBlock defaultSharedClaudeMd existing
  when (existing /= updated) $
    atomicWriteText path updated

ensureChatDir :: ChatId -> IO FilePath
ensureChatDir cid = do
  path <- chatDir cid
  createDirectoryIfMissing True path
  pure path

-- | Ensure the chat project has both:
--     * .mcp.json  — Claude CLI reads this to spawn the haskclaw MCP server.
--     * .claude/settings.json  — permission entry for the MCP tools.
--
--   Runs on every incoming message so updates to the binary path or permissions
--   propagate without manual deletion.
ensureChatConfig :: ChatId -> IO FilePath
ensureChatConfig cid = do
  configDir <- chatConfigDir cid
  createDirectoryIfMissing True configDir
  mcpPath <- mcpBinaryPath
  mcpJsonPath <- chatMcpJsonPath cid
  settingsPath <- chatSettingsPath cid
  dir <- chatDir cid
  writeMcpJson cid mcpPath mcpJsonPath
  upsertSettings dir settingsPath
  pure configDir

-- | Overwrite .mcp.json with the current haskclaw registration. We rewrite
--   unconditionally so the binary path stays aligned with the running exe.
writeMcpJson :: ChatId -> FilePath -> FilePath -> IO ()
writeMcpJson cid mcpPath path = do
  let encoded = encode (buildMcpJson cid mcpPath)
  current <- readBytesOr "" path
  when (current /= encoded) $ atomicWriteBytes path encoded

upsertSettings :: FilePath -> FilePath -> IO ()
upsertSettings dir path = do
  existing <- readJsonOr (object []) path
  let merged  = mergeHaskclawSettings dir existing
      encoded = encode merged
  current <- readBytesOr "" path
  when (current /= encoded) $ atomicWriteBytes path encoded

-- =====================================================================
-- File helpers
-- =====================================================================

readBytesOr :: LByteString -> FilePath -> IO LByteString
readBytesOr fallback path = do
  exists <- doesFileExist path
  if exists then LBS.readFile path else pure fallback

readTextOr :: Text -> FilePath -> IO Text
readTextOr fallback path = do
  exists <- doesFileExist path
  if exists then TIO.readFile path else pure fallback

readJsonOr :: Value -> FilePath -> IO Value
readJsonOr fallback path = do
  bytes <- readBytesOr "" path
  if LBS.null bytes
    then pure fallback
    else case eitherDecode bytes of
      Right v -> pure v
      Left _  -> pure fallback

atomicWriteBytes :: FilePath -> LByteString -> IO ()
atomicWriteBytes path bs = do
  let tmp = path <> ".tmp"
  LBS.writeFile tmp bs
  renameFile tmp path

atomicWriteText :: FilePath -> Text -> IO ()
atomicWriteText path t = do
  let tmp = path <> ".tmp"
  TIO.writeFile tmp t
  renameFile tmp path
