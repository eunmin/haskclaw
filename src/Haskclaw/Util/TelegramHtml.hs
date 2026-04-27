module Haskclaw.Util.TelegramHtml
  ( toTelegramHtml
  , escapeHtml
  ) where

import Relude

import Data.Char (isAlphaNum)
import qualified Data.Text as T

-- | Convert a subset of Markdown (used by Claude) to HTML compatible with
--   Telegram's @parse_mode=HTML@.
--
--   Supports:
--
--     * fenced code blocks @```lang\\n...\\n```@
--     * inline code @\`x\`@
--     * bold @**x**@
--     * italic @*x*@ (only when @x@ is non-empty and has no leading/trailing
--       whitespace, so bullet markers like @* item@ stay literal)
--     * ATX headers @# … ###### …@ at line start, all rendered as @<b>@
--       (Telegram HTML has no header tags)
--     * links @[text](url)@
--
--   All other characters are HTML-escaped. Unclosed markers are emitted as
--   their literal characters. The parser is non-recursive, so markdown
--   nested inside another marker (e.g. backticks inside bold) is rendered
--   as literal text rather than a nested HTML tag — Telegram's HTML mode
--   does not allow nesting anyway.
toTelegramHtml :: Text -> Text
toTelegramHtml = T.concat . parse True

-- | The 'Bool' tracks whether we are currently positioned at the start of
--   a line, which is required to recognise ATX headers without misfiring on
--   @#@ characters that appear mid-paragraph.
parse :: Bool -> Text -> [Text]
parse _ t | T.null t = []
parse atStart t
  | atStart, Just (body, rest) <- splitHeader t =
      ["<b>", escapeHtml body, "</b>"] <> parse False rest
  | "```" `T.isPrefixOf` t = parseFence (T.drop 3 t)
  | "**"  `T.isPrefixOf` t = parseBold  (T.drop 2 t)
  | "`"   `T.isPrefixOf` t = parseInlineCode (T.drop 1 t)
  | "*"   `T.isPrefixOf` t = parseItalic (T.drop 1 t)
  | "["   `T.isPrefixOf` t = parseLink (T.drop 1 t)
  | "\n"  `T.isPrefixOf` t = "\n" : parse True (T.drop 1 t)
  | otherwise =
      let (plain, rest) = T.break isMarker t
       in escapeHtml plain : parse False rest

isMarker :: Char -> Bool
isMarker c = c == '`' || c == '*' || c == '[' || c == '\n'

-- | Recognise an ATX header @#{1,6}<space>…@. Returns the body of the
--   current line (with surrounding whitespace and an optional trailing
--   @#…@ run stripped, per CommonMark) and the remainder of the input
--   starting at the line break. Empty-titled headers are rejected so the
--   parser does not emit an empty @<b></b>@.
splitHeader :: Text -> Maybe (Text, Text)
splitHeader t = do
  let (hashes, afterHashes) = T.span (== '#') t
      level = T.length hashes
  guard (level >= 1 && level <= 6)
  case T.uncons afterHashes of
    Just (c, rest1) | c == ' ' || c == '\t' -> do
      let lineRest = T.dropWhile isSpaceOrTab rest1
          (line, after) = T.break (== '\n') lineRest
          body = T.stripEnd (dropTrailingHashes (T.stripEnd line))
      guard (not (T.null body))
      pure (body, after)
    _ -> Nothing

isSpaceOrTab :: Char -> Bool
isSpaceOrTab c = c == ' ' || c == '\t'

-- | CommonMark allows headers to be terminated by an optional run of @#@
--   characters preceded by whitespace, e.g. @## Title ##@. Strip that
--   trailing run so the rendered title is just @Title@.
dropTrailingHashes :: Text -> Text
dropTrailingHashes line =
  let withoutTrailingHashes = T.dropWhileEnd (== '#') line
   in if T.length withoutTrailingHashes < T.length line
        && (T.null withoutTrailingHashes
            || isSpaceOrTab (T.last withoutTrailingHashes))
        then withoutTrailingHashes
        else line

splitOn :: Text -> Text -> Maybe (Text, Text)
splitOn needle t = case T.breakOn needle t of
  (_, rest) | T.null rest -> Nothing
  (body, rest) -> Just (body, T.drop (T.length needle) rest)

parseFence :: Text -> [Text]
parseFence t = case splitOn "```" t of
  Nothing -> escapeHtml "```" : parse False t
  Just (raw, after) ->
    let (lang, body) = splitFenceLang raw
        tag = if T.null lang
                then ["<pre>", escapeHtml body, "</pre>"]
                else [ "<pre><code class=\"language-"
                     , escapeAttr lang, "\">"
                     , escapeHtml body
                     , "</code></pre>"
                     ]
     in tag <> parse False after

splitFenceLang :: Text -> (Text, Text)
splitFenceLang raw = case T.uncons raw of
  Just ('\n', rest) -> ("", trimTrailingNewlines rest)
  _ -> case T.breakOn "\n" raw of
    (_, "") -> ("", trimTrailingNewlines raw)
    (firstLine, rest)
      | isLangToken firstLine -> (firstLine, trimTrailingNewlines (T.drop 1 rest))
      | otherwise -> ("", trimTrailingNewlines raw)

trimTrailingNewlines :: Text -> Text
trimTrailingNewlines = T.dropWhileEnd (== '\n')

isLangToken :: Text -> Bool
isLangToken s =
  not (T.null s) && T.length s < 32 && T.all langChar s
  where
    langChar c = isAlphaNum c || c == '+' || c == '-' || c == '_' || c == '#' || c == '.'

parseBold :: Text -> [Text]
parseBold t = case splitOn "**" t of
  Nothing -> escapeHtml "**" : parse False t
  Just (body, rest)
    | T.null body -> escapeHtml "**" : parse False t
    | otherwise -> ["<b>", escapeHtml body, "</b>"] <> parse False rest

parseInlineCode :: Text -> [Text]
parseInlineCode t = case splitOn "`" t of
  Nothing -> escapeHtml "`" : parse False t
  Just (body, rest)
    | T.null body -> escapeHtml "`" : parse False t
    | otherwise -> ["<code>", escapeHtml body, "</code>"] <> parse False rest

parseItalic :: Text -> [Text]
parseItalic t = case T.uncons t of
  Nothing -> [escapeHtml "*"]
  Just (c, _)
    | isSpaceChar c -> escapeHtml "*" : parse False t
  _ -> case splitOn "*" t of
    Nothing -> escapeHtml "*" : parse False t
    Just (body, rest)
      | T.null body -> escapeHtml "*" : parse False t
      | isSpaceChar (T.last body) -> escapeHtml "*" : parse False t
      | otherwise -> ["<i>", escapeHtml body, "</i>"] <> parse False rest

parseLink :: Text -> [Text]
parseLink t = case splitOn "]" t of
  Nothing -> escapeHtml "[" : parse False t
  Just (linkText, afterClose) -> case T.uncons afterClose of
    Just ('(', afterParen) -> case splitOn ")" afterParen of
      Nothing -> escapeHtml "[" : parse False t
      Just (url, after)
        | not (isSafeUrl url) -> escapeHtml "[" : parse False t
        | otherwise ->
            [ "<a href=\""
            , escapeAttr (T.strip url)
            , "\">"
            , escapeHtml linkText
            , "</a>"
            ] <> parse False after
    _ -> escapeHtml "[" : parse False t

-- | Telegram only accepts @http@, @https@, and @tg@ link schemes inside
--   @<a>@ tags. Anything else (including bare paths) makes Telegram reject
--   the entire message, so we drop those links back to plain text.
isSafeUrl :: Text -> Bool
isSafeUrl url =
  let u = T.toLower (T.strip url)
   in "http://" `T.isPrefixOf` u
        || "https://" `T.isPrefixOf` u
        || "tg://" `T.isPrefixOf` u

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

escapeHtml :: Text -> Text
escapeHtml = T.concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc c   = T.singleton c

escapeAttr :: Text -> Text
escapeAttr = T.concatMap esc
  where
    esc '&' = "&amp;"
    esc '"' = "&quot;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc c   = T.singleton c
