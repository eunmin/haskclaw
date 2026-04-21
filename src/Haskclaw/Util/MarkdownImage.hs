module Haskclaw.Util.MarkdownImage
  ( InlineImage (..)
  , extractImages
  , isLocalImagePath
  ) where

import Relude

import qualified Data.Char as Char
import qualified Data.Text as T

data InlineImage = InlineImage
  { caption :: Maybe Text
  , path    :: FilePath
  } deriving (Eq, Show)

-- | Scan the text for Markdown image references of the form
--   @![caption](/abs/path.png)@ where the URL is an absolute local path with a
--   supported image extension. Remote URLs (http/https) and relative paths are
--   left as-is. Returns the cleaned text (matches removed, surrounding
--   whitespace tidied) and the list of inline images in the order encountered.
extractImages :: Text -> (Text, [InlineImage])
extractImages input =
  let (pieces, imgs) = goPieces input
  in (normalize (T.concat pieces), imgs)

goPieces :: Text -> ([Text], [InlineImage])
goPieces txt = case T.breakOn "![" txt of
  (pre, rest) | T.null rest -> ([pre], [])
              | otherwise -> case parseHead rest of
                  Just (img, remainder) ->
                    let (morePieces, moreImgs) = goPieces remainder
                    in (stripTrailingInline pre : stripLeadingInline morePieces, img : moreImgs)
                  Nothing ->
                    let (morePieces, moreImgs) =
                          goPieces (T.drop 2 rest) -- skip past the "!["
                    in (pre <> "![" : morePieces, moreImgs)

-- | Glue @'stripTrailingInline'@ output to the start of a list without losing
--   elements. The first piece of the tail has its leading inline space removed.
stripLeadingInline :: [Text] -> [Text]
stripLeadingInline [] = []
stripLeadingInline (p : ps) = T.dropWhile isSpaceNotNl p : ps

stripTrailingInline :: Text -> Text
stripTrailingInline = T.dropWhileEnd isSpaceNotNl

isSpaceNotNl :: Char -> Bool
isSpaceNotNl c = Char.isSpace c && c /= '\n'

-- | Parse a single @![alt](url)@ starting at the beginning of the input.
--   Returns the parsed image plus the remainder after it.
parseHead :: Text -> Maybe (InlineImage, Text)
parseHead input = do
  afterBang <- T.stripPrefix "![" input
  (alt, afterAlt) <- splitOnChar ']' afterBang
  afterBracket <- T.stripPrefix "(" afterAlt
  (url, afterUrl) <- splitOnChar ')' afterBracket
  let trimmed = T.strip url
  guard (isLocalImagePath trimmed)
  let captionText = T.strip alt
      cap = if T.null captionText then Nothing else Just captionText
  pure (InlineImage cap (toString trimmed), afterUrl)

-- | Split at the first occurrence of @c@, disallowing newlines in the prefix.
--   Returns (before, after) with the delimiter consumed.
splitOnChar :: Char -> Text -> Maybe (Text, Text)
splitOnChar c t =
  let (before, rest) = T.break (\ch -> ch == c || ch == '\n') t
  in case T.uncons rest of
    Just (ch, after) | ch == c -> Just (before, after)
    _                          -> Nothing

-- | Local absolute path to a supported image format.
isLocalImagePath :: Text -> Bool
isLocalImagePath t =
  let lower = T.toLower t
      hasExt = any (`T.isSuffixOf` lower) supportedExtensions
      isAbs  = "/" `T.isPrefixOf` t
      isRemote = any (`T.isPrefixOf` lower) ["http://", "https://", "data:"]
  in isAbs && hasExt && not isRemote

supportedExtensions :: [Text]
supportedExtensions = [".png", ".jpg", ".jpeg", ".webp", ".gif"]

-- | Normalize the final text: trim trailing whitespace on lines and collapse
--   three or more consecutive newlines down to two.
normalize :: Text -> Text
normalize =
  T.stripEnd
    . collapseNewlines
    . T.unlines
    . fmap T.stripEnd
    . T.lines

collapseNewlines :: Text -> Text
collapseNewlines t = case T.breakOn "\n\n\n" t of
  (pre, rest) | T.null rest -> pre
              | otherwise ->
                  pre <> "\n\n" <> collapseNewlines (T.dropWhile (== '\n') rest)
