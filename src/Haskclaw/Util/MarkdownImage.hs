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
  let (raw, imgs) = go input
  in (normalize raw, imgs)
  where
    go txt = case T.breakOn "![" txt of
      (pre, rest) | T.null rest -> (pre, [])
                  | otherwise -> case parseHead rest of
                      Just (img, remainder) ->
                        let (tailTxt, tailImgs) = go remainder
                        in (mergeFragments pre tailTxt, img : tailImgs)
                      Nothing ->
                        let (tailTxt, tailImgs) = go (T.drop 2 rest)
                        in (pre <> "![" <> tailTxt, tailImgs)

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

-- | Rejoin the text fragments around a removed image. If both sides were only
--   separated by inline whitespace (spaces/tabs), leave a single space so
--   surrounding words don't collide. At a newline boundary, let
--   'collapseNewlines' handle spacing instead.
mergeFragments :: Text -> Text -> Text
mergeFragments pre suf =
  let preStrip = T.dropWhileEnd isSpaceNotNl pre
      sufStrip = T.dropWhile isSpaceNotNl suf
      preHadSpace = T.length pre > T.length preStrip
      sufHadSpace = T.length suf > T.length sufStrip
      preAtBoundary = T.null preStrip || T.isSuffixOf "\n" preStrip
      sufAtBoundary = T.null sufStrip || T.isPrefixOf "\n" sufStrip
  in if preAtBoundary || sufAtBoundary
       then preStrip <> sufStrip
       else if preHadSpace || sufHadSpace
              then preStrip <> " " <> sufStrip
              else preStrip <> sufStrip

isSpaceNotNl :: Char -> Bool
isSpaceNotNl c = Char.isSpace c && c /= '\n'

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
