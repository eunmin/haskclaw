module Haskclaw.Unit.Util.TelegramHtmlSpec (spec) where

import Relude

import Test.Hspec (Spec, describe, it, shouldBe)

import Haskclaw.Util.TelegramHtml (escapeHtml, toTelegramHtml)

spec :: Spec
spec = do
  describe "escapeHtml" $ do
    it "escapes the HTML metacharacters &, <, > and leaves other text alone" $
      escapeHtml "a < b & c > d" `shouldBe` "a &lt; b &amp; c &gt; d"

    it "leaves Korean text untouched" $
      escapeHtml "스케줄 정보" `shouldBe` "스케줄 정보"

    it "preserves quotes and apostrophes" $
      escapeHtml "\"hi\" 'there'" `shouldBe` "\"hi\" 'there'"

  describe "toTelegramHtml — plain text" $ do
    it "returns text without markers unchanged" $
      toTelegramHtml "hello world" `shouldBe` "hello world"

    it "escapes <, >, & in surrounding text" $
      toTelegramHtml "1 < 2 && 3 > 0" `shouldBe` "1 &lt; 2 &amp;&amp; 3 &gt; 0"

  describe "toTelegramHtml — bold" $ do
    it "wraps **x** in <b>...</b>" $
      toTelegramHtml "hi **bold** end" `shouldBe` "hi <b>bold</b> end"

    it "leaves an unclosed ** as a literal pair" $
      toTelegramHtml "x **bold" `shouldBe` "x **bold"

    it "escapes HTML metacharacters inside bold" $
      toTelegramHtml "**a<b>**" `shouldBe` "<b>a&lt;b&gt;</b>"

  describe "toTelegramHtml — italic with single asterisk" $ do
    it "wraps *x* in <i>...</i>" $
      toTelegramHtml "an *italic* word" `shouldBe` "an <i>italic</i> word"

    it "does not italicise a bullet marker like '* item'" $
      toTelegramHtml "* item one\n* item two"
        `shouldBe` "* item one\n* item two"

    it "does not italicise when the closing * is preceded by a space" $
      toTelegramHtml "*hello *world" `shouldBe` "*hello *world"

  describe "toTelegramHtml — inline code" $ do
    it "wraps `x` in <code>...</code>" $
      toTelegramHtml "use `cat file` here"
        `shouldBe` "use <code>cat file</code> here"

    it "escapes HTML metacharacters inside inline code" $
      toTelegramHtml "`<script>`"
        `shouldBe` "<code>&lt;script&gt;</code>"

    it "leaves an unclosed backtick as a literal" $
      toTelegramHtml "`oops" `shouldBe` "`oops"

  describe "toTelegramHtml — fenced code blocks" $ do
    it "renders a language-tagged fence as <pre><code class=\"language-...\">" $
      toTelegramHtml "```haskell\nmain = putStrLn \"hi\"\n```"
        `shouldBe`
          "<pre><code class=\"language-haskell\">main = putStrLn \"hi\"</code></pre>"

    it "renders a fence with no language tag as a bare <pre>" $
      toTelegramHtml "```\nplain block\n```"
        `shouldBe` "<pre>plain block</pre>"

    it "ignores markdown markers inside a fence" $
      toTelegramHtml "```\n**not bold** *not italic* `not code`\n```"
        `shouldBe`
          "<pre>**not bold** *not italic* `not code`</pre>"

    it "escapes HTML metacharacters inside the fence body" $
      toTelegramHtml "```\nif (a < b && c > 0)\n```"
        `shouldBe` "<pre>if (a &lt; b &amp;&amp; c &gt; 0)</pre>"

  describe "toTelegramHtml — links" $ do
    it "renders [text](https://...) as <a href=\"...\">text</a>" $
      toTelegramHtml "see [docs](https://example.com/x)"
        `shouldBe` "see <a href=\"https://example.com/x\">docs</a>"

    it "drops links whose URL scheme Telegram would reject" $
      toTelegramHtml "open [file](/etc/passwd)"
        `shouldBe` "open [file](/etc/passwd)"

    it "renders tg:// links as a Telegram-friendly anchor" $
      toTelegramHtml "[user](tg://user?id=42)"
        `shouldBe` "<a href=\"tg://user?id=42\">user</a>"

    it "treats an unclosed [ as literal" $
      toTelegramHtml "before [oops"
        `shouldBe` "before [oops"

  describe "toTelegramHtml — ATX headers" $ do
    it "wraps a single-hash header in <b>" $
      toTelegramHtml "# Title\n" `shouldBe` "<b>Title</b>\n"

    it "wraps a header with no trailing newline" $
      toTelegramHtml "# Title" `shouldBe` "<b>Title</b>"

    it "wraps headers up to level 6, all as <b>" $ do
      toTelegramHtml "## Two\n"     `shouldBe` "<b>Two</b>\n"
      toTelegramHtml "###### Six\n" `shouldBe` "<b>Six</b>\n"

    it "leaves seven or more hashes as literal text" $
      toTelegramHtml "####### too deep\n"
        `shouldBe` "####### too deep\n"

    it "requires a space after the hashes" $
      toTelegramHtml "#hello\n" `shouldBe` "#hello\n"

    it "does not turn a mid-line # into a header" $
      toTelegramHtml "see issue #42 today"
        `shouldBe` "see issue #42 today"

    it "recognises a header on a non-first line" $
      toTelegramHtml "intro\n## Section\nbody"
        `shouldBe` "intro\n<b>Section</b>\nbody"

    it "ignores # lines inside fenced code blocks" $
      toTelegramHtml "```bash\n# this is a comment\n```"
        `shouldBe`
          "<pre><code class=\"language-bash\"># this is a comment</code></pre>"

    it "strips the optional CommonMark trailing # run" $
      toTelegramHtml "## Title ##\n" `shouldBe` "<b>Title</b>\n"

    it "drops headers with empty bodies" $
      toTelegramHtml "###   \n" `shouldBe` "###   \n"

  describe "toTelegramHtml — Claude scheduler reply (regression)" $
    it "round-trips the schedule confirmation message into valid HTML" $ do
      let input = mconcat
            [ "메일 확인 스케줄이 등록되었습니다!\n\n"
            , "**스케줄 정보:**\n"
            , "- **작업 ID**: task-1777287087-79d5\n"
            , "- **실행 시간**: 매일 오후 8시 (20:00)\n"
            , "- **Cron 표현식**: `0 20 * * *`\n"
            , "- **작업 내용**: mail-checker 스킬을 사용하여 Gmail과 네이버 메일 확인"
            ]
          expected = mconcat
            [ "메일 확인 스케줄이 등록되었습니다!\n\n"
            , "<b>스케줄 정보:</b>\n"
            , "- <b>작업 ID</b>: task-1777287087-79d5\n"
            , "- <b>실행 시간</b>: 매일 오후 8시 (20:00)\n"
            , "- <b>Cron 표현식</b>: <code>0 20 * * *</code>\n"
            , "- <b>작업 내용</b>: mail-checker 스킬을 사용하여 Gmail과 네이버 메일 확인"
            ]
      toTelegramHtml input `shouldBe` expected
