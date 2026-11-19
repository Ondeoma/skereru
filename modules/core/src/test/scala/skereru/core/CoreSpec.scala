package skereru.core

import skereru.shared.*

import java.net.InetAddress
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.time.{Instant, ZoneId}

class CoreSpec extends munit.FunSuite {

  private def bytes(s: String) = s.replace("\n", "\r\n").getBytes(UTF_8)

  private val multipart = bytes(
    """From: =?UTF-8?B?5bGx55SwIOWkqumDjg==?= <taro@example.com>
      |To: hanako@example.jp, "Doe, John" <john@example.org>
      |Subject: =?ISO-2022-JP?B?GyRCJEYkOSRIGyhC?=
      |Date: Mon, 16 Nov 2026 10:00:00 +0900
      |Message-ID: <abc@example.com>
      |MIME-Version: 1.0
      |Content-Type: multipart/mixed; boundary="XYZ"
      |
      |preamble
      |--XYZ
      |Content-Type: multipart/alternative; boundary=ALT
      |
      |--ALT
      |Content-Type: text/plain; charset=utf-8
      |Content-Transfer-Encoding: quoted-printable
      |
      |=E3=81=93=E3=82=93=E3=81=AB=E3=81=A1=E3=81=AF
      |--ALT
      |Content-Type: text/html; charset=utf-8
      |
      |<p>hi<script>alert(1)</script><img src="https://evil.example/t.png"></p>
      |--ALT--
      |--XYZ
      |Content-Type: application/pdf; name="a.pdf"
      |Content-Disposition: attachment; filename*=UTF-8''%E8%A6%8B%E7%A9%8D.pdf
      |Content-Transfer-Encoding: base64
      |
      |SGVsbG8=
      |--XYZ--
      |""".stripMargin
  )

  test("MIME: 構造と部品番号、文字コード・転送符号化の復号") {
    val root = Mime.parse(multipart)
    assert(root.isMultipart)
    assertEquals(root.children.size, 2)
    assertEquals(root.headerDecoded("From"), Some("山田 太郎 <taro@example.com>"))
    assertEquals(root.headerDecoded("Subject"), Some("てすと"))
    val plain = root.part(List(1, 1)).get
    assertEquals(plain.mimeType, "text/plain")
    assertEquals(Mime.text(multipart, plain), "こんにちは")
    val pdf = root.part(List(2)).get
    assertEquals(pdf.filename, Some("見積.pdf"))
    assert(pdf.isAttachment)
    assertEquals(new String(Mime.content(multipart, pdf), UTF_8), "Hello")
    assertEquals(root.leaves().map(_._1), List(List(1, 1), List(1, 2), List(2)))
  }

  test("IMAP: BODYSTRUCTURE・ENVELOPE・BODY[1.1]・HEADER.FIELDS") {
    val root = Mime.parse(multipart)
    val bs = ImapFormat.bodyStructure(multipart, root, extended = true)
    assert(bs.startsWith("(((\"text\" \"plain\" (\"CHARSET\" \"utf-8\")"), bs)
    assert(bs.contains("\"mixed\" (\"BOUNDARY\" \"XYZ\")"), bs)
    val env = ImapFormat.envelope(root)
    assert(env.contains("(\"Doe, John\" NIL \"john\" \"example.org\")"), env)
    assertEquals(
      new String(ImapFormat.sectionBytes(multipart, root, Section(List(1, 1), "", Nil)), UTF_8),
      "=E3=81=93=E3=82=93=E3=81=AB=E3=81=A1=E3=81=AF"
    )
    val hf = new String(
      ImapFormat.sectionBytes(multipart, root, Section(Nil, "HEADER.FIELDS", List("subject", "date"))),
      ISO_8859_1
    )
    assertEquals(hf, "Subject: =?ISO-2022-JP?B?GyRCJEYkOSRIGyhC?=\r\nDate: Mon, 16 Nov 2026 10:00:00 +0900\r\n\r\n")
    val mime = new String(ImapFormat.sectionBytes(multipart, root, Section(List(2), "MIME", Nil)), ISO_8859_1)
    assert(mime.startsWith("Content-Type: application/pdf"), mime)
  }

  test("IMAP: 単一部品のメッセージの BODY[1] は本文") {
    val m = bytes("Subject: x\n\nbody\n")
    val root = Mime.parse(m)
    assertEquals(new String(ImapFormat.sectionBytes(m, root, Section(List(1), "", Nil)), UTF_8), "body\r\n")
    assertEquals(new String(ImapFormat.sectionBytes(m, root, Section(Nil, "TEXT", Nil)), UTF_8), "body\r\n")
  }

  test("IMAP: コマンドの解析（リテラル・FETCH・SEARCH）") {
    val r = new ImapReader(
      "a1 UID FETCH 1:*,5 (FLAGS BODY.PEEK[HEADER.FIELDS (FROM SUBJECT)]<0.100> BODYSTRUCTURE)\r\n".getBytes(UTF_8)
    )
    assertEquals(r.tag(), "a1"); r.sp(); assertEquals(r.atom().toUpperCase, "UID"); r.sp(); r.atom(); r.sp()
    val set = r.seqSet()
    assert(set.contains(3, 10) && set.contains(10, 10) && !set.contains(11, 10))
    r.sp()
    val atts = r.fetchAtts()
    assertEquals(atts.size, 3)
    assertEquals(
      atts(1),
      FetchAtt.BodySection(true, Section(Nil, "HEADER.FIELDS", List("FROM", "SUBJECT")), Some((0L, 100L)))
    )
    val lit = new ImapReader("a2 LOGIN {4}\r\nuser \"pa\\\"ss\"\r\n".getBytes(UTF_8))
    lit.tag(); lit.sp(); lit.atom(); lit.sp()
    assertEquals(lit.astring(), "user"); lit.sp(); assertEquals(lit.astring(), "pa\"ss"); lit.end()
    val s = new ImapReader("OR FROM taro SUBJECT \"見積\" UNSEEN SINCE 1-jan-2026".getBytes(UTF_8)).searchKeys()
    assertEquals(
      s,
      SearchKey.And(List(
        SearchKey.Or(SearchKey.HeaderContains("From", "taro"), SearchKey.HeaderContains("Subject", "見積")),
        SearchKey.Flag("\\Seen", false),
        SearchKey.Since(java.time.LocalDate.of(2026, 1, 1))
      ))
    )
    intercept[ImapSyntax](new ImapReader("FOO".getBytes(UTF_8)).searchKeys())
  }

  test("修正 UTF-7 のメールボックス名") {
    assertEquals(MUtf7.encode("仕事/請求書"), "&TtVOiw-/&istsQmb4-")
    assertEquals(MUtf7.decode(MUtf7.encode("仕事/請求書 & A")), "仕事/請求書 & A")
    assertEquals(MUtf7.encode("A&B"), "A&-B")
  }

  test("アドレスの解析（表示名・グループ・コメント）") {
    val l = Address.parseList("\"Yamada, Taro\" <taro@Example.COM>, team: a@x.jp, b@y.jp;, c@z.jp (C さん)")
    assertEquals(l.map(_.address), List("taro@example.com", "a@x.jp", "b@y.jp", "c@z.jp"))
    assertEquals(l.head.nameDecoded, Some("Yamada, Taro"))
    assertEquals(l.last.nameDecoded, Some("C さん"))
    assert(Address.valid("a.b+tag@example.co.jp"))
    assert(!Address.valid("a..b@example.com") && !Address.valid("a@localhost") && !Address.valid("<a@b.com>"))
    assertEquals(Address.withoutTag("taro+news@example.com"), "taro@example.com")
  }

  /** 決まった応答を返す DNS。 */
  final class FakeDns(txts: Map[String, List[String]],
                      as: Map[String, List[String]] = Map.empty,
                      mxs: Map[String, List[(Int, String)]] = Map.empty) extends Dns {
    def txt(name: String) = Right(txts.getOrElse(name.toLowerCase, Nil))
    def addresses(name: String) = Right(as.getOrElse(name.toLowerCase, Nil).map(InetAddress.getByName))
    def mx(name: String) = Right(mxs.getOrElse(name.toLowerCase, Nil))
    def ptr(ip: InetAddress) = Right(Nil)
  }

  test("SPF: ip4・include・mx・redirect・softfail・ループの上限") {
    val dns = FakeDns(
      Map(
        "example.com" -> List("v=spf1 ip4:192.0.2.0/24 include:_spf.example.net mx -all"),
        "_spf.example.net" -> List("v=spf1 ip6:2001:db8::/32 ~all"),
        "redirect.example" -> List("v=spf1 redirect=example.com"),
        "loop.example" -> List("v=spf1 include:loop.example -all")
      ),
      as = Map("mx1.example.com" -> List("198.51.100.7")),
      mxs = Map("example.com" -> List(10 -> "mx1.example.com"))
    )
    val spf = new Spf(dns)
    def ip(s: String) = InetAddress.getByName(s)
    assertEquals(spf.check(ip("192.0.2.55"), "a@example.com", "h"), SpfResult.Pass)
    assertEquals(spf.check(ip("2001:db8::1"), "a@example.com", "h"), SpfResult.Pass)
    assertEquals(spf.check(ip("198.51.100.7"), "a@example.com", "h"), SpfResult.Pass)
    assertEquals(spf.check(ip("203.0.113.1"), "a@example.com", "h"), SpfResult.Fail)
    assertEquals(spf.check(ip("203.0.113.1"), "a@redirect.example", "h"), SpfResult.Fail)
    assertEquals(spf.check(ip("192.0.2.1"), "a@redirect.example", "h"), SpfResult.Pass)
    assertEquals(spf.check(ip("192.0.2.1"), "a@none.example", "h"), SpfResult.None)
    assertEquals(spf.check(ip("192.0.2.1"), "a@loop.example", "h"), SpfResult.PermError)
  }

  test("DKIM: 署名して検証できる。本文を変えると fail") {
    val (priv, pub) = Dkim.generate()
    val dns = FakeDns(Map("s1._domainkey.example.com" -> List(Dkim.dnsRecord(pub))))
    val msg = Compose.crlf(bytes(
      "From: Taro <taro@example.com>\nTo: b@example.org\nSubject: hello\n  folded\nDate: Mon, 16 Nov 2026 10:00:00 +0900\nMessage-ID: <x@example.com>\n\nbody  text \n\n\n"
    ))
    val sig = Dkim.sign(msg, "example.com", "s1", Dkim.privateKey(priv), Instant.now())
    val signed = sig.getBytes(ISO_8859_1) ++ msg
    val res = new Dkim(dns).verify(signed, Instant.now())
    assertEquals(res.map(_.result), List("pass"), res.toString)
    val tampered = new String(signed, ISO_8859_1).replace("body  text", "body text!").getBytes(ISO_8859_1)
    assertEquals(new Dkim(dns).verify(tampered, Instant.now()).map(_.result), List("fail"))
    // relaxed の本文の正規化（行末の空白・末尾の空行）
    assertEquals(new String(new Dkim(dns).canonBody(bytes(" a  b \n\n\n"), "relaxed"), UTF_8), " a b\r\n")
    assertEquals(new String(new Dkim(dns).canonBody(Array.emptyByteArray, "simple"), UTF_8), "\r\n")
  }

  test("DMARC: 整合（relaxed）と policy") {
    val dns = FakeDns(Map("_dmarc.example.com" -> List("v=DMARC1; p=reject; sp=quarantine")))
    val dmarc = new Dmarc(dns)
    assertEquals(dmarc.evaluate("example.com", SpfResult.Pass, "bounce.example.com", Nil).result, "pass")
    assertEquals(
      dmarc.evaluate("example.com", SpfResult.Pass, "other.org", Nil),
      DmarcResult("fail", "reject", "example.com")
    )
    assertEquals(dmarc.evaluate("news.example.com", SpfResult.Fail, "x.org", Nil).policy, "quarantine")
    assertEquals(
      dmarc.evaluate(
        "example.com",
        SpfResult.Fail,
        "x.org",
        List(DkimResult("mail.example.com", "s", "pass", ""))
      ).result,
      "pass"
    )
    assertEquals(OrgDomain.of("a.b.example.co.jp"), "example.co.jp")
  }

  test("迷惑メールの規則: 危険な添付・なりすまし") {
    val m = bytes("From: CEO <ceo@ourcorp.jp>\nSubject: URGENT!!!\n\nhi\n")
    val in = SpamInput(
      m,
      Mime.parse(m),
      None,
      "pc",
      None,
      "x@evil.example",
      SpfResult.Fail,
      Nil,
      DmarcResult("none", "none", "ourcorp.jp"),
      Nil,
      None,
      claimsLocalDomain = true,
      virus = None,
      now = Instant.now()
    )
    val v = SpamRules.evaluate(in, SpamThresholds())
    assert(v.hits.exists(_.rule == "SPOOFED_LOCAL_DOMAIN"), v.summary)
    assertEquals(v.action, SpamAction.Reject)
    val clean = bytes("From: a@example.com\nTo: b@example.jp\nSubject: 会議\nDate: " +
      Dates.rfc5322(Instant.now(), ZoneId.of("Asia/Tokyo")) +
      "\nMessage-ID: <1@example.com>\n\n明日の会議の件です。\n")
    val ok = SpamRules.evaluate(
      in.copy(
        msg = clean,
        root = Mime.parse(clean),
        helo = "mail.example.com",
        reverseName = Some("mail.example.com"),
        spf = SpfResult.Pass,
        claimsLocalDomain = false
      ),
      SpamThresholds()
    )
    assertEquals(ok.action, SpamAction.Deliver, ok.summary)
  }

  test("ベイズ: 学習した語で判定できる") {
    val spamTok = Bayes.tokens(
      bytes("Subject: 激安 バイアグラ\n\n今すぐ購入 casino bonus"),
      Mime.parse(bytes("Subject: 激安 バイアグラ\n\n今すぐ購入 casino bonus"))
    )
    assert(spamTok.contains("casino") && spamTok.contains("s:激安") && spamTok.contains("購入") &&
      !spamTok.contains("s:casino"))
    val counts = Map("casino" -> (40, 1), "bonus" -> (35, 2), "会議" -> (0, 40), "資料" -> (1, 30))
    assert(Bayes.classify(
      counts.filter((k, _) => Set("casino", "bonus")(k)),
      50,
      50
    ).exists(_ > 0.9))
    assert(Bayes.classify(
      counts.filter((k, _) => Set("会議", "資料")(k)),
      50,
      50
    ).exists(_ < 0.1))
    assertEquals(Bayes.classify(counts, 10, 10), None)
  }

  test("HTML の無害化: script・外部画像・イベント属性") {
    val (h, remote) = Html.sanitize(
      """<p onclick="x()">hi<script>alert(1)</script><img src="https://t.example/p.gif"><img src="cid:logo1"><a href="javascript:alert(1)">x</a></p>""",
      cid => Some(s"/cid/$cid"),
      allowRemote = false
    )
    assert(remote)
    assert(!h.contains("script") && !h.contains("onclick") && !h.contains("javascript:") && !h.contains("t.example"), h)
    assert(h.contains("/cid/logo1"), h)
    assertEquals(Html.toText("<p>a<br>b</p><ul><li>x</li></ul>"), "a\nb\n・x")
  }

  test("振り分けの規則") {
    val m = bytes("From: news@shop.example\nTo: me@example.jp\nList-Id: <sale.shop.example>\nSubject: セールのお知らせ\n\n本文\n")
    val rule = FilterRuleDto(
      java.util.UUID.randomUUID(),
      "ショップ",
      true,
      true,
      List(
        RuleConditionDto(RuleField.From, RuleOp.EndsWith, "@shop.example", None),
        RuleConditionDto(RuleField.Subject, RuleOp.Contains, "セール", None)
      ),
      List(
        RuleActionDto(RuleAction.Move, Some("買い物")),
        RuleActionDto(RuleAction.MarkRead, None),
        RuleActionDto(RuleAction.Stop, None)
      ),
      0
    )
    val other =
      rule.copy(id = java.util.UUID.randomUUID(), position = 1, actions = List(RuleActionDto(RuleAction.Flag, None)))
    val o = Rules.apply(List(rule, other), m, Mime.parse(m), Nil)
    assertEquals(o.moveTo, Some("買い物"))
    assert(o.markRead && !o.flag)
  }

  test("検索の構文") {
    val q = MailQuery.parse("from:taro subject:\"月次 報告\" is:unread has:attachment larger:2M after:2026/01/01 請求書")
    assertEquals(q.from, List("taro")); assertEquals(q.subject, List("月次 報告"))
    assertEquals(q.unread, Some(true)); assertEquals(q.hasAttachment, Some(true))
    assertEquals(q.larger, Some(2L * 1024 * 1024)); assertEquals(q.words, List("請求書"))
    assertEquals(q.after, Some(java.time.LocalDate.of(2026, 1, 1)))
  }

  test("組み立てたメッセージを解析できる（日本語の件名・添付・返信のヘッダー）") {
    val o = Outgoing(
      MailAddress(Some("山田"), "taro", "example.com"),
      List(MailAddress(None, "b", "example.org")),
      Nil,
      "見積書の送付",
      "本文です",
      Some("<p>本文です</p>"),
      List(Attachment("見積書.pdf", "application/pdf", "PDF".getBytes(UTF_8))),
      Some("<p@x>"),
      List("<p@x>")
    )
    val raw = Compose.build(o, "<id@example.com>", Instant.now(), ZoneId.of("Asia/Tokyo"))
    val root = Mime.parse(raw)
    assertEquals(root.headerDecoded("Subject"), Some("見積書の送付"))
    assertEquals(root.header("Message-ID"), Some("<id@example.com>"))
    assertEquals(root.header("In-Reply-To"), Some("<p@x>"))
    assert(!new String(raw, ISO_8859_1).contains("Bcc"))
    val leaves = root.leaves().map(_._2)
    assertEquals(leaves.flatMap(_.filename), List("見積書.pdf"))
    assertEquals(Mime.text(raw, leaves.find(_.mimeType == "text/plain").get).trim, "本文です")
    assertEquals(Compose.replySubject("Re: x"), "Re: x")
    assertEquals(Compose.baseSubject("Re: Fwd: 返信: 見積"), "見積")
  }

  test("ヘッダーの符号化と復号（分割されたマルチバイト・生の UTF-8）") {
    val enc = Mime.encodeHeader("とても長い日本語の件名をきちんと符号化して復号できることを確かめるための文字列です")
    assertEquals(Mime.decodeHeader(enc.replace("\r\n", "")), "とても長い日本語の件名をきちんと符号化して復号できることを確かめるための文字列です")
    assertEquals(Mime.decodeHeader(new String("件名".getBytes(UTF_8), ISO_8859_1)), "件名")
    assertEquals(Mime.decodeHeader("=?UTF-8?Q?a_b?= =?UTF-8?Q?c?="), "a bc")
  }

  test("セキュリティ: From と揃っていない SPF の pass では、自ドメインのなりすましの判定を外さない") {
    val m = bytes("From: CEO <ceo@ourcorp.jp>\nTo: a@ourcorp.jp\nSubject: x\nMessage-ID: <1@x>\nDate: " +
      Dates.rfc5322(Instant.now(), ZoneId.of("Asia/Tokyo")) + "\n\nhi\n")
    val base = SpamInput(
      m,
      Mime.parse(m),
      None,
      "mail.attacker.example",
      Some("mail.attacker.example"),
      "x@attacker.example",
      SpfResult.Pass,
      Nil,
      DmarcResult("none", "none", "ourcorp.jp"),
      Nil,
      None,
      claimsLocalDomain = true,
      virus = None,
      now = Instant.now()
    )
    assert(SpamRules.evaluate(base, SpamThresholds()).hits.exists(_.rule == "SPOOFED_LOCAL_DOMAIN"))
    val aligned = base.copy(mailFrom = "bounce@ourcorp.jp")
    assert(!SpamRules.evaluate(aligned, SpamThresholds()).hits.exists(_.rule == "SPOOFED_LOCAL_DOMAIN"))
  }

  test("セキュリティ: DKIM の l= で署名の後ろに足された本文は pass にしない") {
    val (priv, pub) = Dkim.generate()
    val dns = FakeDns(Map("s1._domainkey.example.com" -> List(Dkim.dnsRecord(pub))))
    val msg = Compose.crlf(bytes("From: a@example.com\nSubject: s\n\nbody\n"))
    val sig = Dkim.sign(msg, "example.com", "s1", Dkim.privateKey(priv), Instant.now())
    // l= を付けた署名に作り直す代わりに、署名の後ろに本文を足したものの検証で、l= のない署名は fail になることを確かめる
    val appended = sig.getBytes(ISO_8859_1) ++ msg ++ "added\r\n".getBytes(ISO_8859_1)
    assertEquals(new Dkim(dns).verify(appended, Instant.now()).map(_.result), List("fail"))
  }

  test("セキュリティ: CSS のエスケープ・image-set で外部の画像を読ませない") {
    val (h, _) = Html.sanitize(
      """<div style="background:u\72l(https://t.example/a.png)">x</div><p style="background-image:image-set('https://t.example/b.png' 1x)">y</p>""",
      _ => None,
      allowRemote = false
    )
    assert(!h.contains("t.example"), h)
  }

  test("宛先の書き方とメールボックス名の検査（画面と共通）") {
    assertEquals(MailText.quoted(Some("Doe, \"J\""), "j@example.com"), "\"Doe, J\" <j@example.com>")
    assertEquals(MailText.quoted(None, "j@example.com"), "j@example.com")
    assert(MailText.validMailboxName("仕事/請求"))
    assert(!MailText.validMailboxName("INBOX") && !MailText.validMailboxName("a//b") && !MailText.validMailboxName("*"))
  }

  /** 時間のかかりすぎ（計算が二乗になる正規表現など）を検出する。 */
  private def fast[A](label: String, ms: Long = 2000)(f: => A): A = {
    val t = System.nanoTime(); val r = f
    val took = (System.nanoTime() - t) / 1_000_000
    assert(took < ms, s"$label に ${took}ms かかった")
    r
  }

  test("セキュリティ: 改行や空白・閉じない url( を並べた入力で、処理が遅くならない") {
    val dkim = new Dkim(FakeDns(Map.empty))
    val body = ("\r\n" * 200_000 + "x").getBytes(ISO_8859_1)
    assertEquals(new String(fast("DKIM の本文の正規化")(dkim.canonBody(body, "simple")), ISO_8859_1).length, 200_000 * 2 + 3)
    assertEquals(new String(dkim.canonBody("a\r\n\r\n\r\n".getBytes(ISO_8859_1), "simple"), ISO_8859_1), "a\r\n")
    fast("HTML のテキスト化")(Html.toText("<p>" + "<b> </b>" * 50_000 + "x</p>"))
    val (h, _) = fast("CSS の url の除去")(Html.sanitize(
      "<p style=\"background:" + "url(" * 50_000 + "https://t.example/a\">x</p>",
      _ => None,
      allowRemote = false
    ))
    assert(!h.contains("t.example"), h.take(200))
  }

  test("セキュリティ: 区切りだけを並べたメッセージでも、部品の数は上限まで") {
    val m = ("Content-Type: multipart/mixed; boundary=b\r\n\r\n" + "--b\r\n" * 100_000).getBytes(ISO_8859_1)
    val root = fast("MIME の解析")(Mime.parse(m))
    assert(root.leaves().size <= Mime.MaxParts + 1, root.leaves().size.toString)
  }

  test("セキュリティ: From の末尾のドットや複数の From で、自ドメインのなりすましの判定を外さない") {
    assertEquals(Address.parseOne("CEO <ceo@ourcorp.jp.>").map(_.domain), Some("ourcorp.jp"))
    assertEquals(Address.parseOne("ceo@ourcorp.jp;").map(_.domain), Some("ourcorp.jp"))
    val date = Dates.rfc5322(Instant.now(), ZoneId.of("Asia/Tokyo"))
    def eval(head: String) = {
      val m = bytes(head + "To: a@ourcorp.jp\nSubject: x\nMessage-ID: <1@x>\nDate: " + date + "\n\nhi\n")
      SpamRules.evaluate(
        SpamInput(
          m,
          Mime.parse(m),
          None,
          "mail.attacker.example",
          Some("mail.attacker.example"),
          "x@attacker.example",
          SpfResult.Pass,
          Nil,
          DmarcResult("none", "none", "attacker.example"),
          Nil,
          None,
          claimsLocalDomain = true,
          virus = None,
          now = Instant.now()
        ),
        SpamThresholds()
      ).hits.map(_.rule)
    }
    // 認証された attacker.example の陰に、自ドメインを紛れ込ませる
    assert(eval("From: x@attacker.example, ceo@ourcorp.jp\n").contains("SPOOFED_LOCAL_DOMAIN"))
    val two = eval("From: x@attacker.example\nFrom: ceo@ourcorp.jp\n")
    assert(two.contains("FROM_MULTIPLE_HEADERS") && two.contains("SPOOFED_LOCAL_DOMAIN"), two.toString)
    assert(eval("From: x@attacker.example\nSubject: 至急\n").contains("DUPLICATE_HEADERS"))
  }

  test("セキュリティ: 組織ドメインは Public Suffix List で決める") {
    assertNotEquals(OrgDomain.of("evil.tokyo.jp"), OrgDomain.of("shop.tokyo.jp"))
    assertEquals(OrgDomain.of("mail.shop.tokyo.jp"), "shop.tokyo.jp")
    assertEquals(OrgDomain.of("a.b.example.co.jp"), "example.co.jp")
    assertEquals(OrgDomain.of("mail.demo.example"), "demo.example")
  }

  test("セキュリティ: 戻り先は同じサイトのパスだけ・IMAP の範囲の数に上限") {
    assert(SafePath.isLocal("/mail/inbox"))
    assert(!SafePath.isLocal("//evil.example") && !SafePath.isLocal("/\\evil.example") &&
      !SafePath.isLocal("https://evil.example") && !SafePath.isLocal("/a\nb"))
    intercept[ImapSyntax](SeqSet.parse((1 to 5000).mkString(",")))
  }

  test("取り込み: Gmail のラベル・IMAP のフォルダの置き場所") {
    assertEquals(
      MailImport.gmailLabels("Inbox,Starred,\"a,b\",=?UTF-8?B?5LuV5LqL?="),
      List("Inbox", "Starred", "a,b", "仕事")
    )
    val p = MailImport.gmailPlacement(List("Inbox", "Starred", "Unread", "Important", "Category Updates", "仕事/案件"))
    assertEquals(p.targets, List(ImportTarget.Special(SpecialUse.Inbox), ImportTarget.Folder("仕事/案件")))
    assert(p.flagged && !p.seen)
    assertEquals(
      MailImport.gmailPlacement(List("Opened", "Archived")).targets,
      List(ImportTarget.Special(SpecialUse.Archive))
    )
    assertEquals(MailImport.gmailPlacement(List("Inbox", "Spam")).targets, List(ImportTarget.Special(SpecialUse.Junk)))
    assertEquals(MailImport.gmailPlacement(List("Chat")).targets, Nil)
    assertEquals(
      MailImport.imapFolder("[Gmail]/All Mail", '/', Set("\\All", "\\HasNoChildren")),
      ImapFolderPlan.AllMail
    )
    assertEquals(
      MailImport.imapFolder("[Gmail]/Sent Mail", '/', Set("\\Sent")),
      ImapFolderPlan.Into(ImportTarget.Special(SpecialUse.Sent))
    )
    assertEquals(MailImport.imapFolder("[Gmail]/Starred", '/', Set("\\Flagged")), ImapFolderPlan.Skip)
    assertEquals(
      MailImport.imapFolder("Work.Projects", '.', Set.empty),
      ImapFolderPlan.Into(ImportTarget.Folder("Work/Projects"))
    )
    // 使えない名前は「取り込み/…」に
    assertEquals(MailImport.folderName("INBOX", '/'), "取り込み/INBOX")
    assertEquals(MailImport.folderName("a*b", '/'), "ab")
  }

  test("招待: iCalendar を読み、返事（REPLY）を組み立てる") {
    val ics = """BEGIN:VCALENDAR
                |METHOD:REQUEST
                |BEGIN:VTIMEZONE
                |TZID:Asia/Tokyo
                |END:VTIMEZONE
                |BEGIN:VEVENT
                |UID:abc-123@example.org
                |DTSTART;TZID=Asia/Tokyo:20261120T100000
                |DTEND;TZID=Asia/Tokyo:20261120T110000
                |SUMMARY:定例\, 週次の打ち合わせ
                |LOCATION:会議室 A
                |ORGANIZER;CN="Sato, Hanako":mailto:sato@example.org
                |ATTENDEE;CN=山田;PARTSTAT=NEEDS-ACTION:mailto:Yamada@Example.org
                |SEQUENCE:2
                |BEGIN:VALARM
                |TRIGGER:-PT10M
                |END:VALARM
                |END:VEVENT
                |END:VCALENDAR""".stripMargin
    val inv = Ical.parse(ics, ZoneId.of("UTC")).get
    assertEquals(inv.method, "REQUEST"); assertEquals(inv.summary, "定例, 週次の打ち合わせ")
    assertEquals(inv.start, IcalTime.At(Instant.parse("2026-11-20T01:00:00Z")))
    assertEquals(inv.organizer.map(o => (o.address, o.name)), Some(("sato@example.org", Some("Sato, Hanako"))))
    assertEquals(inv.attendees.map(_.address), List("yamada@example.org"))
    val r = Ical.reply(inv, "yamada@example.org", Some("山田 太郎"), "ACCEPTED", Instant.parse("2026-11-17T00:00:00Z"))
    assert(
      r.contains("METHOD:REPLY") && r.contains("PARTSTAT=ACCEPTED") && r.contains("UID:abc-123@example.org") &&
        r.contains("SEQUENCE:2"),
      r
    )
    assert(r.split("\r\n").forall(_.getBytes(UTF_8).length <= 76), r)
    // 終日・DURATION
    val day = Ical.parse(
      "BEGIN:VCALENDAR\nBEGIN:VEVENT\nUID:d\nDTSTART;VALUE=DATE:20261201\nDURATION:P2D\nEND:VEVENT\nEND:VCALENDAR",
      ZoneId.of("Asia/Tokyo")
    ).get
    assertEquals(day.end, IcalTime.Day(java.time.LocalDate.parse("2026-12-03")))
    assert(Ical.parse("BEGIN:VCALENDAR\nEND:VCALENDAR", ZoneId.of("UTC")).isEmpty)
  }

  test("書き出し: mbox の1通（ラベル・>From のエスケープ・改行）") {
    val raw = "Subject: x\r\n\r\nline\r\nFrom here\r\n>From there\r\n".getBytes(UTF_8)
    val e = new String(
      MailExport.entry(
        raw,
        MailExport.labels(Some(SpecialUse.Inbox), "INBOX", seen = false, flagged = true),
        Instant.EPOCH
      ),
      UTF_8
    )
    assert(e.startsWith("From skereru@localhost Thu Jan 01 00:00:00 1970\nX-Gmail-Labels: Inbox,Starred,Unread\n"), e)
    assert(e.contains("\n>From here\n>>From there\n") && !e.contains("\r"), e)
    assertEquals(MailExport.labelHeader(List("a,b", "c")), "\"a,b\",c")
    assertEquals(MailImport.gmailLabels(MailExport.labelHeader(List("a,b", "c"))), List("a,b", "c"))
  }
}
