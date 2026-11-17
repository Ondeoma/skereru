package skereru.core

import java.net.InetAddress

/** 迷惑メールの判定に使う材料（受信時に集めたもの）。 */
final case class SpamInput(
    msg: Array[Byte],
    root: MimePart,
    ip: Option[InetAddress],
    helo: String,
    reverseName: Option[String],
    mailFrom: String,
    spf: SpfResult,
    dkim: List[DkimResult],
    dmarc: DmarcResult,
    dnsblHits: List[String],
    bayes: Option[Double],
    claimsLocalDomain: Boolean,
    virus: Option[String],
    now: java.time.Instant
)

final case class SpamHit(rule: String, score: Double, description: String)

enum SpamAction:
  case Deliver, Junk, Reject

final case class SpamVerdict(score: Double, hits: List[SpamHit], action: SpamAction):
  def summary: String = hits.map(h => f"${h.rule}=${h.score}%.1f").mkString(" ")

final case class SpamThresholds(junk: Double = 5.0, reject: Double = 15.0)

/** 規則による点数づけ（SpamAssassin と同じ考え方。正の点ほど迷惑メールらしい）。 */
object SpamRules:

  private val DangerousExt = Set("exe", "scr", "pif", "com", "bat", "cmd", "vbs", "vbe", "js", "jse", "wsf", "wsh", "hta", "cpl", "msi", "msp",
    "jar", "lnk", "reg", "ps1", "iso", "img", "vhd", "vhdx", "one", "chm", "application", "gadget", "msc", "scf", "inf", "xll", "appx")
  private val MacroExt = Set("docm", "xlsm", "pptm", "dotm", "xltm", "potm", "ppam", "xlam", "sldm")
  private val ArchiveExt = Set("zip", "rar", "7z", "gz", "tgz", "cab", "ace", "arj", "lzh")

  /** From のすべてのアドレス（From ヘッダーが複数あれば、そのすべて）。 */
  def fromAddresses(root: MimePart): List[MailAddress] = root.headerAll("From").flatMap(Address.parseList)

  def evaluate(in: SpamInput, th: SpamThresholds): SpamVerdict =
    val hits = scala.collection.mutable.ListBuffer.empty[SpamHit]
    def hit(rule: String, score: Double, d: String) = hits += SpamHit(rule, score, d)
    val root = in.root
    val from = root.header("From").flatMap(Address.parseOne)
    val fromDomain = from.map(_.domain).getOrElse("")
    val subject = root.headerDecoded("Subject").getOrElse("")

    // From の形（複数の From ヘッダーや複数のアドレスは、判定するアドレスと表示されるアドレスをずらす手口）
    val froms = fromAddresses(root)
    if root.headerAll("From").length > 1 then hit("FROM_MULTIPLE_HEADERS", 6.0, "From ヘッダーが複数ある")
    else if froms.length > 1 then hit("FROM_MULTIPLE_ADDRESSES", 2.5, "From に複数のアドレスがある")
    if froms.isEmpty || froms.exists(a => !Address.validDomain(a.domain)) then hit("FROM_INVALID", 3.0, "From のアドレスが正しくない")
    // 1つしか書けないヘッダーの重複（DKIM の署名は下のヘッダーで通り、画面には上のヘッダーが出る。署名済みのメールの上に足して内容を偽る手口）
    val dupes = List("Subject", "Date", "To", "Cc", "Reply-To", "Sender", "Message-ID").filter(h => root.headerAll(h).length > 1)
    if dupes.nonEmpty then hit("DUPLICATE_HEADERS", 5.0, s"1つだけのはずのヘッダーが複数ある: ${dupes.mkString(", ")}")

    // ウイルス（ClamAV）
    in.virus.foreach(v => hit("VIRUS", 100, s"ウイルスを検出: $v"))

    // 送信ドメイン認証
    in.spf match
      case SpfResult.Fail     => hit("SPF_FAIL", 3.0, "SPF が fail")
      case SpfResult.SoftFail => hit("SPF_SOFTFAIL", 1.5, "SPF が softfail")
      case SpfResult.PermError => hit("SPF_PERMERROR", 0.5, "SPF のレコードが正しくない")
      case SpfResult.Pass     => hit("SPF_PASS", -0.5, "SPF が pass")
      case _ => ()
    if in.dkim.exists(_.pass) then hit("DKIM_VALID", -0.5, "DKIM の署名が正しい")
    if in.dkim.exists(_.result == "fail") && !in.dkim.exists(_.pass) then hit("DKIM_INVALID", 1.5, "DKIM の署名が一致しない")
    in.dmarc.result match
      case "fail" =>
        in.dmarc.policy match
          case "reject"     => hit("DMARC_REJECT", 8.0, "DMARC が fail（ポリシーは reject）")
          case "quarantine" => hit("DMARC_QUARANTINE", 5.5, "DMARC が fail（ポリシーは quarantine）")
          case _            => hit("DMARC_FAIL_NONE", 1.0, "DMARC が fail（ポリシーは none）")
      case "pass" => hit("DMARC_PASS", -1.0, "DMARC が pass")
      case _ => ()
    // 自分たちのドメインを名乗る、認証されていない外からのメール（なりすまし）
    // From が複数あるときは、そのすべてが認証されていること（認証された1つの陰に、自ドメインを紛れ込ませる手口があるため）
    def aligned(d: String) =
      in.dkim.exists(r => r.pass && OrgDomain.of(r.domain) == OrgDomain.of(d)) ||
        (in.spf == SpfResult.Pass && in.mailFrom.nonEmpty && OrgDomain.of(Address.domainOf(in.mailFrom)) == OrgDomain.of(d))
    if in.claimsLocalDomain && !(froms.nonEmpty && froms.forall(a => aligned(a.domain))) then
      hit("SPOOFED_LOCAL_DOMAIN", 10.0, "このサーバーのドメインを名乗っているが、認証されていない")

    // 送信元のホスト
    in.dnsblHits.foreach(z => hit("RCVD_IN_DNSBL", 4.0, s"送信元が DNSBL に載っている: $z"))
    if in.ip.isDefined && in.reverseName.isEmpty then hit("RDNS_NONE", 1.0, "送信元に逆引きの名前がない")
    if in.helo.isEmpty || (!in.helo.contains('.') && !in.helo.startsWith("[")) then hit("HELO_NOT_FQDN", 1.0, "HELO が完全な名前ではない")
    if in.helo.matches("""^\d+\.\d+\.\d+\.\d+$""") then hit("HELO_BARE_IP", 1.5, "HELO が IP アドレスそのもの")

    // ヘッダー
    if root.header("Message-ID").isEmpty then hit("MISSING_MID", 1.0, "Message-ID がない")
    root.header("Date") match
      case None => hit("MISSING_DATE", 1.5, "Date がない")
      case Some(d) =>
        Dates.parseRfc5322(d).foreach { t =>
          if t.isAfter(in.now.plusSeconds(24 * 3600)) then hit("DATE_IN_FUTURE", 2.0, "Date が未来")
          if t.isBefore(in.now.minusSeconds(30L * 24 * 3600)) then hit("DATE_IN_PAST", 1.0, "Date が30日以上前")
        }
    if from.isEmpty then hit("MISSING_FROM", 3.0, "From がない")
    from.flatMap(_.nameDecoded).foreach { n =>
      // 表示名に別のアドレスを書いて信用させる
      val m = """[A-Za-z0-9._%+-]+@([A-Za-z0-9.-]+\.[A-Za-z]{2,})""".r.findFirstMatchIn(n)
      if m.exists(x => !x.group(1).equalsIgnoreCase(fromDomain)) then hit("FROM_NAME_HAS_OTHER_ADDR", 2.5, "表示名に別のドメインのアドレスがある")
    }
    if subject.trim.isEmpty then hit("MISSING_SUBJECT", 0.5, "件名がない")
    val letters = subject.filter(_.isLetter)
    if letters.length >= 10 && letters.forall(c => !c.isLower) && letters.exists(_.isUpper) && letters.forall(_ < 0x80) then hit("SUBJ_ALL_CAPS", 1.0, "件名がすべて大文字")
    if subject.count(c => c == '!' || c == '！') >= 3 then hit("SUBJ_EXCLAIM", 0.8, "件名に感嘆符が多い")
    if root.header("Reply-To").flatMap(Address.parseOne).exists(r => fromDomain.nonEmpty && OrgDomain.of(r.domain) != OrgDomain.of(fromDomain)) &&
      !in.dkim.exists(_.pass) then hit("REPLYTO_OTHER_DOMAIN", 0.8, "Reply-To が From と別のドメイン（未署名）")
    if root.header("To").isEmpty && root.header("Cc").isEmpty then hit("MISSING_TO", 0.8, "To も Cc もない")

    // 本文・添付
    val leaves = root.leaves().map(_._2)
    val attachments = leaves.filter(_.isAttachment)
    val htmlOnly = leaves.exists(_.mimeType == "text/html") && !leaves.exists(_.mimeType == "text/plain")
    if htmlOnly then hit("HTML_ONLY", 0.5, "HTML だけで、テキストの本文がない")
    attachments.flatMap(_.filename).foreach { f =>
      val ext = f.toLowerCase.split('.').toList
      val last = ext.lastOption.getOrElse("")
      if DangerousExt(last) then hit("DANGEROUS_ATTACHMENT", 8.0, s"実行形式の添付ファイル: $f")
      else if MacroExt(last) then hit("MACRO_ATTACHMENT", 3.0, s"マクロつきの Office 文書: $f")
      if ext.size >= 3 && Set("pdf", "doc", "docx", "xls", "xlsx", "jpg", "png", "txt")(ext(ext.size - 2)) && (DangerousExt(last) || ArchiveExt(last)) then
        hit("DOUBLE_EXTENSION", 3.0, s"二重の拡張子: $f")
    }
    if attachments.exists(a => a.filename.exists(f => ArchiveExt(f.toLowerCase.split('.').last))) && in.dkim.forall(!_.pass) then
      hit("ARCHIVE_UNSIGNED", 0.7, "未署名のメールに圧縮ファイル")

    val bodyText = leaves.filter(p => p.mainType == "text").take(3).map(p => Mime.text(in.msg, p)).mkString("\n").take(200_000)
    val urls = Urls.find(bodyText)
    if urls.exists(u => """https?://\d+\.\d+\.\d+\.\d+""".r.findPrefixOf(u).isDefined) then hit("URI_NUMERIC_IP", 1.5, "IP アドレスの URL がある")
    if urls.exists(u => Urls.Shorteners.exists(s => u.toLowerCase.contains("//" + s + "/"))) then hit("URI_SHORTENER", 0.8, "短縮 URL がある")
    if bodyText.toLowerCase.contains("unsubscribe") || bodyText.contains("配信停止") then
      if root.header("List-Unsubscribe").isEmpty then hit("UNSUB_NO_HEADER", 0.5, "配信停止の案内があるが List-Unsubscribe がない")

    // ベイズ
    in.bayes.foreach { p =>
      if p >= 0.99 then hit("BAYES_99", 4.5, f"ベイズ分類: ${p * 100}%.0f%%")
      else if p >= 0.95 then hit("BAYES_95", 3.0, f"ベイズ分類: ${p * 100}%.0f%%")
      else if p >= 0.80 then hit("BAYES_80", 2.0, f"ベイズ分類: ${p * 100}%.0f%%")
      else if p >= 0.60 then hit("BAYES_60", 1.0, f"ベイズ分類: ${p * 100}%.0f%%")
      else if p <= 0.01 then hit("BAYES_00", -2.0, f"ベイズ分類: ${p * 100}%.0f%%")
      else if p <= 0.05 then hit("BAYES_05", -1.0, f"ベイズ分類: ${p * 100}%.0f%%")
    }

    val score = math.round(hits.map(_.score).sum * 10) / 10.0
    val action =
      if in.virus.isDefined || score >= th.reject then SpamAction.Reject
      else if score >= th.junk then SpamAction.Junk
      else SpamAction.Deliver
    SpamVerdict(score, hits.toList, action)

/** URL の抜き出し。 */
object Urls:
  val Shorteners = Set("bit.ly", "tinyurl.com", "t.co", "goo.gl", "ow.ly", "is.gd", "buff.ly", "rebrand.ly", "cutt.ly", "shorturl.at", "x.gd")
  private val Re = """https?://[^\s<>"')\]]+""".r
  def find(text: String): List[String] = Re.findAllIn(text).take(500).toList
  def host(u: String): Option[String] = scala.util.Try(java.net.URI.create(u).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase)

/** ベイズ分類（トークンの数え上げと、Robinson-Fisher の結合）。 */
object Bayes:

  /** 学習・判定に使うトークン（件名・本文の語、日本語は2文字ずつ、URL のホスト、送信者のドメイン）。 */
  def tokens(msg: Array[Byte], root: MimePart): Set[String] =
    val subject = root.headerDecoded("Subject").getOrElse("")
    val from = root.header("From").flatMap(Address.parseOne)
    val texts = root.leaves().map(_._2).filter(_.mainType == "text").take(3).map { p =>
      val t = Mime.text(msg, p)
      if p.subType == "html" then Html.toText(t) else t
    }.mkString("\n").take(100_000)
    val out = scala.collection.mutable.Set.empty[String]
    def words(s: String, prefix: String): Unit =
      // 英数字の語（3〜20文字）
      """[\p{L}\p{N}][\p{L}\p{N}'_.-]*""".r.findAllIn(s.toLowerCase).foreach { w =>
        val ascii = w.filter(_ < 0x80)
        if ascii.length == w.length && w.length >= 3 && w.length <= 20 then out += prefix + w
      }
      // 日本語・中国語などは2文字ずつ
      val cjk = s.filter(c => Character.UnicodeScript.of(c.toInt) match
        case Character.UnicodeScript.HAN | Character.UnicodeScript.HIRAGANA | Character.UnicodeScript.KATAKANA | Character.UnicodeScript.HANGUL => true
        case _ => c == '\n')
      cjk.split('\n').foreach(seg => seg.sliding(2).filter(_.length == 2).foreach(b => out += prefix + b))
    words(subject, "s:")
    words(texts, "")
    Urls.find(texts).flatMap(Urls.host).foreach(h => out += "url:" + h)
    from.foreach(f => out += "from:" + f.domain)
    root.header("X-Mailer").orElse(root.header("User-Agent")).foreach(m => out += "ua:" + m.toLowerCase.takeWhile(c => c != '/' && c != ' ').take(30))
    out.toSet.take(3000)

  /** トークンごとの数（spam・ham）と学習した通数から、迷惑メールである確率を出す。学習が足りなければ None。 */
  def classify(counts: Map[String, (Int, Int)], nSpam: Int, nHam: Int): Option[Double] =
    if nSpam < 30 || nHam < 30 then None
    else
      val s = 1.0; val x = 0.5
      val fs = counts.toList.flatMap { (_, sh) =>
        val (sp, hm) = sh
        val n = sp + hm
        if n == 0 then None
        else
          val ps = sp.toDouble / nSpam; val ph = hm.toDouble / nHam
          val pw = if ps + ph == 0 then 0.5 else ps / (ps + ph)
          Some((s * x + n * pw) / (s + n))
      }.filter(f => math.abs(f - 0.5) > 0.1).sortBy(f => -math.abs(f - 0.5)).take(150)
      if fs.isEmpty then Some(0.5)
      else
        val n = fs.size
        val h = chi2Q(-2 * fs.map(f => math.log(math.max(1e-200, 1 - f))).sum, 2 * n)
        val sp = chi2Q(-2 * fs.map(f => math.log(math.max(1e-200, f))).sum, 2 * n)
        // H が 1 に近いほど迷惑メールらしい（Robinson の指標）
        Some((1 + (1 - h) - (1 - sp)) / 2)

  /** カイ二乗分布の上側確率（自由度は偶数）。 */
  private def chi2Q(x2: Double, v: Int): Double =
    val m = x2 / 2
    var sum = math.exp(-m)
    var term = sum
    var i = 1
    while i < v / 2 do
      term *= m / i
      sum += term
      i += 1
    math.min(sum, 1.0)

/** 日時の解析（Date ヘッダー）。 */
object Dates:
  private val fmt = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME

  def parseRfc5322(raw: String): Option[java.time.Instant] =
    val s = raw.replaceAll("""\([^)]*\)""", "").replaceAll("\\s+", " ").trim
    // 曜日なし・秒なし・2桁の年・タイムゾーン名などの揺れを吸収する
    val noDow = s.replaceFirst("^[A-Za-z]{3},\\s*", "")
    val candidates = List(s, noDow, noDow.replaceFirst("(\\d{1,2}:\\d{2})(\\s)", "$1:00$2"))
    candidates.view.flatMap { c =>
      scala.util.Try(java.time.ZonedDateTime.parse(c, fmt).toInstant).toOption
        .orElse(scala.util.Try(java.time.ZonedDateTime.parse(c, java.time.format.DateTimeFormatter.ofPattern("d MMM yyyy HH:mm:ss Z", java.util.Locale.ENGLISH)).toInstant).toOption)
        .orElse(scala.util.Try(java.time.ZonedDateTime.parse(c.replaceAll(" (UT|GMT|UTC|Z)$", " +0000").replaceAll(" JST$", " +0900").replaceAll(" EST$", " -0500")
          .replaceAll(" EDT$", " -0400").replaceAll(" PST$", " -0800").replaceAll(" PDT$", " -0700"),
          java.time.format.DateTimeFormatter.ofPattern("d MMM yyyy HH:mm:ss Z", java.util.Locale.ENGLISH)).toInstant).toOption)
    }.headOption

  def rfc5322(t: java.time.Instant, zone: java.time.ZoneId): String = fmt.format(t.atZone(zone))
