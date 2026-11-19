package skereru.core

/** 規則による点数づけ（SpamAssassin と同じ考え方。正の点ほど迷惑メールらしい）。 */
object SpamRules {

  private val DangerousExt = Set(
    "exe",
    "scr",
    "pif",
    "com",
    "bat",
    "cmd",
    "vbs",
    "vbe",
    "js",
    "jse",
    "wsf",
    "wsh",
    "hta",
    "cpl",
    "msi",
    "msp",
    "jar",
    "lnk",
    "reg",
    "ps1",
    "iso",
    "img",
    "vhd",
    "vhdx",
    "one",
    "chm",
    "application",
    "gadget",
    "msc",
    "scf",
    "inf",
    "xll",
    "appx"
  )
  private val MacroExt = Set("docm", "xlsm", "pptm", "dotm", "xltm", "potm", "ppam", "xlam", "sldm")
  private val ArchiveExt = Set("zip", "rar", "7z", "gz", "tgz", "cab", "ace", "arj", "lzh")

  /** From のすべてのアドレス（From ヘッダーが複数あれば、そのすべて）。 */
  def fromAddresses(root: MimePart): List[MailAddress] = root.headerAll("From").flatMap(Address.parseList)

  def evaluate(in: SpamInput, th: SpamThresholds): SpamVerdict = {
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
    if froms.isEmpty || froms.exists(a => !Address.validDomain(a.domain)) then
      hit("FROM_INVALID", 3.0, "From のアドレスが正しくない")
    // 1つしか書けないヘッダーの重複（DKIM の署名は下のヘッダーで通り、画面には上のヘッダーが出る。署名済みのメールの上に足して内容を偽る手口）
    val dupes =
      List("Subject", "Date", "To", "Cc", "Reply-To", "Sender", "Message-ID").filter(h => root.headerAll(h).length > 1)
    if dupes.nonEmpty then hit("DUPLICATE_HEADERS", 5.0, s"1つだけのはずのヘッダーが複数ある: ${dupes.mkString(", ")}")

    // ウイルス（ClamAV）
    in.virus.foreach(v => hit("VIRUS", 100, s"ウイルスを検出: $v"))

    // 送信ドメイン認証
    in.spf match {
      case SpfResult.Fail => hit("SPF_FAIL", 3.0, "SPF が fail")
      case SpfResult.SoftFail => hit("SPF_SOFTFAIL", 1.5, "SPF が softfail")
      case SpfResult.PermError => hit("SPF_PERMERROR", 0.5, "SPF のレコードが正しくない")
      case SpfResult.Pass => hit("SPF_PASS", -0.5, "SPF が pass")
      case _ => ()
    }
    if in.dkim.exists(_.pass) then hit("DKIM_VALID", -0.5, "DKIM の署名が正しい")
    if in.dkim.exists(_.result == "fail") && !in.dkim.exists(_.pass) then hit("DKIM_INVALID", 1.5, "DKIM の署名が一致しない")
    in.dmarc.result match {
      case "fail" =>
        in.dmarc.policy match {
          case "reject" => hit("DMARC_REJECT", 8.0, "DMARC が fail（ポリシーは reject）")
          case "quarantine" => hit("DMARC_QUARANTINE", 5.5, "DMARC が fail（ポリシーは quarantine）")
          case _ => hit("DMARC_FAIL_NONE", 1.0, "DMARC が fail（ポリシーは none）")
        }
      case "pass" => hit("DMARC_PASS", -1.0, "DMARC が pass")
      case _ => ()
    }
    // 自分たちのドメインを名乗る、認証されていない外からのメール（なりすまし）
    // From が複数あるときは、そのすべてが認証されていること（認証された1つの陰に、自ドメインを紛れ込ませる手口があるため）
    def aligned(d: String) =
      in.dkim.exists(r => r.pass && OrgDomain.of(r.domain) == OrgDomain.of(d)) ||
        (in.spf == SpfResult.Pass && in.mailFrom.nonEmpty &&
          OrgDomain.of(Address.domainOf(in.mailFrom)) == OrgDomain.of(d))
    if in.claimsLocalDomain && !(froms.nonEmpty && froms.forall(a => aligned(a.domain))) then
      hit("SPOOFED_LOCAL_DOMAIN", 10.0, "このサーバーのドメインを名乗っているが、認証されていない")

    // 送信元のホスト
    in.dnsblHits.foreach(z => hit("RCVD_IN_DNSBL", 4.0, s"送信元が DNSBL に載っている: $z"))
    if in.ip.isDefined && in.reverseName.isEmpty then hit("RDNS_NONE", 1.0, "送信元に逆引きの名前がない")
    if in.helo.isEmpty || (!in.helo.contains('.') && !in.helo.startsWith("[")) then
      hit("HELO_NOT_FQDN", 1.0, "HELO が完全な名前ではない")
    if in.helo.matches("""^\d+\.\d+\.\d+\.\d+$""") then hit("HELO_BARE_IP", 1.5, "HELO が IP アドレスそのもの")

    // ヘッダー
    if root.header("Message-ID").isEmpty then hit("MISSING_MID", 1.0, "Message-ID がない")
    root.header("Date") match {
      case None => hit("MISSING_DATE", 1.5, "Date がない")
      case Some(d) =>
        Dates.parseRfc5322(d).foreach { t =>
          if t.isAfter(in.now.plusSeconds(24 * 3600)) then hit("DATE_IN_FUTURE", 2.0, "Date が未来")
          if t.isBefore(in.now.minusSeconds(30L * 24 * 3600)) then hit("DATE_IN_PAST", 1.0, "Date が30日以上前")
        }
    }
    if from.isEmpty then hit("MISSING_FROM", 3.0, "From がない")
    from.flatMap(_.nameDecoded).foreach { n =>
      // 表示名に別のアドレスを書いて信用させる
      val m = """[A-Za-z0-9._%+-]+@([A-Za-z0-9.-]+\.[A-Za-z]{2,})""".r.findFirstMatchIn(n)
      if m.exists(x => !x.group(1).equalsIgnoreCase(fromDomain)) then
        hit("FROM_NAME_HAS_OTHER_ADDR", 2.5, "表示名に別のドメインのアドレスがある")
    }
    if subject.trim.isEmpty then hit("MISSING_SUBJECT", 0.5, "件名がない")
    val letters = subject.filter(_.isLetter)
    if letters.length >= 10 && letters.forall(c => !c.isLower) && letters.exists(_.isUpper) && letters.forall(_ < 0x80)
    then hit("SUBJ_ALL_CAPS", 1.0, "件名がすべて大文字")
    if subject.count(c => c == '!' || c == '！') >= 3 then hit("SUBJ_EXCLAIM", 0.8, "件名に感嘆符が多い")
    if root.header("Reply-To").flatMap(Address.parseOne).exists(r =>
        fromDomain.nonEmpty && OrgDomain.of(r.domain) != OrgDomain.of(fromDomain)
      ) &&
      !in.dkim.exists(_.pass)
    then hit("REPLYTO_OTHER_DOMAIN", 0.8, "Reply-To が From と別のドメイン（未署名）")
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
      if ext.size >= 3 && Set("pdf", "doc", "docx", "xls", "xlsx", "jpg", "png", "txt")(ext(ext.size - 2)) &&
        (DangerousExt(last) || ArchiveExt(last))
      then
        hit("DOUBLE_EXTENSION", 3.0, s"二重の拡張子: $f")
    }
    if attachments.exists(a => a.filename.exists(f => ArchiveExt(f.toLowerCase.split('.').last))) &&
      in.dkim.forall(!_.pass)
    then
      hit("ARCHIVE_UNSIGNED", 0.7, "未署名のメールに圧縮ファイル")

    val bodyText =
      leaves.filter(p => p.mainType == "text").take(3).map(p => Mime.text(in.msg, p)).mkString("\n").take(200_000)
    val urls = Urls.find(bodyText)
    if urls.exists(u => """https?://\d+\.\d+\.\d+\.\d+""".r.findPrefixOf(u).isDefined) then
      hit("URI_NUMERIC_IP", 1.5, "IP アドレスの URL がある")
    if urls.exists(u => Urls.Shorteners.exists(s => u.toLowerCase.contains("//" + s + "/"))) then
      hit("URI_SHORTENER", 0.8, "短縮 URL がある")
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
  }
}
