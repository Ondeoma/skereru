package skereru.core

import skereru.shared.*

/** 取り込み先（特別なメールボックスか、名前のフォルダ）。 */
enum ImportTarget:
  case Special(use: String)
  case Folder(name: String)

/** 1通の置き場所と印。targets が空なら取り込まない（Gmail のチャットなど）。 */
final case class Placement(targets: List[ImportTarget], seen: Boolean, flagged: Boolean)

/** IMAP のフォルダの扱い。AllMail は Gmail の「すべてのメール」（ほかのフォルダにないもの＝アーカイブだけを取り込む）。 */
enum ImapFolderPlan:
  case Into(target: ImportTarget)
  case AllMail
  case Skip

/** ほかのサービス（Gmail など）から取り込むときの、置き場所の決め方。 */
object MailImport:

  /** 取り込んだフォルダの名前に使えない名前のときに入れる先。 */
  val Fallback = "取り込み"

  /** X-Gmail-Labels（Google Takeout の mbox）を分ける。"a,b" と、カンマを含むラベルの "\"x,y\"" に対応する。 */
  def gmailLabels(raw: String): List[String] =
    val out = scala.collection.mutable.ListBuffer.empty[String]
    val sb = new StringBuilder
    var quoted = false
    raw.foreach {
      case '"'                => quoted = !quoted
      case ',' if !quoted     => out += sb.toString; sb.clear()
      case c                  => sb.append(c)
    }
    out += sb.toString
    out.toList.map(l => Mime.decodeHeader(l).trim).filter(_.nonEmpty)

  /** Gmail のラベルから置き場所を決める。ラベルはそれぞれのフォルダに入れる（Gmail のラベルと同じ見え方）。
    * ゴミ箱・迷惑メールのものはそこだけ。どのラベルもなければアーカイブ。 */
  def gmailPlacement(labels: List[String]): Placement =
    val ls = labels.map(_.trim)
    def has(l: String) = ls.exists(_.equalsIgnoreCase(l))
    val seen = !has("Unread")
    val flagged = has("Starred")
    if has("Chat") || has("Chats") then Placement(Nil, seen, flagged)
    else if has("Trash") then Placement(List(ImportTarget.Special(SpecialUse.Trash)), seen, flagged)
    else if has("Spam") then Placement(List(ImportTarget.Special(SpecialUse.Junk)), seen, flagged)
    else
      val special = ls.flatMap(_.toLowerCase match
        case "inbox"            => Some(SpecialUse.Inbox)
        case "sent"             => Some(SpecialUse.Sent)
        case "draft" | "drafts" => Some(SpecialUse.Drafts)
        case _                  => None
      ).distinct.map(ImportTarget.Special(_))
      // 状態を表すだけのラベル（既読・重要・カテゴリなど）はフォルダにしない
      val system = Set("unread", "opened", "starred", "important", "archived", "inbox", "sent", "draft", "drafts", "trash", "spam")
      val folders = ls.filterNot(l => system(l.toLowerCase) || l.toLowerCase.startsWith("category ") || l.startsWith("IMAP_"))
        .map(folderName(_, '/')).distinct.map(ImportTarget.Folder(_))
      val targets = special ++ folders
      Placement(if targets.isEmpty then List(ImportTarget.Special(SpecialUse.Archive)) else targets, seen, flagged)

  /** IMAP のフォルダの扱い（SPECIAL-USE の属性と、名前）。 */
  def imapFolder(fullName: String, delimiter: Char, attrs: Set[String]): ImapFolderPlan =
    val a = attrs.map(_.toLowerCase)
    if a("\\noselect") || a("\\nonexistent") then ImapFolderPlan.Skip
    else if a("\\all") then ImapFolderPlan.AllMail
    // Gmail の「重要」「スター付き」は、ほかのフォルダのメールの見え方なので取り込まない（スターは印として取り込む）
    else if a("\\important") || a("\\flagged") then ImapFolderPlan.Skip
    else if fullName.equalsIgnoreCase("INBOX") then ImapFolderPlan.Into(ImportTarget.Special(SpecialUse.Inbox))
    else if a("\\sent") then ImapFolderPlan.Into(ImportTarget.Special(SpecialUse.Sent))
    else if a("\\drafts") then ImapFolderPlan.Into(ImportTarget.Special(SpecialUse.Drafts))
    else if a("\\junk") then ImapFolderPlan.Into(ImportTarget.Special(SpecialUse.Junk))
    else if a("\\trash") then ImapFolderPlan.Into(ImportTarget.Special(SpecialUse.Trash))
    else if a("\\archive") then ImapFolderPlan.Into(ImportTarget.Special(SpecialUse.Archive))
    else ImapFolderPlan.Into(ImportTarget.Folder(folderName(fullName, delimiter)))

  /** ほかのサービスのフォルダ名を、このサーバーで使える名前にする（区切りは "/"。使えない名前は「取り込み/…」）。 */
  def folderName(name: String, delimiter: Char): String =
    val parts = name.split(java.util.regex.Pattern.quote(delimiter.toString)).toList
      .map(_.filterNot(c => c < 0x20 || c == '*' || c == '%' || c == '/').trim).filter(_.nonEmpty)
    val n = parts.mkString("/").take(200)
    if MailText.validMailboxName(n) then n else if n.isEmpty then Fallback else s"$Fallback/${n.take(150)}"

/** mbox への書き出し（取り込みと対になる。フォルダ・スター・未読は Google Takeout と同じ X-Gmail-Labels で書く）。 */
object MailExport:

  /** 1通のラベル（特別なメールボックスは Gmail と同じ名前。アーカイブはラベルなし）。 */
  def labels(specialUse: Option[String], name: String, seen: Boolean, flagged: Boolean): List[String] =
    val box = specialUse match
      case Some(SpecialUse.Inbox)   => List("Inbox")
      case Some(SpecialUse.Sent)    => List("Sent")
      case Some(SpecialUse.Drafts)  => List("Draft")
      case Some(SpecialUse.Junk)    => List("Spam")
      case Some(SpecialUse.Trash)   => List("Trash")
      case Some(SpecialUse.Archive) => Nil
      case _                        => List(name)
    box ++ (if flagged then List("Starred") else Nil) ++ (if seen then Nil else List("Unread"))

  /** ラベルのヘッダーの値（カンマ・引用符を含むラベルは引用符で囲む）。 */
  def labelHeader(ls: List[String]): String =
    ls.map(l => if l.exists(c => c == ',' || c == '"') then "\"" + l.replace("\"", "") + "\"" else l).mkString(",")

  private val Asctime = java.time.format.DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss yyyy", java.util.Locale.US)

  /** mbox（mboxrd）の1通。改行は LF にし、行頭の ">…From " には > を1つ足す。 */
  def entry(raw: Array[Byte], labels: List[String], date: java.time.Instant): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream(raw.length + 256)
    val latin1 = java.nio.charset.StandardCharsets.ISO_8859_1
    out.write(s"From skereru@localhost ${Asctime.format(date.atZone(java.time.ZoneOffset.UTC))}\n".getBytes(latin1))
    // ラベルは取り込むときに外すので、本体の内容（重複の判定）には影響しない
    out.write(s"X-Gmail-Labels: ${labelHeader(labels)}\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    var i = 0
    var lineStart = true
    while i < raw.length do
      val c = raw(i)
      if lineStart then
        var j = i
        while j < raw.length && raw(j) == '>' do j += 1
        if j + 5 <= raw.length && raw(j) == 'F' && raw(j + 1) == 'r' && raw(j + 2) == 'o' && raw(j + 3) == 'm' && raw(j + 4) == ' ' then out.write('>')
      if c == '\r' && i + 1 < raw.length && raw(i + 1) == '\n' then () // CRLF は LF にする
      else out.write(c)
      lineStart = c == '\n'
      i += 1
    if raw.nonEmpty && raw.last != '\n' then out.write('\n')
    out.write('\n')
    out.toByteArray
