package skereru.core

import skereru.shared.*

/** 取り込み先（特別なメールボックスか、名前のフォルダ）。 */
enum ImportTarget {
  case Special(
    use: String)
  case Folder(
    name: String)
}

/** 1通の置き場所と印。targets が空なら取り込まない（Gmail のチャットなど）。 */
final case class Placement(targets: List[ImportTarget], seen: Boolean, flagged: Boolean)

/** IMAP のフォルダの扱い。AllMail は Gmail の「すべてのメール」（ほかのフォルダにないもの＝アーカイブだけを取り込む）。 */
enum ImapFolderPlan {
  case Into(
    target: ImportTarget)
  case AllMail
  case Skip
}

/** ほかのサービス（Gmail など）から取り込むときの、置き場所の決め方。 */
object MailImport {

  /** 取り込んだフォルダの名前に使えない名前のときに入れる先。 */
  val Fallback = "取り込み"

  /** X-Gmail-Labels（Google Takeout の mbox）を分ける。"a,b" と、カンマを含むラベルの "\"x,y\"" に対応する。 */
  def gmailLabels(raw: String): List[String] = {
    val out = scala.collection.mutable.ListBuffer.empty[String]
    val sb = new StringBuilder
    var quoted = false
    raw.foreach {
      case '"' => quoted = !quoted
      case ',' if !quoted => out += sb.toString; sb.clear()
      case c => sb.append(c)
    }
    out += sb.toString
    out.toList.map(l => Mime.decodeHeader(l).trim).filter(_.nonEmpty)
  }

  /**
   * Gmail のラベルから置き場所を決める。ラベルはそれぞれのフォルダに入れる（Gmail のラベルと同じ見え方）。
   * ゴミ箱・迷惑メールのものはそこだけ。どのラベルもなければアーカイブ。
   */
  def gmailPlacement(labels: List[String]): Placement = {
    val ls = labels.map(_.trim)
    def has(l: String) = ls.exists(_.equalsIgnoreCase(l))
    val seen = !has("Unread")
    val flagged = has("Starred")
    if has("Chat") || has("Chats") then Placement(Nil, seen, flagged)
    else if has("Trash") then Placement(List(ImportTarget.Special(SpecialUse.Trash)), seen, flagged)
    else if has("Spam") then Placement(List(ImportTarget.Special(SpecialUse.Junk)), seen, flagged)
    else {
      val special = ls.flatMap(_.toLowerCase match {
        case "inbox" => Some(SpecialUse.Inbox)
        case "sent" => Some(SpecialUse.Sent)
        case "draft" | "drafts" => Some(SpecialUse.Drafts)
        case _ => None
      }).distinct.map(ImportTarget.Special(_))
      // 状態を表すだけのラベル（既読・重要・カテゴリなど）はフォルダにしない
      val system =
        Set("unread", "opened", "starred", "important", "archived", "inbox", "sent", "draft", "drafts", "trash", "spam")
      val folders =
        ls.filterNot(l => system(l.toLowerCase) || l.toLowerCase.startsWith("category ") || l.startsWith("IMAP_"))
          .map(folderName(_, '/')).distinct.map(ImportTarget.Folder(_))
      val targets = special ++ folders
      Placement(if targets.isEmpty then List(ImportTarget.Special(SpecialUse.Archive)) else targets, seen, flagged)
    }
  }

  /** IMAP のフォルダの扱い（SPECIAL-USE の属性と、名前）。 */
  def imapFolder(fullName: String, delimiter: Char, attrs: Set[String]): ImapFolderPlan = {
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
  }

  /** ほかのサービスのフォルダ名を、このサーバーで使える名前にする（区切りは "/"。使えない名前は「取り込み/…」）。 */
  def folderName(name: String, delimiter: Char): String = {
    val parts = name.split(java.util.regex.Pattern.quote(delimiter.toString)).toList
      .map(_.filterNot(c => c < 0x20 || c == '*' || c == '%' || c == '/').trim).filter(_.nonEmpty)
    val n = parts.mkString("/").take(200)
    if MailText.validMailboxName(n) then n else if n.isEmpty then Fallback else s"$Fallback/${n.take(150)}"
  }
}
