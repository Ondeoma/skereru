package skereru.mail

import io.circe.Json
import skereru.core.*
import skereru.db.*
import skereru.shared.*

import java.time.Instant

/** メッセージを解析して、保存する属性（件名・差出人・宛先・プレビュー・検索用の本文）を作る。 */
object Indexer {

  def bodyText(
    raw: Array[Byte],
    root: MimePart
  ): String = {
    val leaves = root.leaves().map(_._2)
    val plain = leaves.find(p => p.mimeType == "text/plain" && !p.isAttachment)
    val html = leaves.find(p => p.mimeType == "text/html" && !p.isAttachment)
    plain.map(Mime.text(raw, _)).orElse(html.map(h => Html.toText(Mime.text(raw, h)))).getOrElse("")
  }

  def index(
    raw: Array[Byte],
    blobId: String,
    internalDate: Instant,
    flags: List[String],
    spam: Option[SpamVerdict],
    auth: Option[Json],
    parsed: Option[MimePart] = None
  ): NewMessage = {
    val root = parsed.getOrElse(Mime.parse(raw))
    val from = root.header("From").flatMap(Address.parseOne)
    // 表示名は引用符で囲んで保存する（"Doe, John" のようにカンマを含む名前があるため）
    def addrs(
      h: String
    ) = root.header(h).toList.flatMap(Address.parseList)
      .map(a => MailText.quoted(a.nameDecoded, a.address)).mkString(", ")
    val text = bodyText(raw, root)
    val files = root.leaves().map(_._2).filter(_.isAttachment)
    val refs = root.header("References").toList.flatMap(r => "<[^>]+>".r.findAllIn(r).toList)
    NewMessage(
      blobId = blobId,
      size = raw.length,
      internalDate = internalDate,
      flags = flags,
      subject = root.headerDecoded("Subject").getOrElse("").replaceAll("[\r\n\t]+", " ").trim,
      fromAddr = from.map(_.address.toLowerCase),
      fromName = from.flatMap(_.nameDecoded),
      toAddrs = addrs("To"),
      ccAddrs = addrs("Cc"),
      sentAt = root.header("Date").flatMap(Dates.parseRfc5322),
      messageId = root.header("Message-ID").map(_.trim).filter(_.nonEmpty).map(_.take(500)),
      inReplyTo = root.header("In-Reply-To").flatMap(r => "<[^>]+>".r.findFirstIn(r)),
      references = refs.takeRight(30),
      hasAttachments = files.nonEmpty,
      preview = text.replaceAll("(?m)^>.*$", "").replaceAll("\\s+", " ").trim.take(200),
      // 添付ファイル（テキスト・PDF・Office・ODF）の本文も検索できるようにする
      searchText =
        (text.take(500_000) + "\n" + files.flatMap(_.filename).mkString(" ") +
          (if files.isEmpty then "" else "\n" + AttachmentText.extract(raw, root))).replace("\u0000", ""),
      spamScore = spam.map(_.score),
      spamReport = spam.map(_.summary),
      authResults = auth
    )
  }
}
