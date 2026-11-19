package skereru.core

import skereru.shared.*

/** mbox への書き出し（取り込みと対になる。フォルダ・スター・未読は Google Takeout と同じ X-Gmail-Labels で書く）。 */
object MailExport {

  /** 1通のラベル（特別なメールボックスは Gmail と同じ名前。アーカイブはラベルなし）。 */
  def labels(
    specialUse: Option[String],
    name: String,
    seen: Boolean,
    flagged: Boolean
  ): List[String] = {
    val box = specialUse match {
      case Some(SpecialUse.Inbox) => List("Inbox")
      case Some(SpecialUse.Sent) => List("Sent")
      case Some(SpecialUse.Drafts) => List("Draft")
      case Some(SpecialUse.Junk) => List("Spam")
      case Some(SpecialUse.Trash) => List("Trash")
      case Some(SpecialUse.Archive) => Nil
      case _ => List(name)
    }
    box ++ (if flagged then List("Starred") else Nil) ++ (if seen then Nil else List("Unread"))
  }

  /** ラベルのヘッダーの値（カンマ・引用符を含むラベルは引用符で囲む）。 */
  def labelHeader(
    ls: List[String]
  ): String =
    ls.map(l => if l.exists(c => c == ',' || c == '"') then "\"" + l.replace("\"", "") + "\"" else l).mkString(",")

  private val Asctime = java.time.format.DateTimeFormatter.ofPattern("EEE MMM dd HH:mm:ss yyyy", java.util.Locale.US)

  /** mbox（mboxrd）の1通。改行は LF にし、行頭の ">…From " には > を1つ足す。 */
  def entry(
    raw: Array[Byte],
    labels: List[String],
    date: java.time.Instant
  ): Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream(raw.length + 256)
    val latin1 = java.nio.charset.StandardCharsets.ISO_8859_1
    out.write(s"From skereru@localhost ${Asctime.format(date.atZone(java.time.ZoneOffset.UTC))}\n".getBytes(latin1))
    // ラベルは取り込むときに外すので、本体の内容（重複の判定）には影響しない
    out.write(s"X-Gmail-Labels: ${labelHeader(labels)}\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))
    var i = 0
    var lineStart = true
    while i < raw.length do {
      val c = raw(i)
      if lineStart then {
        var j = i
        while j < raw.length && raw(j) == '>' do j += 1
        if j + 5 <= raw.length && raw(j) == 'F' && raw(j + 1) == 'r' && raw(j + 2) == 'o' && raw(j + 3) == 'm' &&
          raw(j + 4) == ' '
        then out.write('>')
      }
      if c == '\r' && i + 1 < raw.length && raw(i + 1) == '\n' then () // CRLF は LF にする
      else out.write(c)
      lineStart = c == '\n'
      i += 1
    }
    if raw.nonEmpty && raw.last != '\n' then out.write('\n')
    out.write('\n')
    out.toByteArray
  }
}
