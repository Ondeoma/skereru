package skereru.core

import jakarta.activation.DataHandler
import jakarta.mail.{Message, Session}
import jakarta.mail.internet.*
import jakarta.mail.util.ByteArrayDataSource

import java.nio.charset.StandardCharsets
import java.time.{Instant, ZoneId}
import java.util.{Date, Properties}

/** 添付ファイル（中身・名前・種類）。 */
final case class Attachment(filename: String, contentType: String, data: Array[Byte], contentId: Option[String] = None)

/** 組み立てるメッセージ。 */
final case class Outgoing(
  from: MailAddress,
  to: List[MailAddress],
  cc: List[MailAddress],
  subject: String,
  text: String,
  html: Option[String],
  attachments: List[Attachment],
  inReplyTo: Option[String],
  references: List[String],
  extraHeaders: List[(String, String)] = Nil,
  /** iCalendar の部品（method と本文）。会議の招待への返事など。本文（text）と並べた multipart/alternative にする。 */
  calendar: Option[(String, String)] = None)

/** メッセージの組み立て（Jakarta Mail を使う。改行は CRLF）。 */
object Compose {

  private val session = Session.getInstance(new Properties())

  /** Message-ID（<ランダム@ドメイン>）。 */
  def newMessageId(domain: String): String = {
    val r = new Array[Byte](18)
    new java.security.SecureRandom().nextBytes(r)
    s"<${java.util.Base64.getUrlEncoder.withoutPadding().encodeToString(r)}@$domain>"
  }

  private def ia(a: MailAddress): InternetAddress = {
    val x = new InternetAddress(a.address)
    a.nameDecoded.foreach(n => x.setPersonal(n, "UTF-8"))
    x
  }

  /** 組み立てたメッセージのバイト列と Message-ID。Bcc はヘッダーに書かない。 */
  def build(o: Outgoing, messageId: String, now: Instant, zone: ZoneId): Array[Byte] = {
    val msg = new MimeMessage(session) {
      override def updateMessageID(): Unit = setHeader("Message-ID", messageId)
    }
    msg.setFrom(ia(o.from))
    if o.to.nonEmpty then msg.setRecipients(Message.RecipientType.TO, o.to.map(ia).toArray[jakarta.mail.Address])
    if o.cc.nonEmpty then msg.setRecipients(Message.RecipientType.CC, o.cc.map(ia).toArray[jakarta.mail.Address])
    msg.setSubject(o.subject, "UTF-8")
    msg.setSentDate(Date.from(now))
    o.inReplyTo.foreach(r => msg.setHeader("In-Reply-To", r))
    if o.references.nonEmpty then msg.setHeader("References", MimeUtility.fold(12, o.references.mkString(" ")))
    o.extraHeaders.foreach((k, v) => msg.setHeader(k, v))

    def textPart(): MimeBodyPart = {
      val p = new MimeBodyPart()
      p.setText(o.text, "UTF-8", "plain")
      p
    }
    def htmlPart(h: String): MimeBodyPart = {
      val p = new MimeBodyPart()
      p.setText(h, "UTF-8", "html")
      p
    }
    def calendarPart(method: String, ics: String): MimeBodyPart = {
      val p = new MimeBodyPart()
      p.setContent(ics, s"text/calendar; method=$method; charset=UTF-8")
      p.setHeader("Content-Transfer-Encoding", "base64")
      p
    }
    val body: MimeMultipart | Null = o.calendar match {
      case Some((method, ics)) =>
        val alt = new MimeMultipart("alternative")
        alt.addBodyPart(textPart())
        alt.addBodyPart(calendarPart(method, ics))
        alt
      case None => o.html match {
          case Some(h) =>
            val alt = new MimeMultipart("alternative")
            alt.addBodyPart(textPart())
            val inline = o.attachments.filter(_.contentId.isDefined)
            if inline.isEmpty then alt.addBodyPart(htmlPart(h))
            else {
              val rel = new MimeMultipart("related")
              rel.addBodyPart(htmlPart(h))
              inline.foreach(a => rel.addBodyPart(attachmentPart(a)))
              val relPart = new MimeBodyPart()
              relPart.setContent(rel)
              alt.addBodyPart(relPart)
            }
            alt
          case None => null
        }
    }
    val files = o.attachments.filter(a => o.html.isEmpty || a.contentId.isEmpty)
    if files.isEmpty then
      if body == null then msg.setText(o.text, "UTF-8", "plain") else msg.setContent(body)
    else {
      val mixed = new MimeMultipart("mixed")
      val first = new MimeBodyPart()
      if body == null then first.setText(o.text, "UTF-8", "plain") else first.setContent(body)
      mixed.addBodyPart(first)
      files.foreach(a => mixed.addBodyPart(attachmentPart(a)))
      msg.setContent(mixed)
    }
    msg.saveChanges()
    msg.setHeader("Date", Dates.rfc5322(now, zone)) // setSentDate は JVM の既定のタイムゾーンになるため
    val out = new java.io.ByteArrayOutputStream()
    msg.writeTo(out)
    out.toByteArray
  }

  private def attachmentPart(a: Attachment): MimeBodyPart = {
    val p = new MimeBodyPart()
    p.setDataHandler(new DataHandler(new ByteArrayDataSource(a.data, a.contentType)))
    p.setFileName(MimeUtility.encodeText(a.filename, "UTF-8", "B"))
    a.contentId match {
      case Some(cid) =>
        p.setContentID(s"<$cid>")
        p.setDisposition(jakarta.mail.Part.INLINE)
      case None => p.setDisposition(jakarta.mail.Part.ATTACHMENT)
    }
    p
  }

  /** 件名の Re: / Fwd: を付ける（重ねない）。 */
  def replySubject(s: String): String = if s.trim.toLowerCase.matches("^(re|aw|sv|返信)\\s*[:：].*") then s else s"Re: $s"
  def forwardSubject(s: String): String = if s.trim.toLowerCase.matches("^(fwd?|転送)\\s*[:：].*") then s else s"Fwd: $s"

  /** スレッドの判定に使う件名（Re: などを取り除く）。 */
  def baseSubject(s: String): String =
    s.replaceAll("(?i)^(\\s*(re|fwd?|aw|sv|返信|転送)(\\[\\d+\\])?\\s*[:：]\\s*)+", "").replaceAll("\\s+", " ").trim

  /** 引用の本文（"> " を付ける）。 */
  def quote(text: String): String =
    text.replace("\r\n", "\n").split("\n", -1).map(l => if l.startsWith(">") then ">" + l else "> " + l).mkString("\n")

  /** 配送できなかったことを知らせるメール（DSN、RFC 3464）。 */
  def bounce(postmaster: MailAddress,
             to: String,
             original: Array[Byte],
             failed: List[(String, String)],
             reportingHost: String,
             now: Instant,
             zone: ZoneId): Array[Byte] = {
    val origHeaders = Mime.latin1(original, 0, Mime.headerEnd(original, 0, original.length))
    val boundary = "=_dsn_" + Compose.newMessageId(reportingHost).replaceAll("[<>@.]", "")
    val human =
      s"""次の宛先にメールを配送できませんでした。
         |This is the mail system at $reportingHost. Your message could not be delivered to the following recipient(s).
         |
         |""".stripMargin + failed.map((r, e) => s"<$r>: $e").mkString("\n") + "\n"
    val status = failed.map {
      (r, e) =>
        {
          val code = """\b([245]\.\d{1,3}\.\d{1,3})\b""".r.findFirstIn(e).getOrElse("5.0.0")
          s"Final-Recipient: rfc822; $r\r\nAction: failed\r\nStatus: $code\r\nDiagnostic-Code: smtp; ${e.replaceAll("[\r\n]+", " ").take(500)}\r\n"
        }
    }.mkString("\r\n")
    val sb = new StringBuilder
    sb.append(s"From: ${postmaster.nameDecoded.fold("")(n => Mime.encodeHeader(n) + " ")}<${postmaster.address}>\r\n")
    sb.append(s"To: <$to>\r\n")
    sb.append(s"Subject: ${Mime.encodeHeader("配送できませんでした (Undelivered Mail Returned to Sender)")}\r\n")
    sb.append(s"Date: ${Dates.rfc5322(now, zone)}\r\n")
    sb.append(s"Message-ID: ${newMessageId(reportingHost)}\r\n")
    sb.append("Auto-Submitted: auto-replied\r\nMIME-Version: 1.0\r\n")
    sb.append(s"Content-Type: multipart/report; report-type=delivery-status; boundary=\"$boundary\"\r\n\r\n")
    sb.append(s"--$boundary\r\nContent-Type: text/plain; charset=UTF-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\n")
    val head = sb.toString.getBytes(StandardCharsets.UTF_8)
    val rest =
      s"\r\n--$boundary\r\nContent-Type: message/delivery-status\r\n\r\nReporting-MTA: dns; $reportingHost\r\n\r\n$status\r\n" +
        s"--$boundary\r\nContent-Type: text/rfc822-headers\r\n\r\n"
    val tail = s"\r\n--$boundary--\r\n"
    head ++ human.replace("\n", "\r\n").getBytes(StandardCharsets.UTF_8) ++ rest.getBytes(StandardCharsets.UTF_8) ++
      origHeaders.getBytes(StandardCharsets.ISO_8859_1) ++ tail.getBytes(StandardCharsets.UTF_8)
  }

  /** 改行を CRLF にそろえる（単独の CR・LF も CRLF にする）。 */
  def crlf(bytes: Array[Byte]): Array[Byte] = {
    val out = new java.io.ByteArrayOutputStream(bytes.length + 64)
    var i = 0
    while i < bytes.length do {
      val c = bytes(i)
      if c == '\r' then {
        out.write('\r'); out.write('\n')
        if i + 1 < bytes.length && bytes(i + 1) == '\n' then i += 1
      } else if c == '\n' then { out.write('\r'); out.write('\n') }
      else out.write(c)
      i += 1
    }
    out.toByteArray
  }
}
