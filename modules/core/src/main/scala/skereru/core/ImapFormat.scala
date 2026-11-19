package skereru.core

import java.nio.charset.StandardCharsets
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

/** IMAP の応答の整形（文字列の引用・ENVELOPE・BODYSTRUCTURE・日時）。 */
object ImapFormat {

  /** 文字列を引用符で囲む。改行・8bit を含むならリテラルにする（呼び出し側でバイト列として送る）。 */
  def quote(
    s: String
  ): String =
    if s.exists(c => c == '\r' || c == '\n' || c > 0x7e || c < 0x20) then {
      val b = s.getBytes(StandardCharsets.UTF_8)
      s"{${b.length}}\r\n" + new String(b, StandardCharsets.ISO_8859_1)
    } else "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** ヘッダーの生の値（ISO-8859-1 で読んだもの）を nstring にする。 */
  def nstringRaw(
    s: Option[String]
  ): String = s match {
    case None => "NIL"
    case Some(v) =>
      if v.exists(c => c == '\r' || c == '\n' || c > 0x7e || c < 0x20 || c == '"' || c == '\\') then
        s"{${v.length}}\r\n$v" // ISO-8859-1 の文字列はそのままバイト数
      else "\"" + v + "\""
  }

  def nstring(
    s: Option[String]
  ): String = s.fold("NIL")(quote)

  def astring(
    s: String
  ): String =
    if s.nonEmpty && s.forall(c => c > 0x20 && c < 0x7f && !"(){ %*\"\\]".contains(c)) then s else quote(s)

  /** メールボックス名（修正 UTF-7 にして引用する）。 */
  def mailbox(
    name: String
  ): String = quote(MUtf7.encode(name))

  private val internalFmt = DateTimeFormatter.ofPattern("dd-MMM-yyyy HH:mm:ss Z", Locale.ENGLISH)
  def internalDate(
    t: java.time.Instant,
    zone: java.time.ZoneId
  ): String = "\"" + internalFmt.format(t.atZone(zone)) + "\""

  /** APPEND の日時（"17-Jul-1996 02:44:25 -0700"）。 */
  def parseInternalDate(
    s: String
  ): Option[java.time.Instant] =
    scala.util.Try(ZonedDateTime.parse(s.trim.replaceFirst("^ ", "0"), internalFmt).toInstant)
      .orElse(scala.util.Try(ZonedDateTime.parse(
        s.trim,
        DateTimeFormatter.ofPattern("d-MMM-yyyy HH:mm:ss Z", Locale.ENGLISH)
      ).toInstant)).toOption

  private def addressList(
    raw: Option[String]
  ): String =
    raw.map(Address.parseGroups).filter(_.nonEmpty) match {
      case None => "NIL"
      case Some(items) =>
        def one(
          a: MailAddress
        ) =
          s"(${nstringRaw(a.name)} NIL ${nstringRaw(Some(a.local))} ${
              if a.domain.isEmpty then "NIL" else nstringRaw(Some(a.domain))
            })"
        "(" + items.map {
          case Left(a) => one(a)
          case Right(g) => s"(NIL NIL ${nstringRaw(Some(g.name))} NIL)" + g.members.map(one).mkString +
              "(NIL NIL NIL NIL)"
        }.mkString + ")"
    }

  def envelope(
    p: MimePart
  ): String = {
    def h(
      n: String
    ) = p.header(n)
    val from = h("From")
    List(
      nstringRaw(h("Date")),
      nstringRaw(h("Subject")),
      addressList(from),
      addressList(h("Sender").orElse(from)),
      addressList(h("Reply-To").orElse(from)),
      addressList(h("To")),
      addressList(h("Cc")),
      addressList(h("Bcc")),
      nstringRaw(h("In-Reply-To")),
      nstringRaw(h("Message-ID"))
    ).mkString("(", " ", ")")
  }

  private def params(
    pv: ParamValue
  ): String =
    if pv.params.isEmpty then "NIL"
    else
      pv.params.toList.sortBy(_._1).map(
        (
          k,
          v
        ) => s"${quote(k.toUpperCase)} ${quote(v)}"
      ).mkString("(", " ", ")")

  /** BODY / BODYSTRUCTURE（extended=true で拡張データつき）。 */
  def bodyStructure(
    msg: Array[Byte],
    p: MimePart,
    extended: Boolean
  ): String =
    if p.isMultipart then {
      val kids = if p.children.isEmpty then "(\"text\" \"plain\" NIL NIL NIL \"7bit\" 0 0)"
      else p.children.map(bodyStructure(msg, _, extended)).mkString
      val ext = if extended then s" ${params(ParamValue("", p.contentType.params))} ${disp(p)} NIL NIL" else ""
      s"($kids ${quote(p.subType)}$ext)"
    } else {
      val base = List(
        quote(p.mainType),
        quote(p.subType),
        params(p.contentType),
        nstringRaw(p.header("Content-ID")),
        nstringRaw(p.header("Content-Description")),
        quote(p.encoding.toUpperCase match {
          case "7BIT" | "8BIT" | "BINARY" | "BASE64" | "QUOTED-PRINTABLE" => p.encoding.toUpperCase; case o => o
        }),
        p.size.toString
      )
      val typeSpecific =
        if p.isMessage then
          p.message.toList.flatMap(m => List(envelope(m), bodyStructure(msg, m, extended), p.bodyLines.toString))
        else if p.mainType == "text" then List(p.bodyLines.toString)
        else Nil
      val ext = if extended then List("NIL", disp(p), "NIL", "NIL") else Nil
      (base ++ typeSpecific ++ ext).mkString("(", " ", ")")
    }

  private def disp(
    p: MimePart
  ): String = p.disposition match {
    case None => "NIL"
    case Some(pv) => s"(${quote(pv.value.toLowerCase)} ${params(pv)})"
  }

  /** BODY[section] の中身（バイト列）。部品が存在しなければ空。 */
  def sectionBytes(
    msg: Array[Byte],
    root: MimePart,
    s: Section
  ): Array[Byte] = {
    def slice(
      a: Int,
      b: Int
    ) = java.util.Arrays.copyOfRange(msg, a, math.max(a, b))
    // 部品番号の先の対象（HEADER・TEXT は message/rfc822 の中身のメッセージ）
    root.part(s.path) match {
      case None => Array.emptyByteArray
      case Some(p0) =>
        val p = if s.path.nonEmpty && Set("HEADER", "TEXT", "HEADER.FIELDS", "HEADER.FIELDS.NOT")(s.kind) then
          p0.message.getOrElse(p0)
        else p0
        s.kind match {
          case "" =>
            if s.path.isEmpty then msg
            else slice(p.bodyStart, p.end)
          case "HEADER" => slice(p.start, p.bodyStart)
          case "TEXT" => slice(p.bodyStart, p.end)
          case "MIME" => slice(p0.start, p0.bodyStart)
          case "HEADER.FIELDS" | "HEADER.FIELDS.NOT" =>
            val want = s.fields.map(_.toLowerCase).toSet
            val not = s.kind.endsWith(".NOT")
            val text = Mime.latin1(msg, p.start, p.bodyStart)
            // 折り返しを含む元の行のまま取り出す
            val out = new StringBuilder
            var keep = false
            text.split("(?<=\n)").foreach { line =>
              if line.startsWith(" ") || line.startsWith("\t") then { if keep then out.append(line) }
              else if line.trim.isEmpty then ()
              else {
                val n = line.takeWhile(_ != ':').trim.toLowerCase
                keep = want.contains(n) != not
                if keep then out.append(line)
              }
            }
            out.append("\r\n")
            out.toString.getBytes(StandardCharsets.ISO_8859_1)
          case _ => Array.emptyByteArray
        }
    }
  }
}
