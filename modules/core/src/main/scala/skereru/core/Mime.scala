package skereru.core

import java.nio.charset.{Charset, StandardCharsets}
import java.util.Base64
import scala.collection.mutable.ArrayBuffer

/** ヘッダーの1行（折り返しは戻した値。raw はバイトを ISO-8859-1 で読んだ文字列なので、元のバイト列に戻せる）。 */
final case class HeaderField(name: String, raw: String):
  /** RFC 2047 の符号化語を戻し、UTF-8 の生のヘッダー（SMTPUTF8）も読めるようにした値。 */
  def decoded: String = Mime.decodeHeader(raw)

/** Content-Type などの値とパラメーター（RFC 2231 の分割・符号化は戻してある）。 */
final case class ParamValue(value: String, params: Map[String, String]):
  def param(name: String): Option[String] = params.get(name.toLowerCase)

/** MIME の部品。位置はすべて元のメッセージのバイト列の中のオフセット。
  * start〜bodyStart がヘッダー（区切りの空行を含む）、bodyStart〜end が本文。 */
final case class MimePart(
    start: Int,
    bodyStart: Int,
    end: Int,
    headers: List[HeaderField],
    contentType: ParamValue,
    children: List[MimePart],
    message: Option[MimePart],
    bodyLines: Int
):
  def mainType: String = contentType.value.takeWhile(_ != '/').toLowerCase
  def subType: String  = contentType.value.dropWhile(_ != '/').drop(1).toLowerCase
  def mimeType: String = s"$mainType/$subType"
  def isMultipart: Boolean = mainType == "multipart"
  def isMessage: Boolean   = mimeType == "message/rfc822"

  def header(name: String): Option[String] = headers.find(_.name.equalsIgnoreCase(name)).map(_.raw)
  def headerDecoded(name: String): Option[String] = headers.find(_.name.equalsIgnoreCase(name)).map(_.decoded)
  def headerAll(name: String): List[String] = headers.filter(_.name.equalsIgnoreCase(name)).map(_.raw)

  def encoding: String = header("Content-Transfer-Encoding").map(_.trim.toLowerCase).filter(_.nonEmpty).getOrElse("7bit")
  def disposition: Option[ParamValue] = header("Content-Disposition").map(Mime.parseParams)
  def contentId: Option[String] = header("Content-ID").map(_.trim.stripPrefix("<").stripSuffix(">"))
  def size: Int = end - bodyStart

  /** 添付ファイル名（Content-Disposition の filename、なければ Content-Type の name）。 */
  def filename: Option[String] =
    disposition.flatMap(_.param("filename")).orElse(contentType.param("name")).map(Mime.decodeHeader).map(_.trim).filter(_.nonEmpty)

  def isAttachment: Boolean =
    !isMultipart && (disposition.exists(_.value.equalsIgnoreCase("attachment")) ||
      (filename.isDefined && !disposition.exists(_.value.equalsIgnoreCase("inline"))) ||
      (mainType != "text" && !disposition.exists(_.value.equalsIgnoreCase("inline")) && mimeType != "message/delivery-status"))

  /** IMAP の部品番号で子をたどる（1 始まり。単一部品のメッセージでは 1 が本文）。 */
  def part(path: List[Int]): Option[MimePart] = path match
    case Nil => Some(this)
    case n :: rest =>
      val next =
        if isMultipart then children.lift(n - 1)
        else if isMessage then message.flatMap(m => if m.isMultipart then m.children.lift(n - 1) else if n == 1 then Some(m) else None)
        else if n == 1 then Some(this)
        else None
      // 単一部品の自身(1)は、それ以上たどれない
      next.flatMap(p => if p eq this then (if rest.isEmpty then Some(p) else None) else p.part(rest))

  /** 葉の部品をすべて（部品番号つき）。 */
  def leaves(prefix: List[Int] = Nil): List[(List[Int], MimePart)] =
    if isMultipart then children.zipWithIndex.flatMap((c, i) => c.leaves(prefix :+ (i + 1)))
    else List((if prefix.isEmpty then List(1) else prefix) -> this)

object Mime:

  private val Latin1 = StandardCharsets.ISO_8859_1

  def latin1(bytes: Array[Byte], from: Int, until: Int): String = new String(bytes, from, math.max(0, until - from), Latin1)

  /** メッセージ全体を解析する。壊れた入力でも例外を出さず、読めたところまでの構造を返す。 */
  def parse(bytes: Array[Byte]): MimePart = parsePart(bytes, 0, bytes.length, defaultType = "text/plain", depth = 0, new Budget(MaxParts))

  private val MaxDepth = 20
  /** 1通の部品の数の上限（区切り行だけを大量に並べて、部品のオブジェクトでメモリを使い切らせない）。超えた分は分けずに1つの部品として扱う。 */
  val MaxParts = 1000
  private final class Budget(var left: Int)

  private def parsePart(b: Array[Byte], start: Int, end: Int, defaultType: String, depth: Int, budget: Budget): MimePart =
    budget.left -= 1
    val bodyStart = headerEnd(b, start, end)
    val headers   = parseHeaders(latin1(b, start, bodyStart))
    val ct = headers.find(_.name.equalsIgnoreCase("Content-Type")).map(h => parseParams(h.raw))
      .filter(p => p.value.contains('/')).getOrElse(ParamValue(defaultType, if defaultType == "text/plain" then Map("charset" -> "us-ascii") else Map.empty))
    val main = ct.value.takeWhile(_ != '/').toLowerCase
    val lines = countLines(b, bodyStart, end)
    if depth < MaxDepth && budget.left > 0 && main == "multipart" && ct.param("boundary").exists(_.nonEmpty) then
      val childDefault = if ct.value.equalsIgnoreCase("multipart/digest") then "message/rfc822" else "text/plain"
      val kids = splitMultipart(b, bodyStart, end, ct.param("boundary").get, budget.left).map((s, e) => parsePart(b, s, e, childDefault, depth + 1, budget))
      MimePart(start, bodyStart, end, headers, ct, kids, None, lines)
    else if depth < MaxDepth && budget.left > 0 && ct.value.equalsIgnoreCase("message/rfc822") then
      MimePart(start, bodyStart, end, headers, ct, Nil, Some(parsePart(b, bodyStart, end, "text/plain", depth + 1, budget)), lines)
    else MimePart(start, bodyStart, end, headers, ct, Nil, None, lines)

  /** ヘッダーの終わり（空行の直後）。空行がなければ全体がヘッダー。先頭が空行ならヘッダーなし。 */
  def headerEnd(b: Array[Byte], start: Int, end: Int): Int =
    if start < end && b(start) == '\n' then return start + 1
    if start + 1 < end && b(start) == '\r' && b(start + 1) == '\n' then return start + 2
    var i = start
    while i < end do
      if b(i) == '\n' then
        if i + 1 < end && b(i + 1) == '\n' then return i + 2
        if i + 2 < end && b(i + 1) == '\r' && b(i + 2) == '\n' then return i + 3
      i += 1
    end

  private def countLines(b: Array[Byte], from: Int, until: Int): Int =
    var n = 0
    var i = from
    while i < until do
      if b(i) == '\n' then n += 1
      i += 1
    if until > from && b(until - 1) != '\n' then n + 1 else n

  /** ヘッダー部を解析する（折り返しを戻す）。 */
  def parseHeaders(text: String): List[HeaderField] =
    val out = ArrayBuffer.empty[HeaderField]
    var name: String = null
    val value = new StringBuilder
    def flush(): Unit = if name != null then out += HeaderField(name, value.toString.trim)
    text.split("\r?\n", -1).foreach { line =>
      if line.isEmpty then ()
      else if (line.head == ' ' || line.head == '\t') && name != null then value.append(' ').append(line.trim)
      else
        val i = line.indexOf(':')
        if i > 0 && !line.substring(0, i).exists(c => c == ' ' || c == '\t') then
          flush(); name = line.substring(0, i); value.clear(); value.append(line.substring(i + 1))
        else if i > 0 then
          flush(); name = line.substring(0, i).trim; value.clear(); value.append(line.substring(i + 1))
    }
    flush()
    out.toList

  /** multipart の各部品の範囲（区切り行の次の行頭〜次の区切りの直前の改行の前）。 */
  def splitMultipart(b: Array[Byte], from: Int, until: Int, boundary: String, maxParts: Int = Int.MaxValue): List[(Int, Int)] =
    val delim = ("--" + boundary).getBytes(Latin1)
    def isDelim(i: Int): Boolean =
      i + delim.length <= until && (0 until delim.length).forall(k => b(i + k) == delim(k))
    val starts = ArrayBuffer.empty[(Int, Int, Boolean)] // (区切り行の開始, 区切り行の次の行頭, 終端か)
    var i = from
    var lineStart = true
    // 上限を超えたら、残りは最後の部品にまとめる（終端の区切りがないときと同じ扱い）
    while i < until && starts.size <= maxParts do
      if lineStart && b(i) == '-' && isDelim(i) then
        val after = i + delim.length
        val close = after + 1 < until && b(after) == '-' && b(after + 1) == '-'
        var j = after
        while j < until && b(j) != '\n' do j += 1
        val next = math.min(until, j + 1)
        starts += ((i, next, close))
        i = next
        lineStart = true
        if close then i = until
      else
        lineStart = b(i) == '\n'
        i += 1
    starts.toList.sliding(2).collect {
      case List((_, s, false), (d, _, _)) =>
        // 区切りの直前の改行は区切りの一部
        val e = if d - 2 >= s && b(d - 2) == '\r' && b(d - 1) == '\n' then d - 2 else if d - 1 >= s && b(d - 1) == '\n' then d - 1 else d
        (s, math.max(s, e))
    }.toList ++ (starts.lastOption match
      case Some((_, s, false)) if s < until => List((s, until)) // 終端の区切りがない壊れたメッセージ
      case _                                => Nil)

  // ---------------------------------------------------------------- パラメーター（Content-Type・Content-Disposition）

  /** `text/plain; charset="utf-8"; name*=UTF-8''%E3%81%82` を値とパラメーターにする。 */
  def parseParams(raw: String): ParamValue =
    val parts = splitParams(raw)
    val value = parts.headOption.map(_.trim).getOrElse("")
    val kv = parts.drop(1).flatMap { p =>
      val i = p.indexOf('=')
      Option.when(i > 0)(p.substring(0, i).trim.toLowerCase -> unquote(p.substring(i + 1).trim))
    }
    // RFC 2231: name*0*=…, name*1*=… を連結し、name*=charset'lang'%XX を戻す
    val grouped = kv.groupBy((k, _) => k.takeWhile(_ != '*'))
    val params = grouped.map { (name, entries) =>
      val plain = entries.find(_._1 == name).map(_._2)
      val ext = entries.filter(_._1 != name).sortBy { (k, _) => k.drop(name.length + 1).takeWhile(_.isDigit).toIntOption.getOrElse(0) }
      val v =
        if ext.isEmpty then plain.getOrElse("")
        else
          if ext.head._1.endsWith("*") then decode2231(ext.map(_._1.endsWith("*")), ext.map(_._2))
          else ext.map(_._2).mkString
      name -> v
    }
    ParamValue(value, params)

  private def decode2231(encodedFlags: List[Boolean], values: List[String]): String =
    val first = values.head
    val q1 = first.indexOf('\'')
    val q2 = if q1 >= 0 then first.indexOf('\'', q1 + 1) else -1
    val (charset, firstValue) = if q1 >= 0 && q2 > q1 then (first.substring(0, q1), first.substring(q2 + 1)) else ("us-ascii", first)
    val cs = charsetFor(charset)
    val bytes = new java.io.ByteArrayOutputStream()
    (firstValue :: values.tail).zip(encodedFlags).foreach { (v, enc) =>
      if enc then
        var i = 0
        while i < v.length do
          if v(i) == '%' && i + 2 < v.length && isHex(v(i + 1)) && isHex(v(i + 2)) then
            bytes.write(Integer.parseInt(v.substring(i + 1, i + 3), 16)); i += 3
          else
            bytes.write(v(i).toInt & 0xff); i += 1
      else bytes.write(v.getBytes(Latin1))
    }
    new String(bytes.toByteArray, cs)

  private def isHex(c: Char) = Character.digit(c, 16) >= 0

  private def splitParams(s: String): List[String] =
    val out = ArrayBuffer.empty[String]
    val cur = new StringBuilder
    var inQ = false
    var i = 0
    var comment = 0
    while i < s.length do
      val c = s(i)
      if inQ then
        if c == '\\' && i + 1 < s.length then { cur.append(c).append(s(i + 1)); i += 1 }
        else
          if c == '"' then inQ = false
          cur.append(c)
      else if comment > 0 then
        if c == '(' then comment += 1 else if c == ')' then comment -= 1
      else if c == '"' then { inQ = true; cur.append(c) }
      else if c == '(' then comment = 1
      else if c == ';' then { out += cur.toString; cur.clear() }
      else cur.append(c)
      i += 1
    out += cur.toString
    out.toList.filter(_.trim.nonEmpty) match
      case Nil => List("")
      case l   => l

  def unquote(s: String): String =
    if s.length >= 2 && s.head == '"' && s.last == '"' then s.substring(1, s.length - 1).replaceAll("\\\\(.)", "$1") else s

  // ---------------------------------------------------------------- 文字コード・転送符号化

  /** 文字コード名を Java の Charset にする（よくある別名・誤記を吸収し、未知なら UTF-8）。 */
  def charsetFor(name: String): Charset =
    val n = name.trim.toLowerCase.stripPrefix("\"").stripSuffix("\"")
    val mapped = n match
      case "" | "us-ascii" | "ascii" | "ansi_x3.4-1968" => "UTF-8" // 7bit の誤ラベルが多いので UTF-8 で読む（ASCII は同じ）
      case "iso-2022-jp" | "csiso2022jp" | "iso-2022-jp-2" => "x-windows-iso2022jp" // ①や髙などの機種依存文字も読む
      case "shift_jis" | "shift-jis" | "sjis" | "x-sjis" | "cp932" | "ms932" | "windows-31j" => "windows-31j"
      case "euc-jp" | "x-euc-jp" => "x-eucjp-open"
      case "ks_c_5601-1987" | "ks_c_5601" | "euc-kr" => "x-windows-949"
      case "gb2312" | "gbk" | "x-gbk" => "GB18030"
      case "utf8" => "UTF-8"
      case "iso-8859-1" | "latin1" => "windows-1252"
      case other => other
    scala.util.Try(Charset.forName(mapped)).orElse(scala.util.Try(Charset.forName(n))).getOrElse(StandardCharsets.UTF_8)

  /** 転送符号化（base64・quoted-printable）を戻したバイト列。 */
  def decodeTransfer(encoding: String, raw: Array[Byte]): Array[Byte] = encoding.toLowerCase match
    case "base64" =>
      val clean = raw.filter(c => (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '+' || c == '/')
      // 末尾の = は取り除いてあるので、足りない分を補う（4 で割って 1 余る半端な1文字は捨てる）
      val usable = if clean.length % 4 == 1 then clean.dropRight(1) else clean
      val padded = usable ++ Array.fill[Byte]((4 - usable.length % 4) % 4)('='.toByte)
      scala.util.Try(Base64.getDecoder.decode(padded)).getOrElse(Array.emptyByteArray)
    case "quoted-printable" => decodeQp(raw, header = false)
    case _                  => raw

  def decodeQp(raw: Array[Byte], header: Boolean): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream(raw.length)
    var i = 0
    while i < raw.length do
      val c = raw(i)
      if c == '=' then
        if i + 2 < raw.length && isHex(raw(i + 1).toChar) && isHex(raw(i + 2).toChar) then
          out.write(Integer.parseInt(new String(Array(raw(i + 1).toChar, raw(i + 2).toChar)), 16)); i += 3
        else if i + 1 < raw.length && raw(i + 1) == '\n' then i += 2 // ソフト改行
        else if i + 2 < raw.length && raw(i + 1) == '\r' && raw(i + 2) == '\n' then i += 3
        else { out.write(c); i += 1 }
      else if header && c == '_' then { out.write(' '); i += 1 }
      else { out.write(c); i += 1 }
    out.toByteArray

  /** 部品の本文を文字列として読む（転送符号化と文字コードを戻す）。 */
  def text(msg: Array[Byte], p: MimePart): String =
    val raw = java.util.Arrays.copyOfRange(msg, p.bodyStart, p.end)
    val bytes = decodeTransfer(p.encoding, raw)
    val cs = p.contentType.param("charset").map(charsetFor).getOrElse(guessCharset(bytes))
    new String(bytes, cs)

  /** 部品の中身（転送符号化を戻したバイト列。添付ファイルのダウンロード用）。 */
  def content(msg: Array[Byte], p: MimePart): Array[Byte] =
    decodeTransfer(p.encoding, java.util.Arrays.copyOfRange(msg, p.bodyStart, p.end))

  /** 文字コードの指定がないときの推測（UTF-8 として正しければ UTF-8、ESC $ B があれば ISO-2022-JP）。 */
  def guessCharset(bytes: Array[Byte]): Charset =
    if bytes.containsSlice(Array[Byte](0x1b, '$', 'B')) then charsetFor("iso-2022-jp")
    else
      val dec = StandardCharsets.UTF_8.newDecoder()
      if scala.util.Try(dec.decode(java.nio.ByteBuffer.wrap(bytes))).isSuccess then StandardCharsets.UTF_8 else charsetFor("shift_jis")

  // ---------------------------------------------------------------- ヘッダーの復号（RFC 2047）

  private val Encoded = """=\?([^?\s]+)\?([bBqQ])\?([^?\s]*)\?=""".r

  /** 符号化語（=?UTF-8?B?…?=）を戻す。ISO-8859-1 で読んだ生の 8bit（UTF-8 など）も文字に戻す。 */
  def decodeHeader(raw: String): String =
    // ISO-8859-1 で読んだ生のバイト列（0xff 以下の文字だけ）なら、文字コードを推測して読み直す
    val s = if raw.exists(_ > 0x7f) && raw.forall(_ <= 0xff) then
      val bytes = raw.getBytes(Latin1)
      new String(bytes, guessCharset(bytes))
    else raw
    if !s.contains("=?") then s
    else
      // 符号化語どうしの間の空白は取り除く
      val joined = s.replaceAll("""(\?=)\s+(=\?)""", "$1$2")
      // 同じ文字コードの連続した符号化語は、バイト列を連結してから戻す（マルチバイト文字が分割されている場合）
      val sb = new StringBuilder
      var last = 0
      var pendingCs: String = null
      val pending = new java.io.ByteArrayOutputStream()
      def flushPending(): Unit =
        if pendingCs != null then sb.append(new String(pending.toByteArray, charsetFor(pendingCs)))
        pending.reset(); pendingCs = null
      Encoded.findAllMatchIn(joined).foreach { m =>
        if m.start > last then { flushPending(); sb.append(joined.substring(last, m.start)) }
        val cs = m.group(1).takeWhile(_ != '*') // RFC 2231 の言語指定
        val data = m.group(3)
        val bytes =
          if m.group(2).equalsIgnoreCase("B") then
            scala.util.Try(Base64.getMimeDecoder.decode(data)).getOrElse(data.getBytes(Latin1))
          else decodeQp(data.getBytes(Latin1), header = true)
        if pendingCs != null && !pendingCs.equalsIgnoreCase(cs) then flushPending()
        pendingCs = cs
        pending.write(bytes)
        last = m.end
      }
      flushPending()
      if last < joined.length then sb.append(joined.substring(last))
      sb.toString

  /** ヘッダーの値を RFC 2047 で符号化する（ASCII だけならそのまま）。 */
  def encodeHeader(s: String): String =
    if s.forall(c => c >= 0x20 && c < 0x7f) then s
    else
      // 75 文字以内の符号化語に分ける（文字の途中で切らない）
      val words = ArrayBuffer.empty[String]
      val cur = new StringBuilder
      s.codePoints().toArray.foreach { cp =>
        val next = cur.toString + new String(Character.toChars(cp))
        if Base64.getEncoder.encodeToString(next.getBytes(StandardCharsets.UTF_8)).length > 60 && cur.nonEmpty then
          words += cur.toString; cur.clear()
        cur.appendAll(Character.toChars(cp))
      }
      if cur.nonEmpty then words += cur.toString
      words.map(w => "=?UTF-8?B?" + Base64.getEncoder.encodeToString(w.getBytes(StandardCharsets.UTF_8)) + "?=").mkString("\r\n ")
