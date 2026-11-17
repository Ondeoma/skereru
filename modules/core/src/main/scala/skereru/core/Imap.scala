package skereru.core

import java.nio.charset.StandardCharsets
import java.time.{LocalDate, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.collection.mutable.ArrayBuffer

/** IMAP の構文エラー（BAD で返す）。 */
final class ImapSyntax(msg: String) extends Exception(msg)

/** シーケンス集合（1:3,5,7:*）。-1 は「*」（最大の番号）。 */
final case class SeqSet(ranges: List[(Long, Long)]):
  private def resolve(n: Long, max: Long) = if n == -1 then max else n

  def contains(n: Long, max: Long): Boolean = ranges.exists { (a0, b0) =>
    val a = resolve(a0, max); val b = resolve(b0, max)
    n >= math.min(a, b) && n <= math.max(a, b)
  }

  /** 最大値までの番号の範囲（DB の問い合わせ用）。 */
  def bounds(max: Long): List[(Long, Long)] = ranges.map { (a0, b0) =>
    val a = resolve(a0, max); val b = resolve(b0, max)
    (math.min(a, b), math.max(a, b))
  }

  /** 「*」を含むか（UID の範囲で、最大の UID を必ず含めるため）。 */
  def hasStar: Boolean = ranges.exists((a, b) => a == -1 || b == -1)

object SeqSet:
  val MaxRanges = 2000
  def parse(s: String): SeqSet =
    if s.isEmpty then throw ImapSyntax("sequence set is empty")
    def num(x: String): Long =
      if x == "*" then -1
      else x.toLongOption.filter(_ > 0).getOrElse(throw ImapSyntax(s"invalid sequence number: $x"))
    // 範囲の数に上限を付ける（1,3,5,… を並べて、判定や SQL を巨大にさせない）
    if s.count(_ == ',') >= MaxRanges then throw ImapSyntax("too many ranges in sequence set")
    SeqSet(s.split(',').toList.map { p =>
      p.split(':') match
        case Array(a)    => (num(a), num(a))
        case Array(a, b) => (num(a), num(b))
        case _           => throw ImapSyntax(s"invalid sequence set: $s")
    })

  /** UID の並びを短く書く（1:3,5）。 */
  def format(nums: Seq[Long]): String =
    if nums.isEmpty then ""
    else
      val sorted = nums.distinct.sorted
      val out = ArrayBuffer.empty[String]
      var start = sorted.head
      var prev  = sorted.head
      sorted.tail.foreach { n =>
        if n == prev + 1 then prev = n
        else
          out += (if start == prev then s"$start" else s"$start:$prev"); start = n; prev = n
      }
      out += (if start == prev then s"$start" else s"$start:$prev")
      out.mkString(",")

/** BODY[…] の部品の指定。 */
final case class Section(path: List[Int], kind: String, fields: List[String]):
  def render: String =
    val p = path.mkString(".")
    val k = kind match
      case "" => ""
      case "HEADER.FIELDS" | "HEADER.FIELDS.NOT" => s"$kind (${fields.mkString(" ")})"
      case other => other
    if p.nonEmpty && k.nonEmpty then s"$p.$k" else p + k

enum FetchAtt:
  case Flags, Uid, InternalDate, Rfc822Size, Envelope, BodyStructure, Body, Rfc822, Rfc822Header, Rfc822Text, ModSeq
  case BodySection(peek: Boolean, section: Section, partial: Option[(Long, Long)])

  /** \Seen を付ける取得か（BODY[] や RFC822 など、PEEK でないもの）。 */
  def setsSeen: Boolean = this match
    case Rfc822 | Rfc822Text       => true
    case BodySection(peek, _, _)   => !peek
    case _                         => false

enum SearchKey:
  case All
  case Flag(flag: String, set: Boolean) // \Seen なら set=true、UNSEEN なら false
  case Keyword(k: String, set: Boolean)
  case Recent, New, Old
  case HeaderContains(name: String, value: String)
  case Body(text: String)
  case Text(text: String)
  case Before(d: LocalDate)
  case On(d: LocalDate)
  case Since(d: LocalDate)
  case SentBefore(d: LocalDate)
  case SentOn(d: LocalDate)
  case SentSince(d: LocalDate)
  case Larger(n: Long)
  case Smaller(n: Long)
  case Uid(set: SeqSet)
  case Seq(set: SeqSet)
  case Not(k: SearchKey)
  case Or(a: SearchKey, b: SearchKey)
  case And(keys: List[SearchKey])
  case ModSeqAbove(n: Long)

/** IMAP のコマンド行（リテラルを含めて組み立て済みのバイト列）を前から読む。 */
final class ImapReader(bytes: Array[Byte]):
  private var pos = 0

  def remaining: Int = bytes.length - pos
  /** 読む位置（先読みして戻すため）。 */
  def position: Int = pos
  def rewind(p: Int): Unit = pos = p
  def atEnd: Boolean =
    skipCrlf()
    pos >= bytes.length

  private def skipCrlf(): Unit =
    while pos < bytes.length && (bytes(pos) == '\r' || bytes(pos) == '\n') do pos += 1

  private def peek: Int = if pos < bytes.length then bytes(pos) & 0xff else -1

  def sp(): Unit =
    if peek != ' ' then throw ImapSyntax("expected SP")
    while peek == ' ' do pos += 1

  def spOpt(): Boolean =
    if peek == ' ' then { while peek == ' ' do pos += 1; true } else false

  def end(): Unit =
    while peek == ' ' do pos += 1
    skipCrlf()
    if pos < bytes.length then throw ImapSyntax("unexpected extra arguments")

  def peekChar: Char = if pos < bytes.length then bytes(pos).toChar else '\u0000'

  def expect(c: Char): Unit =
    if peek != c then throw ImapSyntax(s"expected '$c'")
    pos += 1

  private def isAtomChar(c: Int): Boolean =
    c > 0x20 && c < 0x7f && c != '(' && c != ')' && c != '{' && c != '"' && c != '\\' && c != '%' && c != '*' && c != ']'

  /** atom（「]」や「*」「%」を許すかは引数で）。 */
  def atom(extra: String = "", stop: String = ""): String =
    val st = pos
    while pos < bytes.length && (isAtomChar(peek) || extra.indexOf(peek) >= 0) && stop.indexOf(peek) < 0 do pos += 1
    if pos == st then throw ImapSyntax("expected atom")
    new String(bytes, st, pos - st, StandardCharsets.ISO_8859_1)

  /** タグ（+ を除く atom）。 */
  def tag(): String =
    val t = atom()
    if t.contains('+') then throw ImapSyntax("invalid tag")
    t

  def number(): Long = atom().toLongOption.filter(_ >= 0).getOrElse(throw ImapSyntax("expected number"))

  /** リテラル {n} / {n+} の中身。 */
  private def literal(): Array[Byte] =
    expect('{')
    val st = pos
    while peek >= '0' && peek <= '9' do pos += 1
    val n = new String(bytes, st, pos - st, StandardCharsets.US_ASCII).toIntOption.getOrElse(throw ImapSyntax("invalid literal"))
    if peek == '+' then pos += 1
    expect('}')
    if peek == '\r' then pos += 1
    expect('\n')
    if pos + n > bytes.length then throw ImapSyntax("literal is truncated")
    val out = java.util.Arrays.copyOfRange(bytes, pos, pos + n)
    pos += n
    out

  private def quoted(): Array[Byte] =
    expect('"')
    val out = new java.io.ByteArrayOutputStream()
    while peek != '"' do
      if peek == -1 || peek == '\r' || peek == '\n' then throw ImapSyntax("unterminated quoted string")
      if peek == '\\' then pos += 1
      out.write(bytes(pos)); pos += 1
    pos += 1
    out.toByteArray

  /** astring のバイト列（atom・引用符・リテラル）。 */
  def astringBytes(): Array[Byte] = peek match
    case '"' => quoted()
    case '{' => literal()
    case _   => atom("]").getBytes(StandardCharsets.ISO_8859_1)

  /** astring（UTF-8 として読む）。 */
  def astring(): String = new String(astringBytes(), StandardCharsets.UTF_8)

  /** string（atom は不可）。 */
  def string(): String = peek match
    case '"' | '{' => new String(astringBytes(), StandardCharsets.UTF_8)
    case _         => throw ImapSyntax("expected string")

  /** メールボックス名（修正 UTF-7 を戻す。INBOX は大文字小文字を区別しない）。 */
  def mailbox(): String =
    val raw = astring()
    val name = MUtf7.decode(raw)
    if name.equalsIgnoreCase("INBOX") then "INBOX" else name

  /** LIST のパターン（% と * を含む）。 */
  def listMailbox(): String = peek match
    case '"' | '{' => astring()
    case _         => atom("%*]")

  /** ( … ) の中の atom の並び（フラグ）。 */
  def flagList(): List[String] =
    if peek == '(' then
      pos += 1
      val out = ArrayBuffer.empty[String]
      while peek != ')' do
        if peek == -1 then throw ImapSyntax("unterminated list")
        spOpt()
        if peek != ')' then out += atom("\\")
      pos += 1
      out.toList
    else List(atom("\\"))

  def seqSet(): SeqSet = SeqSet.parse(atom("*"))

  def hasMore: Boolean =
    val save = pos
    val r = spOpt() && peek != -1 && peek != '\r' && peek != '\n'
    pos = save
    r

  // ---------------------------------------------------------------- FETCH

  def fetchAtts(): List[FetchAtt] =
    if peek == '(' then
      pos += 1
      val out = ArrayBuffer.empty[FetchAtt]
      while peek != ')' do
        if peek == -1 then throw ImapSyntax("unterminated fetch list")
        spOpt()
        if peek != ')' then out ++= fetchAtt()
      pos += 1
      out.toList
    else fetchAtt()

  private def fetchAtt(): List[FetchAtt] =
    val name = atom(stop = "[").toUpperCase
    name match
      case "ALL"           => List(FetchAtt.Flags, FetchAtt.InternalDate, FetchAtt.Rfc822Size, FetchAtt.Envelope)
      case "FAST"          => List(FetchAtt.Flags, FetchAtt.InternalDate, FetchAtt.Rfc822Size)
      case "FULL"          => List(FetchAtt.Flags, FetchAtt.InternalDate, FetchAtt.Rfc822Size, FetchAtt.Envelope, FetchAtt.Body)
      case "FLAGS"         => List(FetchAtt.Flags)
      case "UID"           => List(FetchAtt.Uid)
      case "INTERNALDATE"  => List(FetchAtt.InternalDate)
      case "RFC822.SIZE"   => List(FetchAtt.Rfc822Size)
      case "ENVELOPE"      => List(FetchAtt.Envelope)
      case "BODYSTRUCTURE" => List(FetchAtt.BodyStructure)
      case "RFC822"        => List(FetchAtt.Rfc822)
      case "RFC822.HEADER" => List(FetchAtt.Rfc822Header)
      case "RFC822.TEXT"   => List(FetchAtt.Rfc822Text)
      case "MODSEQ"        => List(FetchAtt.ModSeq)
      case "BODY" | "BODY.PEEK" =>
        if peek != '[' then
          if name == "BODY" then List(FetchAtt.Body) else throw ImapSyntax(s"$name needs a section")
        else
          pos += 1
          val sec = section()
          expect(']')
          val partial =
            if peek == '<' then
              pos += 1
              val st = pos
              while peek != '>' && peek != -1 do pos += 1
              val txt = new String(bytes, st, pos - st, StandardCharsets.US_ASCII)
              expect('>')
              txt.split('.') match
                case Array(a, b) => Some((a.toLongOption.getOrElse(throw ImapSyntax("partial")), b.toLongOption.filter(_ > 0).getOrElse(throw ImapSyntax("partial"))))
                case _           => throw ImapSyntax("invalid partial")
            else None
          List(FetchAtt.BodySection(name.endsWith(".PEEK"), sec, partial))
      case other => throw ImapSyntax(s"unknown fetch attribute: $other")

  private def section(): Section =
    val path = ArrayBuffer.empty[Int]
    var kind = ""
    var fields = List.empty[String]
    var done = false
    while !done && peek != ']' do
      if peek >= '0' && peek <= '9' then
        val st = pos
        while peek >= '0' && peek <= '9' do pos += 1
        path += new String(bytes, st, pos - st, StandardCharsets.US_ASCII).toInt
        if peek == '.' then pos += 1
      else
        val st = pos
        while peek != ']' && peek != ' ' && peek != -1 do pos += 1
        kind = new String(bytes, st, pos - st, StandardCharsets.US_ASCII).toUpperCase
        if !Set("HEADER", "TEXT", "MIME", "HEADER.FIELDS", "HEADER.FIELDS.NOT").contains(kind) then throw ImapSyntax(s"invalid section: $kind")
        if kind == "MIME" && path.isEmpty then throw ImapSyntax("MIME needs a part number")
        if kind.startsWith("HEADER.FIELDS") then
          sp()
          expect('(')
          val fs = ArrayBuffer.empty[String]
          while peek != ')' do
            if peek == -1 then throw ImapSyntax("unterminated header list")
            spOpt()
            if peek != ')' then fs += astring()
          pos += 1
          fields = fs.toList
        done = true
    Section(path.toList, kind, fields)

  // ---------------------------------------------------------------- SEARCH

  def searchKeys(): SearchKey =
    val keys = ArrayBuffer.empty[SearchKey]
    keys += searchKey()
    while hasMore do
      sp()
      keys += searchKey()
    if keys.size == 1 then keys.head else SearchKey.And(keys.toList)

  private val dateFmt = DateTimeFormatter.ofPattern("d-MMM-yyyy", Locale.ENGLISH)
  /** 日付（1-Feb-2024。月名の大文字小文字は問わない）。 */
  private def date(): LocalDate =
    val s = new String(astringBytes(), StandardCharsets.US_ASCII)
    val norm = s.split('-') match
      case Array(d, m, y) => s"$d-${m.toLowerCase.capitalize}-$y"
      case _              => s
    scala.util.Try(LocalDate.parse(norm, dateFmt)).getOrElse(throw ImapSyntax(s"invalid date: $s"))

  def searchKey(): SearchKey =
    if peek == '(' then
      pos += 1
      val keys = ArrayBuffer.empty[SearchKey]
      while peek != ')' do
        if peek == -1 then throw ImapSyntax("unterminated search list")
        spOpt()
        if peek != ')' then keys += searchKey()
      pos += 1
      if keys.size == 1 then keys.head else SearchKey.And(keys.toList)
    else if (peek >= '0' && peek <= '9') || peek == '*' then SearchKey.Seq(seqSet())
    else
      val k = atom().toUpperCase
      def arg(): String = { sp(); astring() }
      def darg(): LocalDate = { sp(); date() }
      def flag(f: String, set: Boolean) = SearchKey.Flag(f, set)
      k match
        case "ALL"        => SearchKey.All
        case "ANSWERED"   => flag("\\Answered", true)
        case "DELETED"    => flag("\\Deleted", true)
        case "DRAFT"      => flag("\\Draft", true)
        case "FLAGGED"    => flag("\\Flagged", true)
        case "SEEN"       => flag("\\Seen", true)
        case "UNANSWERED" => flag("\\Answered", false)
        case "UNDELETED"  => flag("\\Deleted", false)
        case "UNDRAFT"    => flag("\\Draft", false)
        case "UNFLAGGED"  => flag("\\Flagged", false)
        case "UNSEEN"     => flag("\\Seen", false)
        case "RECENT"     => SearchKey.Recent
        case "NEW"        => SearchKey.New
        case "OLD"        => SearchKey.Old
        case "KEYWORD"    => sp(); SearchKey.Keyword(atom(), true)
        case "UNKEYWORD"  => sp(); SearchKey.Keyword(atom(), false)
        case "BCC"        => SearchKey.HeaderContains("Bcc", arg())
        case "CC"         => SearchKey.HeaderContains("Cc", arg())
        case "FROM"       => SearchKey.HeaderContains("From", arg())
        case "TO"         => SearchKey.HeaderContains("To", arg())
        case "SUBJECT"    => SearchKey.HeaderContains("Subject", arg())
        case "HEADER"     => val n = arg(); SearchKey.HeaderContains(n, arg())
        case "BODY"       => SearchKey.Body(arg())
        case "TEXT"       => SearchKey.Text(arg())
        case "BEFORE"     => SearchKey.Before(darg())
        case "ON"         => SearchKey.On(darg())
        case "SINCE"      => SearchKey.Since(darg())
        case "SENTBEFORE" => SearchKey.SentBefore(darg())
        case "SENTON"     => SearchKey.SentOn(darg())
        case "SENTSINCE"  => SearchKey.SentSince(darg())
        case "LARGER"     => sp(); SearchKey.Larger(number())
        case "SMALLER"    => sp(); SearchKey.Smaller(number())
        case "UID"        => sp(); SearchKey.Uid(seqSet())
        case "NOT"        => sp(); SearchKey.Not(searchKey())
        case "OR"         => sp(); val a = searchKey(); sp(); SearchKey.Or(a, searchKey())
        case "MODSEQ"     => sp(); SearchKey.ModSeqAbove(number())
        case other        => throw ImapSyntax(s"unknown search key: $other")

/** IMAP の応答の整形（文字列の引用・ENVELOPE・BODYSTRUCTURE・日時）。 */
object ImapFormat:

  /** 文字列を引用符で囲む。改行・8bit を含むならリテラルにする（呼び出し側でバイト列として送る）。 */
  def quote(s: String): String =
    if s.exists(c => c == '\r' || c == '\n' || c > 0x7e || c < 0x20) then
      val b = s.getBytes(StandardCharsets.UTF_8)
      s"{${b.length}}\r\n" + new String(b, StandardCharsets.ISO_8859_1)
    else "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** ヘッダーの生の値（ISO-8859-1 で読んだもの）を nstring にする。 */
  def nstringRaw(s: Option[String]): String = s match
    case None => "NIL"
    case Some(v) =>
      if v.exists(c => c == '\r' || c == '\n' || c > 0x7e || c < 0x20 || c == '"' || c == '\\') then
        s"{${v.length}}\r\n$v" // ISO-8859-1 の文字列はそのままバイト数
      else "\"" + v + "\""

  def nstring(s: Option[String]): String = s.fold("NIL")(quote)

  def astring(s: String): String =
    if s.nonEmpty && s.forall(c => c > 0x20 && c < 0x7f && !"(){ %*\"\\]".contains(c)) then s else quote(s)

  /** メールボックス名（修正 UTF-7 にして引用する）。 */
  def mailbox(name: String): String = quote(MUtf7.encode(name))

  private val internalFmt = DateTimeFormatter.ofPattern("dd-MMM-yyyy HH:mm:ss Z", Locale.ENGLISH)
  def internalDate(t: java.time.Instant, zone: java.time.ZoneId): String = "\"" + internalFmt.format(t.atZone(zone)) + "\""

  /** APPEND の日時（"17-Jul-1996 02:44:25 -0700"）。 */
  def parseInternalDate(s: String): Option[java.time.Instant] =
    scala.util.Try(ZonedDateTime.parse(s.trim.replaceFirst("^ ", "0"), internalFmt).toInstant)
      .orElse(scala.util.Try(ZonedDateTime.parse(s.trim, DateTimeFormatter.ofPattern("d-MMM-yyyy HH:mm:ss Z", Locale.ENGLISH)).toInstant)).toOption

  private def addressList(raw: Option[String]): String =
    raw.map(Address.parseGroups).filter(_.nonEmpty) match
      case None => "NIL"
      case Some(items) =>
        def one(a: MailAddress) =
          s"(${nstringRaw(a.name)} NIL ${nstringRaw(Some(a.local))} ${if a.domain.isEmpty then "NIL" else nstringRaw(Some(a.domain))})"
        "(" + items.map {
          case Left(a)  => one(a)
          case Right(g) => s"(NIL NIL ${nstringRaw(Some(g.name))} NIL)" + g.members.map(one).mkString + "(NIL NIL NIL NIL)"
        }.mkString + ")"

  def envelope(p: MimePart): String =
    def h(n: String) = p.header(n)
    val from = h("From")
    List(
      nstringRaw(h("Date")), nstringRaw(h("Subject")), addressList(from),
      addressList(h("Sender").orElse(from)), addressList(h("Reply-To").orElse(from)),
      addressList(h("To")), addressList(h("Cc")), addressList(h("Bcc")),
      nstringRaw(h("In-Reply-To")), nstringRaw(h("Message-ID"))
    ).mkString("(", " ", ")")

  private def params(pv: ParamValue): String =
    if pv.params.isEmpty then "NIL"
    else pv.params.toList.sortBy(_._1).map((k, v) => s"${quote(k.toUpperCase)} ${quote(v)}").mkString("(", " ", ")")

  /** BODY / BODYSTRUCTURE（extended=true で拡張データつき）。 */
  def bodyStructure(msg: Array[Byte], p: MimePart, extended: Boolean): String =
    if p.isMultipart then
      val kids = if p.children.isEmpty then "(\"text\" \"plain\" NIL NIL NIL \"7bit\" 0 0)" else p.children.map(bodyStructure(msg, _, extended)).mkString
      val ext = if extended then s" ${params(ParamValue("", p.contentType.params))} ${disp(p)} NIL NIL" else ""
      s"($kids ${quote(p.subType)}$ext)"
    else
      val base = List(
        quote(p.mainType), quote(p.subType), params(p.contentType),
        nstringRaw(p.header("Content-ID")), nstringRaw(p.header("Content-Description")),
        quote(p.encoding.toUpperCase match { case "7BIT" | "8BIT" | "BINARY" | "BASE64" | "QUOTED-PRINTABLE" => p.encoding.toUpperCase; case o => o }),
        p.size.toString
      )
      val typeSpecific =
        if p.isMessage then p.message.toList.flatMap(m => List(envelope(m), bodyStructure(msg, m, extended), p.bodyLines.toString))
        else if p.mainType == "text" then List(p.bodyLines.toString)
        else Nil
      val ext = if extended then List("NIL", disp(p), "NIL", "NIL") else Nil
      (base ++ typeSpecific ++ ext).mkString("(", " ", ")")

  private def disp(p: MimePart): String = p.disposition match
    case None     => "NIL"
    case Some(pv) => s"(${quote(pv.value.toLowerCase)} ${params(pv)})"

  /** BODY[section] の中身（バイト列）。部品が存在しなければ空。 */
  def sectionBytes(msg: Array[Byte], root: MimePart, s: Section): Array[Byte] =
    def slice(a: Int, b: Int) = java.util.Arrays.copyOfRange(msg, a, math.max(a, b))
    // 部品番号の先の対象（HEADER・TEXT は message/rfc822 の中身のメッセージ）
    root.part(s.path) match
      case None => Array.emptyByteArray
      case Some(p0) =>
        val p = if s.path.nonEmpty && Set("HEADER", "TEXT", "HEADER.FIELDS", "HEADER.FIELDS.NOT")(s.kind) then p0.message.getOrElse(p0) else p0
        s.kind match
          case "" =>
            if s.path.isEmpty then msg
            else slice(p.bodyStart, p.end)
          case "HEADER" => slice(p.start, p.bodyStart)
          case "TEXT"   => slice(p.bodyStart, p.end)
          case "MIME"   => slice(p0.start, p0.bodyStart)
          case "HEADER.FIELDS" | "HEADER.FIELDS.NOT" =>
            val want = s.fields.map(_.toLowerCase).toSet
            val not  = s.kind.endsWith(".NOT")
            val text = Mime.latin1(msg, p.start, p.bodyStart)
            // 折り返しを含む元の行のまま取り出す
            val out = new StringBuilder
            var keep = false
            text.split("(?<=\n)").foreach { line =>
              if line.startsWith(" ") || line.startsWith("\t") then { if keep then out.append(line) }
              else if line.trim.isEmpty then ()
              else
                val n = line.takeWhile(_ != ':').trim.toLowerCase
                keep = want.contains(n) != not
                if keep then out.append(line)
            }
            out.append("\r\n")
            out.toString.getBytes(StandardCharsets.ISO_8859_1)
          case _ => Array.emptyByteArray

/** IMAP のメールボックス名の修正 UTF-7（RFC 3501 5.1.3）。 */
object MUtf7:
  def encode(s: String): String =
    val out = new StringBuilder
    val buf = new StringBuilder
    def flush(): Unit = if buf.nonEmpty then
      val b = buf.toString.getBytes(StandardCharsets.UTF_16BE)
      out.append('&').append(java.util.Base64.getEncoder.withoutPadding().encodeToString(b).replace('/', ',')).append('-')
      buf.clear()
    s.foreach { c =>
      if c >= 0x20 && c <= 0x7e then
        flush()
        if c == '&' then out.append("&-") else out.append(c)
      else buf.append(c)
    }
    flush()
    out.toString

  def decode(s: String): String =
    val out = new StringBuilder
    var i = 0
    while i < s.length do
      if s(i) == '&' then
        val j = s.indexOf('-', i)
        if j < 0 then { out.append(s.substring(i)); i = s.length }
        else if j == i + 1 then { out.append('&'); i = j + 1 }
        else
          val enc = s.substring(i + 1, j).replace(',', '/')
          scala.util.Try(java.util.Base64.getDecoder.decode(enc + "=" * ((4 - enc.length % 4) % 4))) match
            case scala.util.Success(b) => out.append(new String(b, StandardCharsets.UTF_16BE))
            case _                     => out.append(s.substring(i, j + 1))
          i = j + 1
      else { out.append(s(i)); i += 1 }
    out.toString
