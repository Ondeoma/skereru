package skereru.core

import java.nio.charset.StandardCharsets
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale
import scala.collection.mutable.ArrayBuffer

/** IMAP のコマンド行（リテラルを含めて組み立て済みのバイト列）を前から読む。 */
final class ImapReader(bytes: Array[Byte]) {
  private var pos = 0

  def remaining: Int = bytes.length - pos

  /** 読む位置（先読みして戻すため）。 */
  def position: Int = pos
  def rewind(p: Int): Unit = pos = p
  def atEnd: Boolean = {
    skipCrlf()
    pos >= bytes.length
  }

  private def skipCrlf(): Unit =
    while pos < bytes.length && (bytes(pos) == '\r' || bytes(pos) == '\n') do pos += 1

  private def peek: Int = if pos < bytes.length then bytes(pos) & 0xff else -1

  def sp(): Unit = {
    if peek != ' ' then throw ImapSyntax("expected SP")
    while peek == ' ' do pos += 1
  }

  def spOpt(): Boolean =
    if peek == ' ' then { while peek == ' ' do pos += 1; true }
    else false

  def end(): Unit = {
    while peek == ' ' do pos += 1
    skipCrlf()
    if pos < bytes.length then throw ImapSyntax("unexpected extra arguments")
  }

  def peekChar: Char = if pos < bytes.length then bytes(pos).toChar else '\u0000'

  def expect(c: Char): Unit = {
    if peek != c then throw ImapSyntax(s"expected '$c'")
    pos += 1
  }

  private def isAtomChar(c: Int): Boolean =
    c > 0x20 && c < 0x7f && c != '(' && c != ')' && c != '{' && c != '"' && c != '\\' && c != '%' && c != '*' &&
      c != ']'

  /** atom（「]」や「*」「%」を許すかは引数で）。 */
  def atom(extra: String = "", stop: String = ""): String = {
    val st = pos
    while pos < bytes.length && (isAtomChar(peek) || extra.indexOf(peek) >= 0) && stop.indexOf(peek) < 0 do pos += 1
    if pos == st then throw ImapSyntax("expected atom")
    new String(bytes, st, pos - st, StandardCharsets.ISO_8859_1)
  }

  /** タグ（+ を除く atom）。 */
  def tag(): String = {
    val t = atom()
    if t.contains('+') then throw ImapSyntax("invalid tag")
    t
  }

  def number(): Long = atom().toLongOption.filter(_ >= 0).getOrElse(throw ImapSyntax("expected number"))

  /** リテラル {n} / {n+} の中身。 */
  private def literal(): Array[Byte] = {
    expect('{')
    val st = pos
    while peek >= '0' && peek <= '9' do pos += 1
    val n = new String(
      bytes,
      st,
      pos - st,
      StandardCharsets.US_ASCII
    ).toIntOption.getOrElse(throw ImapSyntax("invalid literal"))
    if peek == '+' then pos += 1
    expect('}')
    if peek == '\r' then pos += 1
    expect('\n')
    if pos + n > bytes.length then throw ImapSyntax("literal is truncated")
    val out = java.util.Arrays.copyOfRange(bytes, pos, pos + n)
    pos += n
    out
  }

  private def quoted(): Array[Byte] = {
    expect('"')
    val out = new java.io.ByteArrayOutputStream()
    while peek != '"' do {
      if peek == -1 || peek == '\r' || peek == '\n' then throw ImapSyntax("unterminated quoted string")
      if peek == '\\' then pos += 1
      out.write(bytes(pos)); pos += 1
    }
    pos += 1
    out.toByteArray
  }

  /** astring のバイト列（atom・引用符・リテラル）。 */
  def astringBytes(): Array[Byte] = peek match {
    case '"' => quoted()
    case '{' => literal()
    case _ => atom("]").getBytes(StandardCharsets.ISO_8859_1)
  }

  /** astring（UTF-8 として読む）。 */
  def astring(): String = new String(astringBytes(), StandardCharsets.UTF_8)

  /** string（atom は不可）。 */
  def string(): String = peek match {
    case '"' | '{' => new String(astringBytes(), StandardCharsets.UTF_8)
    case _ => throw ImapSyntax("expected string")
  }

  /** メールボックス名（修正 UTF-7 を戻す。INBOX は大文字小文字を区別しない）。 */
  def mailbox(): String = {
    val raw = astring()
    val name = MUtf7.decode(raw)
    if name.equalsIgnoreCase("INBOX") then "INBOX" else name
  }

  /** LIST のパターン（% と * を含む）。 */
  def listMailbox(): String = peek match {
    case '"' | '{' => astring()
    case _ => atom("%*]")
  }

  /** ( … ) の中の atom の並び（フラグ）。 */
  def flagList(): List[String] =
    if peek == '(' then {
      pos += 1
      val out = ArrayBuffer.empty[String]
      while peek != ')' do {
        if peek == -1 then throw ImapSyntax("unterminated list")
        spOpt()
        if peek != ')' then out += atom("\\")
      }
      pos += 1
      out.toList
    } else List(atom("\\"))

  def seqSet(): SeqSet = SeqSet.parse(atom("*"))

  def hasMore: Boolean = {
    val save = pos
    val r = spOpt() && peek != -1 && peek != '\r' && peek != '\n'
    pos = save
    r
  }

  // ---------------------------------------------------------------- FETCH

  def fetchAtts(): List[FetchAtt] =
    if peek == '(' then {
      pos += 1
      val out = ArrayBuffer.empty[FetchAtt]
      while peek != ')' do {
        if peek == -1 then throw ImapSyntax("unterminated fetch list")
        spOpt()
        if peek != ')' then out ++= fetchAtt()
      }
      pos += 1
      out.toList
    } else fetchAtt()

  private def fetchAtt(): List[FetchAtt] = {
    val name = atom(stop = "[").toUpperCase
    name match {
      case "ALL" => List(FetchAtt.Flags, FetchAtt.InternalDate, FetchAtt.Rfc822Size, FetchAtt.Envelope)
      case "FAST" => List(FetchAtt.Flags, FetchAtt.InternalDate, FetchAtt.Rfc822Size)
      case "FULL" => List(FetchAtt.Flags, FetchAtt.InternalDate, FetchAtt.Rfc822Size, FetchAtt.Envelope, FetchAtt.Body)
      case "FLAGS" => List(FetchAtt.Flags)
      case "UID" => List(FetchAtt.Uid)
      case "INTERNALDATE" => List(FetchAtt.InternalDate)
      case "RFC822.SIZE" => List(FetchAtt.Rfc822Size)
      case "ENVELOPE" => List(FetchAtt.Envelope)
      case "BODYSTRUCTURE" => List(FetchAtt.BodyStructure)
      case "RFC822" => List(FetchAtt.Rfc822)
      case "RFC822.HEADER" => List(FetchAtt.Rfc822Header)
      case "RFC822.TEXT" => List(FetchAtt.Rfc822Text)
      case "MODSEQ" => List(FetchAtt.ModSeq)
      case "BODY" | "BODY.PEEK" =>
        if peek != '[' then
          if name == "BODY" then List(FetchAtt.Body) else throw ImapSyntax(s"$name needs a section")
        else {
          pos += 1
          val sec = section()
          expect(']')
          val partial =
            if peek == '<' then {
              pos += 1
              val st = pos
              while peek != '>' && peek != -1 do pos += 1
              val txt = new String(bytes, st, pos - st, StandardCharsets.US_ASCII)
              expect('>')
              txt.split('.') match {
                case Array(a, b) => Some((
                    a.toLongOption.getOrElse(throw ImapSyntax("partial")),
                    b.toLongOption.filter(_ > 0).getOrElse(throw ImapSyntax("partial"))
                  ))
                case _ => throw ImapSyntax("invalid partial")
              }
            } else None
          List(FetchAtt.BodySection(name.endsWith(".PEEK"), sec, partial))
        }
      case other => throw ImapSyntax(s"unknown fetch attribute: $other")
    }
  }

  private def section(): Section = {
    val path = ArrayBuffer.empty[Int]
    var kind = ""
    var fields = List.empty[String]
    var done = false
    while !done && peek != ']' do
      if peek >= '0' && peek <= '9' then {
        val st = pos
        while peek >= '0' && peek <= '9' do pos += 1
        path += new String(bytes, st, pos - st, StandardCharsets.US_ASCII).toInt
        if peek == '.' then pos += 1
      } else {
        val st = pos
        while peek != ']' && peek != ' ' && peek != -1 do pos += 1
        kind = new String(bytes, st, pos - st, StandardCharsets.US_ASCII).toUpperCase
        if !Set("HEADER", "TEXT", "MIME", "HEADER.FIELDS", "HEADER.FIELDS.NOT").contains(kind) then
          throw ImapSyntax(s"invalid section: $kind")
        if kind == "MIME" && path.isEmpty then throw ImapSyntax("MIME needs a part number")
        if kind.startsWith("HEADER.FIELDS") then {
          sp()
          expect('(')
          val fs = ArrayBuffer.empty[String]
          while peek != ')' do {
            if peek == -1 then throw ImapSyntax("unterminated header list")
            spOpt()
            if peek != ')' then fs += astring()
          }
          pos += 1
          fields = fs.toList
        }
        done = true
      }
    Section(path.toList, kind, fields)
  }

  // ---------------------------------------------------------------- SEARCH

  def searchKeys(): SearchKey = {
    val keys = ArrayBuffer.empty[SearchKey]
    keys += searchKey()
    while hasMore do {
      sp()
      keys += searchKey()
    }
    if keys.size == 1 then keys.head else SearchKey.And(keys.toList)
  }

  private val dateFmt = DateTimeFormatter.ofPattern("d-MMM-yyyy", Locale.ENGLISH)

  /** 日付（1-Feb-2024。月名の大文字小文字は問わない）。 */
  private def date(): LocalDate = {
    val s = new String(astringBytes(), StandardCharsets.US_ASCII)
    val norm = s.split('-') match {
      case Array(d, m, y) => s"$d-${m.toLowerCase.capitalize}-$y"
      case _ => s
    }
    scala.util.Try(LocalDate.parse(norm, dateFmt)).getOrElse(throw ImapSyntax(s"invalid date: $s"))
  }

  def searchKey(): SearchKey =
    if peek == '(' then {
      pos += 1
      val keys = ArrayBuffer.empty[SearchKey]
      while peek != ')' do {
        if peek == -1 then throw ImapSyntax("unterminated search list")
        spOpt()
        if peek != ')' then keys += searchKey()
      }
      pos += 1
      if keys.size == 1 then keys.head else SearchKey.And(keys.toList)
    } else if (peek >= '0' && peek <= '9') || peek == '*' then SearchKey.Seq(seqSet())
    else {
      val k = atom().toUpperCase
      def arg(): String = { sp(); astring() }
      def darg(): LocalDate = { sp(); date() }
      def flag(f: String, set: Boolean) = SearchKey.Flag(f, set)
      k match {
        case "ALL" => SearchKey.All
        case "ANSWERED" => flag("\\Answered", true)
        case "DELETED" => flag("\\Deleted", true)
        case "DRAFT" => flag("\\Draft", true)
        case "FLAGGED" => flag("\\Flagged", true)
        case "SEEN" => flag("\\Seen", true)
        case "UNANSWERED" => flag("\\Answered", false)
        case "UNDELETED" => flag("\\Deleted", false)
        case "UNDRAFT" => flag("\\Draft", false)
        case "UNFLAGGED" => flag("\\Flagged", false)
        case "UNSEEN" => flag("\\Seen", false)
        case "RECENT" => SearchKey.Recent
        case "NEW" => SearchKey.New
        case "OLD" => SearchKey.Old
        case "KEYWORD" => sp(); SearchKey.Keyword(atom(), true)
        case "UNKEYWORD" => sp(); SearchKey.Keyword(atom(), false)
        case "BCC" => SearchKey.HeaderContains("Bcc", arg())
        case "CC" => SearchKey.HeaderContains("Cc", arg())
        case "FROM" => SearchKey.HeaderContains("From", arg())
        case "TO" => SearchKey.HeaderContains("To", arg())
        case "SUBJECT" => SearchKey.HeaderContains("Subject", arg())
        case "HEADER" => val n = arg(); SearchKey.HeaderContains(n, arg())
        case "BODY" => SearchKey.Body(arg())
        case "TEXT" => SearchKey.Text(arg())
        case "BEFORE" => SearchKey.Before(darg())
        case "ON" => SearchKey.On(darg())
        case "SINCE" => SearchKey.Since(darg())
        case "SENTBEFORE" => SearchKey.SentBefore(darg())
        case "SENTON" => SearchKey.SentOn(darg())
        case "SENTSINCE" => SearchKey.SentSince(darg())
        case "LARGER" => sp(); SearchKey.Larger(number())
        case "SMALLER" => sp(); SearchKey.Smaller(number())
        case "UID" => sp(); SearchKey.Uid(seqSet())
        case "NOT" => sp(); SearchKey.Not(searchKey())
        case "OR" => sp(); val a = searchKey(); sp(); SearchKey.Or(a, searchKey())
        case "MODSEQ" => sp(); SearchKey.ModSeqAbove(number())
        case other => throw ImapSyntax(s"unknown search key: $other")
      }
    }
}
