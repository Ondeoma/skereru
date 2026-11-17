package skereru.core

import scala.collection.mutable.ArrayBuffer

/** メールアドレス 1 つ。name は表示名（ヘッダーに書かれたまま。RFC 2047 の符号化は残す）。 */
final case class MailAddress(name: Option[String], local: String, domain: String):
  def address: String = if domain.isEmpty then local else s"$local@$domain"
  def nameDecoded: Option[String] = name.map(Mime.decodeHeader).map(_.trim).filter(_.nonEmpty)
  /** 表示用（"山田 太郎 <taro@example.com>"）。 */
  def display: String = nameDecoded.fold(address)(n => s"$n <$address>")
  def lower: String = Address.normalize(address)

/** グループ（"group: a, b;"）を含むアドレスの並び。 */
final case class AddressGroup(name: String, members: List[MailAddress])

object Address:

  /** 比較用の正規化（ドメインは小文字。ローカル部は大文字小文字を区別しない運用にする）。 */
  def normalize(addr: String): String = addr.trim.toLowerCase

  def domainOf(addr: String): String = addr.lastIndexOf('@') match
    case -1 => ""
    case i  => addr.substring(i + 1).toLowerCase.stripSuffix(".")

  def localOf(addr: String): String = addr.lastIndexOf('@') match
    case -1 => addr
    case i  => addr.substring(0, i)

  /** user+tag@example.com の +tag を除いたもの（サブアドレス）。 */
  def withoutTag(addr: String): String =
    val l = localOf(addr)
    val i = l.indexOf('+')
    if i > 0 then s"${l.substring(0, i)}@${domainOf(addr)}" else addr

  private val LocalAtom = """[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+(\.[A-Za-z0-9!#$%&'*+/=?^_`{|}~-]+)*""".r
  private val Label = """[A-Za-z0-9]([A-Za-z0-9-]{0,61}[A-Za-z0-9])?""".r

  /** ドメイン名として正しいか（IDN は事前に punycode にしておくこと）。 */
  def validDomain(d: String): Boolean =
    val s = d.stripSuffix(".")
    s.length <= 253 && s.contains('.') && s.split('.').forall(l => Label.matches(l)) && !s.split('.').last.forall(_.isDigit)

  /** 受け付けるアドレスの形（ドット・アトムのローカル部 + ドメイン。引用符つきのローカル部は受け付けない）。 */
  def valid(addr: String): Boolean =
    val i = addr.lastIndexOf('@')
    i > 0 && i < addr.length - 1 && addr.length <= 254 && i <= 64 && LocalAtom.matches(addr.substring(0, i)) && validDomain(addr.substring(i + 1))

  /** IDN（日本語ドメイン）を punycode にする。 */
  def toAscii(addr: String): String =
    val i = addr.lastIndexOf('@')
    if i < 0 then addr
    else scala.util.Try(addr.substring(0, i + 1) + java.net.IDN.toASCII(addr.substring(i + 1), java.net.IDN.ALLOW_UNASSIGNED)).getOrElse(addr)

  /** ヘッダーの値（To: など）を解析する。グループは展開して返す。壊れた部分は読み飛ばす。 */
  def parseList(raw: String): List[MailAddress] = parseGroups(raw).flatMap {
    case Left(a)  => List(a)
    case Right(g) => g.members
  }

  /** 1 つだけのアドレス（From など）。 */
  def parseOne(raw: String): Option[MailAddress] = parseList(raw).headOption

  /** アドレスとグループを区別して返す（IMAP の ENVELOPE はグループを区別して書く）。 */
  def parseGroups(raw: String): List[Either[MailAddress, AddressGroup]] =
    val toks = tokenize(raw)
    val out  = ArrayBuffer.empty[Either[MailAddress, AddressGroup]]
    var i    = 0
    // 1 つのアドレス（", " か ";" の手前まで）を読む
    def readMailbox(stopAtSemicolon: Boolean): Option[MailAddress] =
      val start = i
      while i < toks.length && !(toks(i) == Tok.Sym(',') || (stopAtSemicolon && toks(i) == Tok.Sym(';'))) do i += 1
      mailbox(toks.slice(start, i).toList)
    while i < toks.length do
      // グループ: 語の並び ':' … ';'
      val colon = (i until toks.length).takeWhile(k => toks(k) != Tok.Sym(',') && toks(k) != Tok.Sym('<')).find(k => toks(k) == Tok.Sym(':'))
      colon match
        case Some(c) if toks.slice(i, c).forall(t => t.isInstanceOf[Tok.Word] || t.isInstanceOf[Tok.Quoted]) =>
          val name = toks.slice(i, c).map(_.text).mkString(" ")
          i = c + 1
          val members = ArrayBuffer.empty[MailAddress]
          while i < toks.length && toks(i) != Tok.Sym(';') do
            if toks(i) == Tok.Sym(',') then i += 1
            else readMailbox(stopAtSemicolon = true).foreach(members += _)
          if i < toks.length then i += 1 // ';'
          out += Right(AddressGroup(name, members.toList))
        case _ =>
          if toks(i) == Tok.Sym(',') then i += 1
          else readMailbox(stopAtSemicolon = false).foreach(a => out += Left(a))
    out.toList

  private def mailbox(ts: List[Tok]): Option[MailAddress] =
    val lt = ts.indexOf(Tok.Sym('<'))
    if lt >= 0 then
      val gt   = ts.indexOf(Tok.Sym('>'), lt)
      val spec = ts.slice(lt + 1, if gt < 0 then ts.length else gt)
      // ルート（<@a,@b:user@host>）は読み飛ばす
      val spec2 = spec.lastIndexOf(Tok.Sym(':')) match
        case -1 => spec
        case k  => spec.drop(k + 1)
      val name = ts.take(lt).map(_.text).mkString(" ").trim
      addrSpec(spec2).map((l, d) => MailAddress(Option(name).filter(_.nonEmpty), l, d))
    else
      // 括弧のコメントは表示名として使う（"taro@example.com (山田 太郎)"）
      val comment = ts.collect { case Tok.Comment(c) => c }.headOption
      addrSpec(ts.filterNot(_.isInstanceOf[Tok.Comment])).map((l, d) => MailAddress(comment.filter(_.nonEmpty), l, d))

  private def addrSpec(ts: List[Tok]): Option[(String, String)] =
    val clean = ts.filterNot(_.isInstanceOf[Tok.Comment])
    val at    = clean.lastIndexOf(Tok.Sym('@'))
    val text  = (xs: List[Tok]) => xs.map {
      case Tok.Quoted(q) => "\"" + q.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
      case t             => t.text
    }.mkString
    if at < 0 then
      val l = text(clean).trim
      Option.when(l.nonEmpty)((l, ""))
    else
      val l = text(clean.take(at)).trim
      // ドメインには記号（";" など）を含めず、末尾の "." も除く（"a@ours.example." を ours.example と同じに扱う）
      val d = text(clean.drop(at + 1).filterNot(_.isInstanceOf[Tok.Sym])).trim.toLowerCase.stripSuffix(".")
      Option.when(l.nonEmpty || d.nonEmpty)((l, d))

  private enum Tok:
    case Word(w: String)
    case Quoted(q: String)
    case Comment(c: String)
    case Sym(c: Char)
    def text: String = this match
      case Word(w)    => w
      case Quoted(q)  => q
      case Comment(c) => c
      case Sym(c)     => c.toString

  private def tokenize(s: String): Vector[Tok] =
    val out = ArrayBuffer.empty[Tok]
    var i = 0
    val specials = Set('<', '>', '@', ',', ';', ':')
    while i < s.length do
      val c = s(i)
      if c == '"' then
        val sb = new StringBuilder
        i += 1
        while i < s.length && s(i) != '"' do
          if s(i) == '\\' && i + 1 < s.length then i += 1
          sb.append(s(i)); i += 1
        i += 1
        out += Tok.Quoted(sb.toString)
      else if c == '(' then
        val sb = new StringBuilder
        var depth = 1
        i += 1
        while i < s.length && depth > 0 do
          if s(i) == '\\' && i + 1 < s.length then { i += 1; sb.append(s(i)) }
          else if s(i) == '(' then { depth += 1; sb.append('(') }
          else if s(i) == ')' then { depth -= 1; if depth > 0 then sb.append(')') }
          else sb.append(s(i))
          i += 1
        out += Tok.Comment(sb.toString.trim)
      else if specials(c) then { out += Tok.Sym(c); i += 1 }
      else if c.isWhitespace then i += 1
      else
        val st = i
        while i < s.length && !specials(s(i)) && !s(i).isWhitespace && s(i) != '"' && s(i) != '(' do i += 1
        out += Tok.Word(s.substring(st, i))
    out.toVector
