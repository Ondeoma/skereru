package skereru.core

import java.net.{Inet4Address, Inet6Address, InetAddress}
import java.nio.charset.StandardCharsets
import java.security.{KeyFactory, MessageDigest, PrivateKey, PublicKey, Signature}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64
import scala.collection.mutable.ArrayBuffer

/** DNS の問い合わせ（SPF・DKIM・DMARC・DNSBL・MX）。名前がない・レコードがないときは Right(Nil)、一時的な失敗は Left。 */
trait Dns:
  def txt(name: String): Either[String, List[String]]
  def addresses(name: String): Either[String, List[InetAddress]]
  def mx(name: String): Either[String, List[(Int, String)]]
  def ptr(ip: InetAddress): Either[String, List[String]]

enum SpfResult:
  case Pass, Fail, SoftFail, Neutral, None, TempError, PermError
  def label: String = this.toString.toLowerCase

/** SPF（RFC 7208）の評価。 */
final class Spf(dns: Dns):

  private final class Ctx(val ip: InetAddress, val sender: String, val helo: String):
    var lookups = 0
    var voids   = 0
    /** 評価全体にかけてよい時間（mx の各ホストの問い合わせは回数に数えないので、遅い DNS で長く止められないように）。 */
    val deadline = System.currentTimeMillis() + 20_000

  private val Modifier = "^[A-Za-z][A-Za-z0-9_.-]*=".r

  private final class Abort(val r: SpfResult) extends Exception(null, null, false, false)

  /** sender は MAIL FROM（空なら postmaster@helo）。 */
  def check(ip: InetAddress, sender: String, helo: String): SpfResult =
    val s = if sender.isEmpty then s"postmaster@$helo" else sender
    val ctx = Ctx(ip, s, helo)
    try checkHost(ctx, Address.domainOf(s), 0)
    catch case a: Abort => a.r

  private def countLookup(ctx: Ctx): Unit =
    if System.currentTimeMillis() > ctx.deadline then throw Abort(SpfResult.TempError)
    ctx.lookups += 1
    if ctx.lookups > 10 then throw Abort(SpfResult.PermError)

  private def void(ctx: Ctx, empty: Boolean): Unit =
    if empty then
      ctx.voids += 1
      if ctx.voids > 2 then throw Abort(SpfResult.PermError)

  private def record(domain: String): Either[SpfResult, Option[String]] =
    dns.txt(domain) match
      case Left(_) => Left(SpfResult.TempError)
      case Right(txts) =>
        txts.filter(t => t.equalsIgnoreCase("v=spf1") || t.toLowerCase.startsWith("v=spf1 ")) match
          case Nil      => Right(None)
          case r :: Nil => Right(Some(r))
          case _        => Left(SpfResult.PermError)

  private def checkHost(ctx: Ctx, domain: String, depth: Int): SpfResult =
    if depth > 10 || domain.isEmpty || domain.length > 253 || domain.split('.').exists(_.isEmpty) then return SpfResult.None
    record(domain) match
      case Left(r)     => r
      case Right(None) => SpfResult.None
      case Right(Some(rec)) =>
        val terms = rec.split("\\s+").toList.drop(1).filter(_.nonEmpty)
        val redirect = terms.collectFirst { case t if t.toLowerCase.startsWith("redirect=") => t.drop(9) }
        val directives = terms.filterNot(t => Modifier.findPrefixOf(t).isDefined)
        val it = directives.iterator
        while it.hasNext do
          val d = it.next()
          val (q, mech) = d.head match
            case '+' => (SpfResult.Pass, d.tail)
            case '-' => (SpfResult.Fail, d.tail)
            case '~' => (SpfResult.SoftFail, d.tail)
            case '?' => (SpfResult.Neutral, d.tail)
            case _   => (SpfResult.Pass, d)
          if matches(ctx, domain, mech, depth) then return q
        redirect match
          case Some(r) =>
            countLookup(ctx)
            checkHost(ctx, expand(r, ctx, domain), depth + 1) match
              case SpfResult.None => SpfResult.PermError
              case other          => other
          case None => SpfResult.Neutral

  private def splitCidr(s: String): (String, Option[Int], Option[Int]) =
    // domain/24//64
    val i6 = s.indexOf("//")
    val (rest, c6) = if i6 >= 0 then (s.substring(0, i6), s.substring(i6 + 2).toIntOption) else (s, None)
    val i4 = rest.lastIndexOf('/')
    if i4 >= 0 then (rest.substring(0, i4), rest.substring(i4 + 1).toIntOption, c6) else (rest, None, c6)

  private def matches(ctx: Ctx, domain: String, mech0: String, depth: Int): Boolean =
    val lower = mech0.toLowerCase
    val (name, arg) = lower.indexOf(':') match
      case -1 => lower.indexOf('/') match
          case -1 => (lower, "")
          case k  => (lower.substring(0, k), mech0.substring(k))
      case k => (lower.substring(0, k), mech0.substring(k + 1))
    name match
      case "all" => true
      case "include" =>
        countLookup(ctx)
        checkHost(ctx, expand(arg, ctx, domain), depth + 1) match
          case SpfResult.Pass                         => true
          case SpfResult.TempError                    => throw Abort(SpfResult.TempError)
          case SpfResult.PermError | SpfResult.None   => throw Abort(SpfResult.PermError)
          case _                                      => false
      case "a" =>
        countLookup(ctx)
        val (d, c4, c6) = splitCidr(if arg.startsWith("/") then domain + arg else if arg.isEmpty then domain else arg)
        ipsOf(ctx, expand(d, ctx, domain)).exists(inCidr(ctx.ip, _, c4, c6))
      case "mx" =>
        countLookup(ctx)
        val (d, c4, c6) = splitCidr(if arg.startsWith("/") then domain + arg else if arg.isEmpty then domain else arg)
        dns.mx(expand(d, ctx, domain)) match
          case Left(_) => throw Abort(SpfResult.TempError)
          case Right(mxs) =>
            void(ctx, mxs.isEmpty)
            if mxs.size > 10 then throw Abort(SpfResult.PermError)
            mxs.exists((_, h) => ipsOf(ctx, h).exists(inCidr(ctx.ip, _, c4, c6)))
      case "ip4" | "ip6" =>
        val (a, c4, c6) = splitCidr(arg)
        val bits = if name == "ip4" then c4 else c4.orElse(c6)
        scala.util.Try(InetAddress.getByName(a)).toOption.exists(net => inCidr(ctx.ip, net, bits, bits))
      case "exists" =>
        countLookup(ctx)
        dns.addresses(expand(arg, ctx, domain)) match
          case Left(_)   => throw Abort(SpfResult.TempError)
          case Right(as) => void(ctx, as.isEmpty); as.exists(_.isInstanceOf[Inet4Address])
      case "ptr" =>
        countLookup(ctx)
        val target = if arg.isEmpty then domain else expand(arg, ctx, domain)
        dns.ptr(ctx.ip).getOrElse(Nil).take(10).exists { n =>
          val host = n.stripSuffix(".").toLowerCase
          (host == target || host.endsWith("." + target)) && ipsOf(ctx, host).contains(ctx.ip)
        }
      case _ => throw Abort(SpfResult.PermError)

  private def ipsOf(ctx: Ctx, host: String): List[InetAddress] =
    if System.currentTimeMillis() > ctx.deadline then throw Abort(SpfResult.TempError)
    dns.addresses(host) match
      case Left(_)   => throw Abort(SpfResult.TempError)
      case Right(as) => void(ctx, as.isEmpty); as

  private def inCidr(ip: InetAddress, net: InetAddress, c4: Option[Int], c6: Option[Int]): Boolean =
    (ip, net) match
      case (_: Inet4Address, _: Inet4Address) => Cidr.contains(net, c4.getOrElse(32), ip)
      case (_: Inet6Address, _: Inet6Address) => Cidr.contains(net, c6.getOrElse(128), ip)
      case _                                  => false

  /** マクロ（%{s} %{l} %{o} %{d} %{i} %{h} %{v}）の展開。 */
  private def expand(spec: String, ctx: Ctx, domain: String): String =
    if !spec.contains('%') then return spec.toLowerCase
    val out = new StringBuilder
    var i = 0
    while i < spec.length do
      spec(i) match
        case '%' if i + 1 < spec.length && spec(i + 1) == '%' => out.append('%'); i += 2
        case '%' if i + 1 < spec.length && spec(i + 1) == '_' => out.append(' '); i += 2
        case '%' if i + 1 < spec.length && spec(i + 1) == '-' => out.append("%20"); i += 2
        case '%' if i + 1 < spec.length && spec(i + 1) == '{' =>
          val j = spec.indexOf('}', i)
          if j < 0 then throw Abort(SpfResult.PermError)
          val body = spec.substring(i + 2, j)
          val letter = body.headOption.map(_.toLower).getOrElse(throw Abort(SpfResult.PermError))
          val rest = body.drop(1)
          val digits = rest.takeWhile(_.isDigit).toIntOption
          val reverse = rest.dropWhile(_.isDigit).headOption.exists(_.toLower == 'r')
          val delims = rest.dropWhile(_.isDigit).dropWhile(c => c.toLower == 'r')
          val value = letter match
            case 's' => ctx.sender
            case 'l' => Address.localOf(ctx.sender)
            case 'o' => Address.domainOf(ctx.sender)
            case 'd' => domain
            case 'h' => ctx.helo
            case 'v' => if ctx.ip.isInstanceOf[Inet4Address] then "in-addr" else "ip6"
            case 'p' => "unknown"
            case 'i' => ctx.ip match
                case a: Inet4Address => a.getHostAddress
                case a => a.getAddress.flatMap(b => List((b >> 4) & 0xf, b & 0xf)).map(Integer.toHexString).mkString(".")
            case _ => throw Abort(SpfResult.PermError)
          val ds = if delims.isEmpty then "." else delims
          val parts0 = value.split(ds.map(c => "\\" + c).mkString("[", "", "]")).toList
          val parts1 = if reverse then parts0.reverse else parts0
          val parts2 = digits.fold(parts1)(n => parts1.takeRight(math.max(1, n)))
          out.append(parts2.mkString("."))
          i = j + 1
        case c => out.append(c); i += 1
    out.toString.toLowerCase

/** CIDR の判定。 */
object Cidr:
  def contains(net: InetAddress, bits: Int, ip: InetAddress): Boolean =
    val a = net.getAddress; val b = ip.getAddress
    if a.length != b.length || bits < 0 || bits > a.length * 8 then false
    else
      val full = bits / 8; val rest = bits % 8
      (0 until full).forall(i => a(i) == b(i)) &&
        (rest == 0 || ((a(full) ^ b(full)) & (0xff << (8 - rest)) & 0xff) == 0)

  /** "192.0.2.0/24" や "2001:db8::/32" や単独のアドレス。 */
  def parse(s: String): Option[(InetAddress, Int)] =
    val (a, b) = s.trim.split('/') match
      case Array(x)    => (x, None)
      case Array(x, y) => (x, y.toIntOption)
      case _           => ("", None)
    if a.isEmpty || !a.forall(c => c.isLetterOrDigit || c == '.' || c == ':') || a.exists(c => c.isLetter && !"abcdefABCDEF".contains(c)) then None
    else scala.util.Try(InetAddress.getByName(a)).toOption.map(ip => ip -> b.getOrElse(ip.getAddress.length * 8))

/** ヘッダーの元の行（折り返しを含む。DKIM の simple の正規化に使う）。 */
final case class RawHeader(name: String, raw: String)

object RawHeaders:
  def split(msg: Array[Byte]): (List[RawHeader], Int) =
    val end  = Mime.headerEnd(msg, 0, msg.length)
    val text = Mime.latin1(msg, 0, end)
    val out = ArrayBuffer.empty[RawHeader]
    val lines = text.split("(?<=\n)")
    lines.foreach { line =>
      if (line.startsWith(" ") || line.startsWith("\t")) && out.nonEmpty then
        val last = out.remove(out.length - 1)
        out += last.copy(raw = last.raw + line)
      else if line.trim.nonEmpty && line.contains(':') then out += RawHeader(line.takeWhile(_ != ':').trim, line)
    }
    (out.toList, end)

  /** keep に合うヘッダーだけを残したメッセージ（本文はそのまま）。何も消さなければ元のバイト列を返す。 */
  def filter(raw: Array[Byte])(keep: RawHeader => Boolean): Array[Byte] =
    val (hs, bodyStart) = split(raw)
    val kept = hs.filter(keep)
    if kept.size == hs.size then raw
    else kept.map(_.raw).mkString.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1) ++ "\r\n".getBytes ++ raw.drop(bodyStart)

final case class DkimResult(domain: String, selector: String, result: String, reason: String, aligned: Boolean = false):
  def pass: Boolean = result == "pass"

/** DKIM（RFC 6376・RFC 8463）の検証と署名。メッセージの改行は CRLF であること。 */
final class Dkim(dns: Dns):

  def tags(s: String): Map[String, String] =
    s.split(';').toList.flatMap { kv =>
      val i = kv.indexOf('=')
      Option.when(i > 0)(kv.substring(0, i).trim.toLowerCase -> kv.substring(i + 1).replaceAll("[ \t\r\n]", "").trim)
    }.toMap

  def verify(msg: Array[Byte], now: java.time.Instant): List[DkimResult] =
    val (headers, bodyStart) = RawHeaders.split(msg)
    headers.filter(_.name.equalsIgnoreCase("DKIM-Signature")).take(5).map(sig => verifyOne(msg, headers, bodyStart, sig, now))

  private def verifyOne(msg: Array[Byte], headers: List[RawHeader], bodyStart: Int, sig: RawHeader, now: java.time.Instant): DkimResult =
    val value = sig.raw.substring(sig.raw.indexOf(':') + 1)
    val t = tags(value)
    val d = t.getOrElse("d", "").toLowerCase
    val s = t.getOrElse("s", "")
    def r(res: String, why: String) = DkimResult(d, s, res, why)
    val algo = t.getOrElse("a", "").toLowerCase
    if t.get("v").contains("1") == false then return r("permerror", "v=1 がありません")
    if d.isEmpty || s.isEmpty || !t.contains("b") || !t.contains("bh") || !t.contains("h") then return r("permerror", "必須のタグがありません")
    if !t("h").toLowerCase.split(':').contains("from") then return r("permerror", "From が署名されていません")
    // rsa-sha1 は RFC 8301 で、検証する側が有効とみなしてはいけない
    if !Set("rsa-sha256", "ed25519-sha256").contains(algo) then return r("permerror", s"未対応のアルゴリズム $algo")
    val idom = t.get("i").map(Address.domainOf)
    if idom.exists(x => !(x == d || x.endsWith("." + d))) then return r("permerror", "i= が d= のドメインではありません")
    if t.get("x").flatMap(_.toLongOption).exists(x => java.time.Instant.ofEpochSecond(x).isBefore(now)) then return r("fail", "署名の有効期限切れ")
    val (hc, bc) = t.getOrElse("c", "simple/simple").toLowerCase.split('/') match
      case Array(a, b) => (a, b)
      case Array(a)    => (a, "simple")
      case _           => ("simple", "simple")
    val hashAlg = if algo.endsWith("sha1") then "SHA-1" else "SHA-256"
    // 本文のハッシュ
    val body = canonBody(java.util.Arrays.copyOfRange(msg, bodyStart, msg.length), bc)
    val limit = t.get("l").flatMap(_.toLongOption)
    if limit.exists(_ > body.length) then return r("permerror", "l= が本文より長い")
    val limited = limit.fold(body)(l => body.take(l.toInt))
    val bh = MessageDigest.getInstance(hashAlg).digest(limited)
    if !MessageDigest.isEqual(bh, Base64.getDecoder.decode(t("bh"))) then return r("fail", "本文のハッシュが一致しません")
    // ヘッダーの正規化（同じ名前は下から順に使う）
    val used = scala.collection.mutable.Map.empty[String, Int]
    val signed = new StringBuilder
    t("h").split(':').map(_.trim).filter(_.nonEmpty).foreach { n =>
      val ln = n.toLowerCase
      val all = headers.filter(_.name.equalsIgnoreCase(ln)).reverse
      val k = used.getOrElse(ln, 0)
      used(ln) = k + 1
      all.lift(k).foreach(h => signed.append(canonHeader(h, hc)))
    }
    val sigNoB = RawHeader(sig.name, Dkim.stripB(sig.raw))
    signed.append(canonHeader(sigNoB, hc).stripSuffix("\r\n"))
    val data = signed.toString.getBytes(StandardCharsets.ISO_8859_1)
    // 公開鍵
    dns.txt(s"$s._domainkey.$d") match
      case Left(e) => r("temperror", s"公開鍵を取得できません: $e")
      case Right(Nil) => r("permerror", "公開鍵がありません")
      case Right(txts) =>
        val kt = tags(txts.mkString)
        val p = kt.getOrElse("p", "")
        if p.isEmpty then return r("permerror", "鍵が失効しています")
        val ktype = kt.getOrElse("k", "rsa").toLowerCase
        if (ktype == "ed25519") != algo.startsWith("ed25519") then return r("permerror", "鍵の種類が違います")
        val ok = scala.util.Try {
          val sigBytes = Base64.getDecoder.decode(t("b"))
          if ktype == "ed25519" then
            val v = Signature.getInstance("Ed25519")
            v.initVerify(Dkim.ed25519Public(Base64.getDecoder.decode(p)))
            v.update(MessageDigest.getInstance("SHA-256").digest(data))
            v.verify(sigBytes)
          else
            val key = Dkim.rsaPublic(Base64.getDecoder.decode(p))
            if key.getModulus.bitLength < 1024 then false
            else
              val v = Signature.getInstance(if hashAlg == "SHA-1" then "SHA1withRSA" else "SHA256withRSA")
              v.initVerify(key)
              v.update(data)
              v.verify(sigBytes)
        }
        ok match
          case scala.util.Success(true) =>
            // l= が本文の全体より短い（署名の後ろに内容が足されている）なら pass にしない
            if limit.exists(_ < body.length) then r("policy", "l= で本文の一部だけが署名されています（署名の後ろに追加された内容があります）")
            else r("pass", "")
          case scala.util.Success(false) => r("fail", "署名が一致しません")
          case scala.util.Failure(e)     => r("permerror", s"鍵を読めません: ${e.getMessage}")

  def canonHeader(h: RawHeader, mode: String): String =
    if mode == "relaxed" then
      val v = h.raw.substring(h.raw.indexOf(':') + 1).replaceAll("\r?\n", "").replaceAll("[ \t]+", " ").trim
      s"${h.name.toLowerCase.trim}:$v\r\n"
    else if h.raw.endsWith("\r\n") then h.raw
    else h.raw.stripSuffix("\n") + "\r\n"

  def canonBody(body: Array[Byte], mode: String): Array[Byte] =
    val text = new String(body, StandardCharsets.ISO_8859_1).replaceAll("(?<!\r)\n", "\r\n")
    if mode == "relaxed" then
      val lines = text.split("\r\n", -1).map(_.replaceAll("[ \t]+", " ").replaceAll(" +$", ""))
      val trimmed = lines.reverse.dropWhile(_.isEmpty).reverse
      if trimmed.isEmpty then Array.emptyByteArray else trimmed.mkString("", "\r\n", "\r\n").getBytes(StandardCharsets.ISO_8859_1)
    else
      // 末尾の空行を除く（正規表現の "(\r\n)+$" は、改行を並べた本文で計算が二乗になるので、後ろから数える）
      var e = text.length
      while e >= 2 && text.charAt(e - 2) == '\r' && text.charAt(e - 1) == '\n' do e -= 2
      (text.substring(0, e) + "\r\n").getBytes(StandardCharsets.ISO_8859_1)

object Dkim:
  /** 署名ヘッダーの b= の値を空にする（タグの位置で判定する。ほかのタグの値は変えない）。 */
  def stripB(raw: String): String =
    val i = raw.indexOf(':') + 1
    raw.substring(0, i) + raw.substring(i).split(";", -1).map { p =>
      val eq = p.indexOf('=')
      if eq > 0 && p.substring(0, eq).trim.equalsIgnoreCase("b") then p.substring(0, eq + 1) else p
    }.mkString(";")

  /** 署名するヘッダー（あるものだけ。From は二重に入れて後からの追加を防ぐ）。 */
  val SignHeaders = List("from", "to", "cc", "subject", "date", "message-id", "reply-to", "in-reply-to", "references", "mime-version", "content-type",
    "content-transfer-encoding", "list-unsubscribe")

  def rsaPublic(der: Array[Byte]): java.security.interfaces.RSAPublicKey =
    val kf = KeyFactory.getInstance("RSA")
    scala.util.Try(kf.generatePublic(new X509EncodedKeySpec(der))).getOrElse {
      // PKCS#1 の RSAPublicKey（一部の古い DNS レコード）を SPKI に包む
      val prefix = Array[Byte](0x30, 0x0d, 0x06, 0x09, 0x2a, 0x86.toByte, 0x48, 0x86.toByte, 0xf7.toByte, 0x0d, 0x01, 0x01, 0x01, 0x05, 0x00)
      val bitStr = derTag(0x03, Array[Byte](0) ++ der)
      kf.generatePublic(new X509EncodedKeySpec(derTag(0x30, prefix ++ bitStr)))
    }.asInstanceOf[java.security.interfaces.RSAPublicKey]

  def ed25519Public(raw: Array[Byte]): PublicKey =
    val prefix = Array[Byte](0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x03, 0x21, 0x00)
    KeyFactory.getInstance("Ed25519").generatePublic(new X509EncodedKeySpec(prefix ++ raw))

  private def derTag(tag: Int, content: Array[Byte]): Array[Byte] =
    val len = content.length
    val lenBytes =
      if len < 128 then Array(len.toByte)
      else
        val b = BigInt(len).toByteArray.dropWhile(_ == 0)
        Array((0x80 | b.length).toByte) ++ b
    Array(tag.toByte) ++ lenBytes ++ content

  /** RSA 2048 の鍵を作る（PKCS#8 の秘密鍵と、DNS に載せる公開鍵の base64）。 */
  def generate(): (Array[Byte], String) =
    val g = java.security.KeyPairGenerator.getInstance("RSA")
    g.initialize(2048)
    val kp = g.generateKeyPair()
    (kp.getPrivate.getEncoded, Base64.getEncoder.encodeToString(kp.getPublic.getEncoded))

  def privateKey(pkcs8: Array[Byte]): PrivateKey = KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(pkcs8))

  /** DNS の TXT レコードの値。 */
  def dnsRecord(publicB64: String): String = s"v=DKIM1; k=rsa; p=$publicB64"

  /** DKIM-Signature ヘッダー（"DKIM-Signature: …\r\n"）を作る。メッセージの先頭に付ける。 */
  def sign(msg: Array[Byte], domain: String, selector: String, key: PrivateKey, now: java.time.Instant): String =
    val d = new Dkim(NoDns)
    val (headers, bodyStart) = RawHeaders.split(msg)
    val body = d.canonBody(java.util.Arrays.copyOfRange(msg, bodyStart, msg.length), "relaxed")
    val bh = Base64.getEncoder.encodeToString(MessageDigest.getInstance("SHA-256").digest(body))
    val present = SignHeaders.filter(n => headers.exists(_.name.equalsIgnoreCase(n)))
    val names = present ++ List("from") // From の二重指定（オーバーサイン）
    val used = scala.collection.mutable.Map.empty[String, Int]
    val signed = new StringBuilder
    names.foreach { n =>
      val all = headers.filter(_.name.equalsIgnoreCase(n)).reverse
      val k = used.getOrElse(n, 0)
      used(n) = k + 1
      all.lift(k).foreach(h => signed.append(d.canonHeader(h, "relaxed")))
    }
    val head = s"v=1; a=rsa-sha256; c=relaxed/relaxed; d=$domain; s=$selector; t=${now.getEpochSecond};\r\n\th=${names.mkString(":")};\r\n\tbh=$bh;\r\n\tb="
    val sigHeader = RawHeader("DKIM-Signature", s"DKIM-Signature: $head")
    signed.append(d.canonHeader(sigHeader, "relaxed").stripSuffix("\r\n"))
    val s = Signature.getInstance("SHA256withRSA")
    s.initSign(key)
    s.update(signed.toString.getBytes(StandardCharsets.ISO_8859_1))
    val b = Base64.getEncoder.encodeToString(s.sign()).grouped(72).mkString("\r\n\t ")
    s"DKIM-Signature: $head$b\r\n"

  private object NoDns extends Dns:
    def txt(name: String)       = Right(Nil)
    def addresses(name: String) = Right(Nil)
    def mx(name: String)        = Right(Nil)
    def ptr(ip: InetAddress)    = Right(Nil)

final case class DmarcResult(result: String, policy: String, domain: String):
  def pass: Boolean = result == "pass"

/** DMARC（RFC 7489）。 */
final class Dmarc(dns: Dns):

  def record(domain: String): Either[String, Option[Map[String, String]]] =
    dns.txt(s"_dmarc.$domain").map(_.find(_.toLowerCase.replace(" ", "").startsWith("v=dmarc1")).map(r => new Dkim(dns).tags(r)))

  def evaluate(fromDomain: String, spf: SpfResult, mailFromDomain: String, dkim: List[DkimResult]): DmarcResult =
    val org = OrgDomain.of(fromDomain)
    val rec = record(fromDomain).flatMap {
      case Some(r) => Right(Some(r -> false))
      case None if org != fromDomain => record(org).map(_.map(_ -> true))
      case None => Right(None)
    }
    rec match
      case Left(_) => DmarcResult("temperror", "none", fromDomain)
      case Right(None) => DmarcResult("none", "none", fromDomain)
      case Right(Some((tags, fromOrg))) =>
        val policy = (if fromOrg then tags.get("sp").orElse(tags.get("p")) else tags.get("p")).map(_.toLowerCase).filter(Set("none", "quarantine", "reject")).getOrElse("none")
        def aligned(d: String, strict: Boolean) = if strict then d.equalsIgnoreCase(fromDomain) else OrgDomain.of(d) == org
        val spfOk  = spf == SpfResult.Pass && aligned(mailFromDomain, tags.get("aspf").contains("s"))
        val dkimOk = dkim.exists(r => r.pass && aligned(r.domain, tags.get("adkim").contains("s")))
        DmarcResult(if spfOk || dkimOk then "pass" else "fail", policy, fromDomain)

/** 組織ドメイン（公開接尾辞の1つ下。Public Suffix List で判定する。tokyo.jp・co.il なども正しく扱う）。 */
object OrgDomain:
  def of(domain: String): String =
    val d = domain.toLowerCase.stripSuffix(".")
    scala.util.Try {
      val n = com.google.common.net.InternetDomainName.from(d)
      if n.isUnderPublicSuffix then n.topPrivateDomain().toString
      else if n.isPublicSuffix then d
      else d.split('.').takeRight(2).mkString(".")   // 一覧にない TLD（.example・.test など）
    }.getOrElse(d)
