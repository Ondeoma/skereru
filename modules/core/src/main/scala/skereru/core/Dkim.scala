package skereru.core

import java.net.InetAddress
import java.nio.charset.StandardCharsets
import java.security.{KeyFactory, MessageDigest, PrivateKey, PublicKey, Signature}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.util.Base64

/** DKIM（RFC 6376・RFC 8463）の検証と署名。メッセージの改行は CRLF であること。 */
final class Dkim(
  dns: Dns) {

  def tags(
    s: String
  ): Map[String, String] =
    s.split(';').toList.flatMap { kv =>
      val i = kv.indexOf('=')
      Option.when(i > 0)(kv.substring(0, i).trim.toLowerCase -> kv.substring(i + 1).replaceAll("[ \t\r\n]", "").trim)
    }.toMap

  def verify(
    msg: Array[Byte],
    now: java.time.Instant
  ): List[DkimResult] = {
    val (headers, bodyStart) = RawHeaders.split(msg)
    headers.filter(_.name.equalsIgnoreCase("DKIM-Signature")).take(5).map(sig =>
      verifyOne(msg, headers, bodyStart, sig, now)
    )
  }

  private def verifyOne(
    msg: Array[Byte],
    headers: List[RawHeader],
    bodyStart: Int,
    sig: RawHeader,
    now: java.time.Instant
  ): DkimResult = {
    val value = sig.raw.substring(sig.raw.indexOf(':') + 1)
    val t = tags(value)
    val d = t.getOrElse("d", "").toLowerCase
    val s = t.getOrElse("s", "")
    def r(
      res: String,
      why: String
    ) = DkimResult(d, s, res, why)
    val algo = t.getOrElse("a", "").toLowerCase
    if t.get("v").contains("1") == false then return r("permerror", "v=1 がありません")
    if d.isEmpty || s.isEmpty || !t.contains("b") || !t.contains("bh") || !t.contains("h") then
      return r("permerror", "必須のタグがありません")
    if !t("h").toLowerCase.split(':').contains("from") then return r("permerror", "From が署名されていません")
    // rsa-sha1 は RFC 8301 で、検証する側が有効とみなしてはいけない
    if !Set("rsa-sha256", "ed25519-sha256").contains(algo) then return r("permerror", s"未対応のアルゴリズム $algo")
    val idom = t.get("i").map(Address.domainOf)
    if idom.exists(x => !(x == d || x.endsWith("." + d))) then return r("permerror", "i= が d= のドメインではありません")
    if t.get("x").flatMap(_.toLongOption).exists(x => java.time.Instant.ofEpochSecond(x).isBefore(now)) then
      return r("fail", "署名の有効期限切れ")
    val (hc, bc) = t.getOrElse("c", "simple/simple").toLowerCase.split('/') match {
      case Array(a, b) => (a, b)
      case Array(a) => (a, "simple")
      case _ => ("simple", "simple")
    }
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
    dns.txt(s"$s._domainkey.$d") match {
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
          if ktype == "ed25519" then {
            val v = Signature.getInstance("Ed25519")
            v.initVerify(Dkim.ed25519Public(Base64.getDecoder.decode(p)))
            v.update(MessageDigest.getInstance("SHA-256").digest(data))
            v.verify(sigBytes)
          } else {
            val key = Dkim.rsaPublic(Base64.getDecoder.decode(p))
            if key.getModulus.bitLength < 1024 then false
            else {
              val v = Signature.getInstance(if hashAlg == "SHA-1" then "SHA1withRSA" else "SHA256withRSA")
              v.initVerify(key)
              v.update(data)
              v.verify(sigBytes)
            }
          }
        }
        ok match {
          case scala.util.Success(true) =>
            // l= が本文の全体より短い（署名の後ろに内容が足されている）なら pass にしない
            if limit.exists(_ < body.length) then r("policy", "l= で本文の一部だけが署名されています（署名の後ろに追加された内容があります）")
            else r("pass", "")
          case scala.util.Success(false) => r("fail", "署名が一致しません")
          case scala.util.Failure(e) => r("permerror", s"鍵を読めません: ${e.getMessage}")
        }
    }
  }

  def canonHeader(
    h: RawHeader,
    mode: String
  ): String =
    if mode == "relaxed" then {
      val v = h.raw.substring(h.raw.indexOf(':') + 1).replaceAll("\r?\n", "").replaceAll("[ \t]+", " ").trim
      s"${h.name.toLowerCase.trim}:$v\r\n"
    } else if h.raw.endsWith("\r\n") then h.raw
    else h.raw.stripSuffix("\n") + "\r\n"

  def canonBody(
    body: Array[Byte],
    mode: String
  ): Array[Byte] = {
    val text = new String(body, StandardCharsets.ISO_8859_1).replaceAll("(?<!\r)\n", "\r\n")
    if mode == "relaxed" then {
      val lines = text.split("\r\n", -1).map(_.replaceAll("[ \t]+", " ").replaceAll(" +$", ""))
      val trimmed = lines.reverse.dropWhile(_.isEmpty).reverse
      if trimmed.isEmpty then Array.emptyByteArray
      else trimmed.mkString("", "\r\n", "\r\n").getBytes(StandardCharsets.ISO_8859_1)
    } else {
      // 末尾の空行を除く（正規表現の "(\r\n)+$" は、改行を並べた本文で計算が二乗になるので、後ろから数える）
      var e = text.length
      while e >= 2 && text.charAt(e - 2) == '\r' && text.charAt(e - 1) == '\n' do e -= 2
      (text.substring(0, e) + "\r\n").getBytes(StandardCharsets.ISO_8859_1)
    }
  }
}

object Dkim {

  /** 署名ヘッダーの b= の値を空にする（タグの位置で判定する。ほかのタグの値は変えない）。 */
  def stripB(
    raw: String
  ): String = {
    val i = raw.indexOf(':') + 1
    raw.substring(0, i) + raw.substring(i).split(";", -1).map { p =>
      val eq = p.indexOf('=')
      if eq > 0 && p.substring(0, eq).trim.equalsIgnoreCase("b") then p.substring(0, eq + 1) else p
    }.mkString(";")
  }

  /** 署名するヘッダー（あるものだけ。From は二重に入れて後からの追加を防ぐ）。 */
  val SignHeaders = List(
    "from",
    "to",
    "cc",
    "subject",
    "date",
    "message-id",
    "reply-to",
    "in-reply-to",
    "references",
    "mime-version",
    "content-type",
    "content-transfer-encoding",
    "list-unsubscribe"
  )

  def rsaPublic(
    der: Array[Byte]
  ): java.security.interfaces.RSAPublicKey = {
    val kf = KeyFactory.getInstance("RSA")
    scala.util.Try(kf.generatePublic(new X509EncodedKeySpec(der))).getOrElse {
      // PKCS#1 の RSAPublicKey（一部の古い DNS レコード）を SPKI に包む
      val prefix = Array[Byte](
        0x30,
        0x0d,
        0x06,
        0x09,
        0x2a,
        0x86.toByte,
        0x48,
        0x86.toByte,
        0xf7.toByte,
        0x0d,
        0x01,
        0x01,
        0x01,
        0x05,
        0x00
      )
      val bitStr = derTag(0x03, Array[Byte](0) ++ der)
      kf.generatePublic(new X509EncodedKeySpec(derTag(0x30, prefix ++ bitStr)))
    }.asInstanceOf[java.security.interfaces.RSAPublicKey]
  }

  def ed25519Public(
    raw: Array[Byte]
  ): PublicKey = {
    val prefix = Array[Byte](0x30, 0x2a, 0x30, 0x05, 0x06, 0x03, 0x2b, 0x65, 0x70, 0x03, 0x21, 0x00)
    KeyFactory.getInstance("Ed25519").generatePublic(new X509EncodedKeySpec(prefix ++ raw))
  }

  private def derTag(
    tag: Int,
    content: Array[Byte]
  ): Array[Byte] = {
    val len = content.length
    val lenBytes =
      if len < 128 then Array(len.toByte)
      else {
        val b = BigInt(len).toByteArray.dropWhile(_ == 0)
        Array((0x80 | b.length).toByte) ++ b
      }
    Array(tag.toByte) ++ lenBytes ++ content
  }

  /** RSA 2048 の鍵を作る（PKCS#8 の秘密鍵と、DNS に載せる公開鍵の base64）。 */
  def generate(): (Array[Byte], String) = {
    val g = java.security.KeyPairGenerator.getInstance("RSA")
    g.initialize(2048)
    val kp = g.generateKeyPair()
    (kp.getPrivate.getEncoded, Base64.getEncoder.encodeToString(kp.getPublic.getEncoded))
  }

  def privateKey(
    pkcs8: Array[Byte]
  ): PrivateKey = KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(pkcs8))

  /** DNS の TXT レコードの値。 */
  def dnsRecord(
    publicB64: String
  ): String = s"v=DKIM1; k=rsa; p=$publicB64"

  /** DKIM-Signature ヘッダー（"DKIM-Signature: …\r\n"）を作る。メッセージの先頭に付ける。 */
  def sign(
    msg: Array[Byte],
    domain: String,
    selector: String,
    key: PrivateKey,
    now: java.time.Instant
  ): String = {
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
    val head =
      s"v=1; a=rsa-sha256; c=relaxed/relaxed; d=$domain; s=$selector; t=${now.getEpochSecond};\r\n\th=${names.mkString(":")};\r\n\tbh=$bh;\r\n\tb="
    val sigHeader = RawHeader("DKIM-Signature", s"DKIM-Signature: $head")
    signed.append(d.canonHeader(sigHeader, "relaxed").stripSuffix("\r\n"))
    val s = Signature.getInstance("SHA256withRSA")
    s.initSign(key)
    s.update(signed.toString.getBytes(StandardCharsets.ISO_8859_1))
    val b = Base64.getEncoder.encodeToString(s.sign()).grouped(72).mkString("\r\n\t ")
    s"DKIM-Signature: $head$b\r\n"
  }

  private object NoDns extends Dns {
    def txt(
      name: String
    ) = Right(Nil)
    def addresses(
      name: String
    ) = Right(Nil)
    def mx(
      name: String
    ) = Right(Nil)
    def ptr(
      ip: InetAddress
    ) = Right(Nil)
  }
}
