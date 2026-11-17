package skereru.core

import java.net.InetAddress
import java.nio.charset.StandardCharsets.{ISO_8859_1, UTF_8}
import java.time.Instant

class ArcSpec extends munit.FunSuite:

  private def bytes(s: String) = s.replace("\n", "\r\n").getBytes(UTF_8)

  final class FakeDns(txts: Map[String, List[String]]) extends Dns:
    def txt(name: String) = Right(txts.getOrElse(name.toLowerCase, Nil))
    def addresses(name: String) = Right(Nil)
    def mx(name: String) = Right(Nil)
    def ptr(ip: InetAddress) = Right(Nil)

  private val (priv1, pub1) = Dkim.generate()
  private val (priv2, pub2) = Dkim.generate()
  private val dns = FakeDns(Map(
    "s1._domainkey.example.com" -> List(Dkim.dnsRecord(pub1)),
    "s2._domainkey.relay.example" -> List(Dkim.dnsRecord(pub2))
  ))
  private val arc = new Arc(dns)

  private val msg = bytes("From: Taro <taro@example.org>\nTo: list@example.com\nSubject: hello\n  folded\nDate: Mon, 16 Nov 2026 10:00:00 +0900\nMessage-ID: <x@example.org>\n\nbody  text \n\n\n")

  private val none = ArcResult("none", 0, "")

  /** 1回目の転送（i=1）で封印したメッセージ。 */
  private def hop1(): Array[Byte] =
    val h = Arc.seal(msg, "example.com", "s1", Dkim.privateKey(priv1), "mx.example.com",
      "spf=pass smtp.mailfrom=example.org; dkim=none; dmarc=pass header.from=example.org", none, Instant.now())
    h.getBytes(ISO_8859_1) ++ msg

  test("ARC: 1回目の封印を検証すると pass（i=1）") {
    val m1 = hop1()
    val text = new String(m1, ISO_8859_1)
    assert(text.startsWith("ARC-Seal: i=1; "), text)
    assert(text.contains("cv=none"), text)
    assert(text.contains("ARC-Authentication-Results: i=1; mx.example.com;"), text)
    assertEquals(arc.verify(m1, Instant.now()), ArcResult("pass", 1, ""))
  }

  test("ARC: 2回目の転送で封印しても pass（i=2・cv=pass）") {
    val m1 = hop1()
    val chain = arc.verify(m1, Instant.now())
    val h2 = Arc.seal(m1, "relay.example", "s2", Dkim.privateKey(priv2), "mx.relay.example", "arc=pass", chain, Instant.now())
    assert(h2.startsWith("ARC-Seal: i=2; "), h2)
    assert(h2.contains("cv=pass"), h2)
    val m2 = h2.getBytes(ISO_8859_1) ++ m1
    assertEquals(arc.verify(m2, Instant.now()), ArcResult("pass", 2, ""))
  }

  test("ARC: 封印の後に本文を変えると fail") {
    val tampered = new String(hop1(), ISO_8859_1).replace("body  text", "body text!").getBytes(ISO_8859_1)
    val r = arc.verify(tampered, Instant.now())
    assertEquals(r.result, "fail", r.toString)
    assertEquals(r.instance, 1)
  }

  test("ARC: ARC のヘッダーがなければ none") {
    assertEquals(arc.verify(msg, Instant.now()), ArcResult("none", 0, ""))
  }

  test("ARC: ARC-Seal の cv を書き換えると fail") {
    val m1 = hop1()
    val chain = arc.verify(m1, Instant.now())
    val m2 = Arc.seal(m1, "relay.example", "s2", Dkim.privateKey(priv2), "mx.relay.example", "arc=pass", chain, Instant.now()).getBytes(ISO_8859_1) ++ m1
    // i=1 の cv を pass に（構造の確認で fail）
    val t1 = new String(m1, ISO_8859_1).replace("cv=none", "cv=pass").getBytes(ISO_8859_1)
    assertEquals(arc.verify(t1, Instant.now()).result, "fail")
    // i=2 の cv を none に（構造の確認で fail）
    val t2 = new String(m2, ISO_8859_1).replace("cv=pass", "cv=none").getBytes(ISO_8859_1)
    assertEquals(arc.verify(t2, Instant.now()).result, "fail")
  }

  test("ARC: 壊れた鎖・上限を超えるセット") {
    // 同じ番号のセットが2つあれば fail
    val m1 = hop1()
    val dup = Arc.seal(msg, "example.com", "s1", Dkim.privateKey(priv1), "mx.example.com", "spf=pass", none, Instant.now()).getBytes(ISO_8859_1) ++ m1
    assertEquals(arc.verify(dup, Instant.now()).result, "fail")
    // i=50 のセットがあれば、もう封印しない
    val many = bytes("ARC-Seal: i=50; cv=pass\nARC-Message-Signature: i=50\nARC-Authentication-Results: i=50; x; none\n") ++ msg
    assertEquals(Arc.seal(many, "example.com", "s1", Dkim.privateKey(priv1), "mx.example.com", "", none, Instant.now()), "")
    // 例外を投げずに fail を返す
    assertEquals(arc.verify(many, Instant.now()).result, "fail")
  }
