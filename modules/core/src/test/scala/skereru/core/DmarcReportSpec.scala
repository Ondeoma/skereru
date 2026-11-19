package skereru.core

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.util.Base64
import java.util.zip.{GZIPOutputStream, ZipEntry, ZipOutputStream}

class DmarcReportSpec extends munit.FunSuite {

  // Google が送ってくる形に近い見本（2行。1行目は合格、2行目は DKIM・SPF とも不合格）
  private val sample =
    """<?xml version="1.0" encoding="UTF-8" ?>
      |<feedback>
      |  <report_metadata>
      |    <org_name>google.com</org_name>
      |    <email>noreply-dmarc-support@google.com</email>
      |    <extra_contact_info>https://support.google.com/a/answer/2466580</extra_contact_info>
      |    <report_id>1234567890123456789</report_id>
      |    <date_range>
      |      <begin>1763251200</begin>
      |      <end>1763337599</end>
      |    </date_range>
      |  </report_metadata>
      |  <policy_published>
      |    <domain>example.com</domain>
      |    <adkim>r</adkim>
      |    <aspf>r</aspf>
      |    <p>quarantine</p>
      |    <sp>quarantine</sp>
      |    <pct>100</pct>
      |    <np>quarantine</np>
      |  </policy_published>
      |  <record>
      |    <row>
      |      <source_ip>203.0.113.10</source_ip>
      |      <count>12</count>
      |      <policy_evaluated>
      |        <disposition>none</disposition>
      |        <dkim>pass</dkim>
      |        <spf>pass</spf>
      |      </policy_evaluated>
      |    </row>
      |    <identifiers>
      |      <header_from>example.com</header_from>
      |    </identifiers>
      |    <auth_results>
      |      <dkim>
      |        <domain>example.com</domain>
      |        <result>pass</result>
      |        <selector>s1</selector>
      |      </dkim>
      |      <spf>
      |        <domain>example.com</domain>
      |        <result>pass</result>
      |      </spf>
      |    </auth_results>
      |  </record>
      |  <record>
      |    <row>
      |      <source_ip>198.51.100.7</source_ip>
      |      <count>3</count>
      |      <policy_evaluated>
      |        <disposition>quarantine</disposition>
      |        <dkim>fail</dkim>
      |        <spf>fail</spf>
      |      </policy_evaluated>
      |    </row>
      |    <identifiers>
      |      <envelope_from>spammer.example</envelope_from>
      |      <header_from>example.com</header_from>
      |    </identifiers>
      |    <auth_results>
      |      <dkim>
      |        <domain>spammer.example</domain>
      |        <result>pass</result>
      |      </dkim>
      |      <dkim>
      |        <domain>other.example</domain>
      |        <result>fail</result>
      |      </dkim>
      |      <spf>
      |        <domain>spammer.example</domain>
      |        <result>pass</result>
      |      </spf>
      |    </auth_results>
      |  </record>
      |</feedback>
      |""".stripMargin.getBytes(UTF_8)

  private def gzip(b: Array[Byte]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val gz = new GZIPOutputStream(out)
    gz.write(b)
    gz.close()
    out.toByteArray
  }

  private def zip(entries: (String, Array[Byte])*): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val z = new ZipOutputStream(out)
    for (name, b) <- entries do {
      z.putNextEntry(new ZipEntry(name))
      z.write(b)
      z.closeEntry()
    }
    z.close()
    out.toByteArray
  }

  private def checkSample(r: Either[String, DmarcAggregate])(using munit.Location): Unit = {
    val a = r.fold(e => fail(s"Left: $e"), identity)
    assertEquals(a.orgName, "google.com")
    assertEquals(a.email, Some("noreply-dmarc-support@google.com"))
    assertEquals(a.reportId, "1234567890123456789")
    assertEquals(a.domain, "example.com")
    assertEquals(a.policy, "quarantine")
    assertEquals(a.begin, Instant.ofEpochSecond(1763251200L))
    assertEquals(a.end, Instant.ofEpochSecond(1763337599L))
    assertEquals(a.rows.length, 2)
    assertEquals(a.total, 15L)
    assertEquals(a.passing, 12L)
    val ok = a.rows.head
    assertEquals(ok.sourceIp, "203.0.113.10")
    assertEquals(ok.count, 12L)
    assertEquals(ok.disposition, "none")
    assertEquals((ok.dkim, ok.spf), ("pass", "pass"))
    assertEquals(ok.headerFrom, "example.com")
    assertEquals(ok.envelopeFrom, None)
    assertEquals(ok.dkimDomains, List("example.com"))
    assertEquals(ok.spfDomains, List("example.com"))
    val bad = a.rows(1)
    assertEquals(bad.disposition, "quarantine")
    assertEquals((bad.dkim, bad.spf), ("fail", "fail"))
    assertEquals(bad.envelopeFrom, Some("spammer.example"))
    assertEquals(bad.dkimDomains, List("spammer.example", "other.example"))
    assertEquals(bad.spfDomains, List("spammer.example"))
  }

  test("XML をそのまま読む") {
    checkSample(DmarcReport.parseXml(sample))
    checkSample(DmarcReport.fromBytes(sample))
  }

  test("gzip と zip を先頭のバイトで見分けて読む") {
    checkSample(DmarcReport.fromBytes(gzip(sample)))
    checkSample(DmarcReport.fromBytes(zip(
      "readme.txt" -> "hi".getBytes(UTF_8),
      "google.com!example.com!1763251200!1763337599.xml" -> sample
    )))
    assert(DmarcReport.fromBytes(zip("readme.txt" -> "hi".getBytes(UTF_8))).isLeft)
  }

  test("メールの添付（.xml.gz を base64）から読む") {
    val b64 = Base64.getMimeEncoder.encodeToString(gzip(sample))
    val raw = List(
      "From: noreply-dmarc-support@google.com",
      "To: dmarc@example.com",
      "Subject: Report domain: example.com Submitter: google.com Report-ID: 1234567890123456789",
      "Date: Mon, 17 Nov 2025 00:00:00 +0000",
      "MIME-Version: 1.0",
      "Content-Type: multipart/mixed; boundary=\"000000000000abcdef\"",
      "",
      "--000000000000abcdef",
      "Content-Type: text/plain; charset=\"UTF-8\"; format=flowed; delsp=yes",
      "Content-Transfer-Encoding: base64",
      "",
      Base64.getEncoder.encodeToString("This is an aggregate report from google.com.".getBytes(UTF_8)),
      "--000000000000abcdef",
      "Content-Type: application/gzip; ",
      "\tname=\"google.com!example.com!1763251200!1763337599.xml.gz\"",
      "Content-Disposition: attachment; ",
      "\tfilename=\"google.com!example.com!1763251200!1763337599.xml.gz\"",
      "Content-Transfer-Encoding: base64",
      "",
      b64,
      "--000000000000abcdef--",
      ""
    ).mkString("\r\n").getBytes(UTF_8)
    checkSample(DmarcReport.fromMessage(raw))
    // 添付のないメールは Left
    val plain = "From: a@example.com\r\nSubject: hi\r\n\r\nhello\r\n".getBytes(UTF_8)
    assert(DmarcReport.fromMessage(plain).isLeft)
  }

  test("外部実体（XXE）を含む XML は拒む") {
    val xxe =
      """<?xml version="1.0"?>
        |<!DOCTYPE feedback [<!ENTITY xxe SYSTEM "file:///etc/passwd">]>
        |<feedback><report_metadata><org_name>&xxe;</org_name></report_metadata></feedback>
        |""".stripMargin.getBytes(UTF_8)
    assert(DmarcReport.parseXml(xxe).isLeft)
    assert(DmarcReport.fromBytes(gzip(xxe)).isLeft)
  }

  test("gzip 爆弾・zip 爆弾は展開しきらずに拒む") {
    val zeros = new Array[Byte](30 * 1024 * 1024)
    val bomb = gzip(zeros)
    assert(bomb.length < 1024 * 1024)
    val r = DmarcReport.fromBytes(bomb)
    assert(r.isLeft)
    assert(r.left.exists(_.contains("上限")), r)
    assert(DmarcReport.fromBytes(zip("bomb.xml" -> zeros)).left.exists(_.contains("上限")))
  }

  test("ルートが feedback でなければ拒む・壊れたものも例外を投げない") {
    assert(DmarcReport.parseXml("<html><body>hi</body></html>".getBytes(UTF_8)).isLeft)
    assert(DmarcReport.parseXml("<feedback><oops".getBytes(UTF_8)).isLeft)
    assert(DmarcReport.fromBytes(Array[Byte](0x1f, 0x8b.toByte, 1, 2, 3)).isLeft)
    assert(DmarcReport.fromBytes(Array.emptyByteArray).isLeft)
  }
}
