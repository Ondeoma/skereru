package skereru.core

import org.apache.pdfbox.pdmodel.{PDDocument, PDPage, PDPageContentStream}
import org.apache.pdfbox.pdmodel.font.{PDType1Font, Standard14Fonts}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.zip.{ZipEntry, ZipOutputStream}

class AttachmentTextSpec extends munit.FunSuite {

  private val W =
    """<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
      |<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body>""".stripMargin
  private val WEnd = "</w:body></w:document>"

  private def zip(entries: (String, Array[Byte])*): Array[Byte] = {
    val bo = new ByteArrayOutputStream
    val z = new ZipOutputStream(bo)
    entries.foreach {
      (name, data) =>
        {
          z.putNextEntry(new ZipEntry(name))
          z.write(data)
          z.closeEntry()
        }
    }
    z.close()
    bo.toByteArray
  }

  private val docx = zip(
    "[Content_Types].xml" -> "<Types/>".getBytes(UTF_8),
    "word/document.xml" ->
      (W + "<w:p><w:r><w:t>見</w:t></w:r><w:r><w:t>積書</w:t></w:r></w:p><w:p><w:r><w:t>2026年11月</w:t></w:r></w:p>" +
        WEnd).getBytes(UTF_8)
  )

  private def b64(b: Array[Byte]) = Base64.getMimeEncoder.encodeToString(b)

  private val message = (
    s"""From: a@example.com
       |To: b@example.com
       |Subject: test
       |MIME-Version: 1.0
       |Content-Type: multipart/mixed; boundary="B"
       |
       |--B
       |Content-Type: text/plain; charset=utf-8
       |Content-Transfer-Encoding: 8bit
       |
       |本文のひみつのことば
       |--B
       |Content-Type: text/plain; charset=utf-8; name="memo.txt"
       |Content-Disposition: attachment; filename="memo.txt"
       |Content-Transfer-Encoding: base64
       |
       |${b64("添付のメモ　です\n\n二行目".getBytes(UTF_8))}
       |--B
       |Content-Type: application/octet-stream; name="mitsumori.docx"
       |Content-Disposition: attachment; filename="mitsumori.docx"
       |Content-Transfer-Encoding: base64
       |
       |${b64(docx)}
       |--B
       |Content-Type: image/png; name="a.png"
       |Content-Disposition: attachment; filename="a.png"
       |Content-Transfer-Encoding: base64
       |
       |iVBORw0KGgo=
       |--B--
       |""".stripMargin.replace("\r\n", "\n").replace("\n", "\r\n")
  ).getBytes(UTF_8)

  test("メッセージの添付（txt・docx）の本文を取り出し、本文の部品は含めない") {
    val root = Mime.parse(message)
    val text = AttachmentText.extract(message, root)
    assert(text.contains("添付のメモ です 二行目"), text)
    assert(text.contains("見積書"), text)
    assert(text.contains("2026年11月"), text)
    assert(!text.contains("ひみつのことば"), text)
    assert(!text.contains("  "), text)
  }

  test("合計の文字数は maxChars まで") {
    val root = Mime.parse(message)
    assert(AttachmentText.extract(message, root, maxChars = 5).length <= 5)
  }

  test("PDF の本文") {
    val doc = new PDDocument
    val bo = new ByteArrayOutputStream
    try {
      val page = new PDPage
      doc.addPage(page)
      val cs = new PDPageContentStream(doc, page)
      cs.beginText()
      cs.setFont(new PDType1Font(Standard14Fonts.FontName.HELVETICA), 12)
      cs.newLineAtOffset(72, 700)
      cs.showText("Invoice 2026")
      cs.endText()
      cs.close()
      doc.save(bo)
    } finally doc.close()
    val text = AttachmentText.fromFile(bo.toByteArray, "application/pdf", "invoice.pdf", 10_000)
    assert(text.exists(_.contains("Invoice 2026")), text)
  }

  test("壊れた PDF は None") {
    assertEquals(AttachmentText.fromFile("%PDF-1.4\nこわれた".getBytes(UTF_8), "application/pdf", "x.pdf", 10_000), None)
    assertEquals(AttachmentText.fromFile(Array[Byte](1, 2, 3), "application/octet-stream", "x.pdf", 10_000), None)
  }

  test("読めない形式は None") {
    assertEquals(AttachmentText.fromFile(Array[Byte](1, 2, 3), "image/png", "a.png", 10_000), None)
  }

  test("ODF・xlsx・pptx") {
    val odt = zip("content.xml" ->
      """<office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"><office:body><office:text><text:p>議事録</text:p><text:p>次回</text:p></office:text></office:body></office:document-content>""".getBytes(
        UTF_8
      ))
    assertEquals(AttachmentText.fromFile(odt, "application/vnd.oasis.opendocument.text", "a.odt", 1000), Some("議事録 次回"))
    val xlsx = zip("xl/sharedStrings.xml" ->
      """<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><si><t>売上</t><rPh><t>ウリアゲ</t></rPh></si><si><t>合計</t></si></sst>""".getBytes(
        UTF_8
      ))
    assertEquals(AttachmentText.fromFile(xlsx, "application/octet-stream", "a.xlsx", 1000), Some("売上 合計"))
    val pptx = zip("ppt/slides/slide1.xml" ->
      """<p:sld xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"><a:p><a:r><a:t>企画書</a:t></a:r></a:p></p:sld>""".getBytes(
        UTF_8
      ))
    assertEquals(
      AttachmentText.fromFile(
        pptx,
        "application/vnd.openxmlformats-officedocument.presentationml.presentation",
        "a.pptx",
        1000
      ),
      Some("企画書")
    )
  }

  test("DTD・外部実体は読まない") {
    val xxe = zip("word/document.xml" ->
      ("""<?xml version="1.0"?><!DOCTYPE d [<!ENTITY x SYSTEM "file:///etc/passwd">]>""" +
        """<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:body><w:p><w:r><w:t>a&x;b</w:t></w:r></w:p></w:body></w:document>""").getBytes(
        UTF_8
      ))
    val text = AttachmentText.fromFile(xxe, "", "a.docx", 1000)
    assert(!text.exists(_.contains("root:")), text)
  }

  /** 30 MB の document.xml（よく圧縮される）を持つ docx。 */
  private def bombDocx(): Array[Byte] = {
    val bo = new ByteArrayOutputStream
    val z = new ZipOutputStream(bo)
    z.putNextEntry(new ZipEntry("word/document.xml"))
    z.write(W.getBytes(UTF_8))
    val chunk = ("<w:p><w:r><w:t>見積書</w:t></w:r></w:p>" * 1000).getBytes(UTF_8)
    var written = 0L
    while written < 30L * 1024 * 1024 do {
      z.write(chunk)
      written += chunk.length
    }
    z.write(WEnd.getBytes(UTF_8))
    z.closeEntry()
    z.close()
    bo.toByteArray
  }

  test("ZIP 爆弾のような docx でも上限で止まる") {
    val bomb = bombDocx()
    assert(bomb.length < 5 * 1024 * 1024)
    val small = AttachmentText.fromFile(bomb, "", "bomb.docx", 1000)
    assert(small.exists(t => t.length <= 1000 && t.contains("見積書")), small)
    // 文字数の上限がなくても、展開量（20 MB）か時間の上限で止まる
    val start = System.nanoTime()
    val big = AttachmentText.fromFile(bomb, "", "bomb.docx", Int.MaxValue)
    assert(big.exists(_.length < 20 * 1024 * 1024))
    assert(System.nanoTime() - start < 20_000_000_000L)
  }

  test("読み飛ばす大きな項目も展開量に数える") {
    val bo = new ByteArrayOutputStream
    val z = new ZipOutputStream(bo)
    z.putNextEntry(new ZipEntry("word/media/big.bin"))
    val zeros = new Array[Byte](1024 * 1024)
    (1 to 30).foreach(_ => z.write(zeros))
    z.closeEntry()
    z.putNextEntry(new ZipEntry("word/document.xml"))
    z.write((W + "<w:p><w:r><w:t>見積書</w:t></w:r></w:p>" + WEnd).getBytes(UTF_8))
    z.closeEntry()
    z.close()
    assertEquals(AttachmentText.fromFile(bo.toByteArray, "", "a.docx", 1000), None)
  }
}
