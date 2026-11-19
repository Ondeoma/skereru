package skereru.core

import org.apache.pdfbox.Loader
import org.apache.pdfbox.text.PDFTextStripper

import java.io.{ByteArrayInputStream, InputStream}
import java.util.zip.ZipInputStream
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants}
import scala.util.Try

/**
 * 添付ファイルの本文の取り出し（全文検索の索引用）。
 * テキスト・HTML・PDF・OOXML（docx・xlsx・pptx）・ODF（odt・ods・odp）を読む。読めない形式・壊れたファイルは飛ばし、例外は出さない。
 * 大きすぎるファイル・ZIP 爆弾・時間のかかるファイルで止まらないよう、大きさ・展開量・項目数・時間に上限がある。
 */
object AttachmentText {

  /** 1つの添付ファイルの大きさの上限（転送符号化を戻したバイト数）。超えたものは読まない。 */
  val MaxFileBytes: Int = 20 * 1024 * 1024

  /** ZIP（OOXML・ODF）で展開して読むバイト数の合計の上限（読み飛ばす項目も数える）。 */
  val MaxZipBytes: Long = 20L * 1024 * 1024

  /** ZIP で調べる項目の数の上限。 */
  val MaxZipEntries = 200

  /** PDF で読むページ数の上限。 */
  val MaxPdfPages = 50

  /** 1通（または1ファイル）にかける時間の目安。 */
  private val TimeBudgetNanos = 3_000_000_000L

  private enum Kind {
    case Text, Html, Pdf, Docx, Xlsx, Pptx, Odf
  }

  /** 検索のための添付ファイルの本文（すべての添付をつないだもの。合計 maxChars 文字まで）。読めない形式は飛ばす。例外は出さない。 */
  def extract(raw: Array[Byte], root: MimePart, maxChars: Int = 200_000): String = {
    val deadline = System.nanoTime() + TimeBudgetNanos
    val out = new Out(maxChars)
    Try {
      root.leaves().iterator.map(_._2).filter(isCandidate).foreach { p =>
        if !out.full && System.nanoTime() < deadline then {
          val name = p.filename.getOrElse("")
          kindOf(p.mimeType, name).foreach { kind =>
            // base64 は元の約 4/3 倍（改行を含めて余裕をみる）。明らかに大きいものは戻す前に飛ばす
            if p.size.toLong <= MaxFileBytes.toLong * 3 / 2 then {
              val bytes = Mime.content(raw, p)
              if bytes.length <= MaxFileBytes then {
                out.break()
                Try(read(kind, bytes, p.contentType.param("charset"), out, deadline))
              }
            }
          }
        }
      }
    }
    out.result
  }

  /** 1つのファイルの本文（mimeType とファイル名の拡張子で形式を決める）。 */
  def fromFile(bytes: Array[Byte], mimeType: String, filename: String, maxChars: Int): Option[String] =
    if bytes.length > MaxFileBytes || maxChars <= 0 then None
    else
      kindOf(mimeType, filename).flatMap { kind =>
        val out = new Out(maxChars)
        Try(read(kind, bytes, None, out, System.nanoTime() + TimeBudgetNanos))
        Some(out.result).filter(_.nonEmpty)
      }

  /** 添付として読む部品か（本文の text/plain・text/html は別に索引するので除く。中のメッセージ（message/rfc822）も除く）。 */
  private def isCandidate(p: MimePart): Boolean =
    !p.isMultipart && !p.isMessage && (p.isAttachment || p.filename.isDefined)

  private def extOf(filename: String): String = {
    val n = filename.trim.toLowerCase
    val i = n.lastIndexOf('.')
    if i >= 0 then n.substring(i + 1) else ""
  }

  /** 形式を決める（拡張子を優先する。application/octet-stream で送られてくる Office 文書が多いため）。 */
  private def kindOf(mimeType: String, filename: String): Option[Kind] = {
    val mt = mimeType.trim.toLowerCase
    extOf(filename) match {
      case "pdf" => Some(Kind.Pdf)
      case "docx" | "docm" | "dotx" | "dotm" => Some(Kind.Docx)
      case "xlsx" | "xlsm" | "xltx" | "xltm" => Some(Kind.Xlsx)
      case "pptx" | "pptm" | "ppsx" | "potx" => Some(Kind.Pptx)
      case "odt" | "ods" | "odp" | "ott" | "ots" | "otp" => Some(Kind.Odf)
      case "html" | "htm" | "xhtml" => Some(Kind.Html)
      case "txt" | "text" | "csv" | "tsv" | "md" | "markdown" | "log" | "json" | "xml" | "yaml" | "yml" | "ics" | "vcf" | "ini" | "conf" | "sql" =>
        Some(Kind.Text)
      case _ =>
        if mt == "application/pdf" then Some(Kind.Pdf)
        else if mt.contains("wordprocessingml") then Some(Kind.Docx)
        else if mt.contains("spreadsheetml") then Some(Kind.Xlsx)
        else if mt.contains("presentationml") then Some(Kind.Pptx)
        else if mt.startsWith("application/vnd.oasis.opendocument.") then Some(Kind.Odf)
        else if mt == "text/html" || mt == "application/xhtml+xml" then Some(Kind.Html)
        else if mt.startsWith("text/") && mt != "text/rtf" && mt != "text/enriched" then Some(Kind.Text)
        else None
    }
  }

  private def read(kind: Kind, bytes: Array[Byte], charset: Option[String], out: Out, deadline: Long): Unit =
    kind match {
      case Kind.Text => out.append(decode(bytes, charset, out.max))
      case Kind.Html => out.append(Html.toText(decode(bytes, charset, Int.MaxValue)))
      case Kind.Pdf => pdf(bytes, out)
      case Kind.Docx => zip(bytes, out, deadline, allText = false)(DocxEntry.matches)
      case Kind.Xlsx => zip(bytes, out, deadline, allText = false)(_ == "xl/sharedStrings.xml")
      case Kind.Pptx => zip(bytes, out, deadline, allText = false)(PptxEntry.matches)
      case Kind.Odf => zip(bytes, out, deadline, allText = true)(_ == "content.xml")
    }

  /**
   * 文字コードの指定があればそれで、なければ推測して（UTF-8、だめなら ISO-2022-JP・Shift_JIS）読む。
   * 必要な文字数を超える分（1文字 4 バイトとして）は読まない。
   */
  private def decode(bytes: Array[Byte], charset: Option[String], maxChars: Int): String = {
    val limit = math.min(bytes.length.toLong, maxChars.toLong * 4).toInt
    val b = if limit < bytes.length then java.util.Arrays.copyOf(bytes, limit) else bytes
    new String(b, charset.map(Mime.charsetFor).getOrElse(Mime.guessCharset(b)))
  }

  // ---------------------------------------------------------------- PDF

  /** PDF の本文（先頭の MaxPdfPages ページまで）。暗号化されていて読めないもの・壊れたものは例外になる（呼び出し側で捨てる）。 */
  private def pdf(bytes: Array[Byte], out: Out): Unit = {
    val doc = Loader.loadPDF(bytes)
    try {
      val st = new PDFTextStripper
      st.setStartPage(1)
      st.setEndPage(MaxPdfPages)
      out.append(st.getText(doc))
    } finally doc.close()
  }

  // ---------------------------------------------------------------- ZIP（OOXML・ODF）

  private val DocxEntry = """word/(document|footnotes|endnotes|header\d*|footer\d*)\.xml""".r
  private val PptxEntry = """ppt/(slides/slide|notesSlides/notesSlide)\d+\.xml""".r

  /** ZIP の展開量・時間の残り。 */
  private final class Budget(var bytes: Long, val deadline: Long) {
    def ok: Boolean = bytes > 0 && System.nanoTime() < deadline
  }

  /** 展開したバイト数を数え、上限か時間切れで終わり（-1）を返す入力。close しても元の ZIP は閉じない。 */
  private final class Counted(in: InputStream, budget: Budget) extends InputStream {
    override def read(): Int =
      if !budget.ok then -1
      else {
        val c = in.read()
        if c >= 0 then budget.bytes -= 1
        c
      }
    override def read(b: Array[Byte], off: Int, len: Int): Int =
      if len == 0 then 0
      else if !budget.ok then -1
      else {
        val n = in.read(b, off, math.min(len.toLong, budget.bytes).toInt)
        if n > 0 then budget.bytes -= n
        n
      }
    override def close(): Unit = ()
  }

  /**
   * ZIP の項目を順に読み、wanted な XML の文字を取り出す。読み飛ばす項目も展開量に数える（ZIP 爆弾対策）。
   * 途中で壊れていても、それまでに読めた文字は残す。
   */
  private def zip(bytes: Array[Byte], out: Out, deadline: Long, allText: Boolean)(wanted: String => Boolean): Unit = {
    val budget = new Budget(MaxZipBytes, deadline)
    val zin = new ZipInputStream(new ByteArrayInputStream(bytes))
    try {
      var entries = 0
      var e = zin.getNextEntry
      while e != null && entries < MaxZipEntries && budget.ok && !out.full do {
        entries += 1
        val in = new Counted(zin, budget)
        if !e.isDirectory && wanted(e.getName) then {
          out.break()
          Try(xmlText(in, out, allText))
        }
        drain(in)
        e = if budget.ok && !out.full then zin.getNextEntry else null
      }
    } finally zin.close()
  }

  /** 項目の残りを読み捨てる（getNextEntry に任せると、上限なしに展開されてしまう）。 */
  private def drain(in: InputStream): Unit = {
    val buf = new Array[Byte](8192)
    while in.read(buf) >= 0 do ()
  }

  /** 区切りとして空白を入れる要素（段落・セル・タブ・改行など）。 */
  private val BreakElements = Set("p", "h", "si", "tab", "br", "cr", "tc", "line-break", "table-cell", "s", "list-item")

  /**
   * XML の文字を取り出す。allText なら全ての文字（ODF）、でなければ t 要素（w:t・a:t・共有文字列の t）の中だけ（OOXML）。
   * DTD・外部実体は読まない（XXE・実体の展開による爆弾の対策）。
   */
  private def xmlText(in: InputStream, out: Out, allText: Boolean): Unit = {
    val f = XMLInputFactory.newFactory()
    f.setProperty(XMLInputFactory.SUPPORT_DTD, java.lang.Boolean.FALSE)
    f.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, java.lang.Boolean.FALSE)
    val r = f.createXMLStreamReader(in)
    try {
      var inT = 0
      var skip = 0 // ふりがな（xlsx の rPh）の中
      while r.hasNext && !out.full do {
        val ev = r.next()
        if ev == XMLStreamConstants.START_ELEMENT then {
          val n = r.getLocalName
          if n == "rPh" then skip += 1
          else if n == "t" then inT += 1
        } else if ev == XMLStreamConstants.END_ELEMENT then {
          val n = r.getLocalName
          if n == "rPh" then skip = math.max(0, skip - 1)
          else if n == "t" then inT = math.max(0, inT - 1)
          if BreakElements.contains(n) then out.break()
        } else if ev == XMLStreamConstants.CHARACTERS || ev == XMLStreamConstants.CDATA ||
          ev == XMLStreamConstants.SPACE
        then
          if skip == 0 && (allText || inT > 0) then out.append(r.getText)
      }
    } finally r.close()
  }

  // ---------------------------------------------------------------- 出力

  /** 空白をまとめながら max 文字までためる。 */
  private final class Out(val max: Int) {
    private val sb = new StringBuilder
    private var space = true // 先頭の空白は捨てる
    def full: Boolean = sb.length >= max
    def append(s: CharSequence): Unit = {
      var i = 0
      while i < s.length && sb.length < max do {
        val c = s.charAt(i)
        if c < ' ' || Character.isWhitespace(c) || Character.isSpaceChar(c) then {
          if !space then {
            sb.append(' ')
            space = true
          }
        } else {
          sb.append(c)
          space = false
        }
        i += 1
      }
    }
    def break(): Unit = append(" ")
    def result: String = {
      val s = sb.toString.trim
      // 上限で切れたサロゲートペアの片割れを捨てる
      if s.nonEmpty && Character.isHighSurrogate(s.last) then s.dropRight(1) else s
    }
  }
}
