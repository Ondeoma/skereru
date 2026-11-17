package skereru.core

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.time.Instant
import java.util.zip.{GZIPInputStream, ZipInputStream}
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.{Element, Node}
import org.xml.sax.{ErrorHandler, SAXParseException}
import scala.util.control.NonFatal

/** DMARC 集計レポートの1行（record）。dkim・spf は policy_evaluated の結果（pass か fail）。 */
final case class DmarcRow(
    sourceIp: String,
    count: Long,
    disposition: String,
    dkim: String,
    spf: String,
    headerFrom: String,
    envelopeFrom: Option[String],
    dkimDomains: List[String],
    spfDomains: List[String]
)

/** DMARC 集計レポート（RFC 7489 付録 C、rua で届くもの）。 */
final case class DmarcAggregate(
    orgName: String,
    email: Option[String],
    reportId: String,
    domain: String,
    policy: String,
    begin: Instant,
    end: Instant,
    rows: List[DmarcRow]
):
  def total: Long = rows.map(_.count).sum
  /** DMARC に合格した通数（DKIM か SPF のどちらかが pass）。 */
  def passing: Long = rows.filter(r => r.dkim == "pass" || r.spf == "pass").map(_.count).sum

/** DMARC 集計レポートの読み取り。中身は外から届く信用できないメールなので、
  * 展開後の大きさ・zip の項目数・行数に上限を設け、例外は投げずに Left にする。 */
object DmarcReport:

  /** 展開後の大きさの上限（zip 爆弾・gzip 爆弾よけ）。 */
  val MaxBytes: Int = 20 * 1024 * 1024
  /** zip の中で調べる項目数の上限。 */
  val MaxZipEntries = 10
  /** 行（record）の数の上限。 */
  val MaxRows = 100_000
  /** 各文字列の長さの上限。 */
  val MaxString = 255

  private val XmlTypes = Set("text/xml", "application/xml")
  private val GzipTypes = Set("application/gzip", "application/x-gzip", "application/gzip-compressed", "application/x-gunzip")
  private val ZipTypes = Set("application/zip", "application/x-zip", "application/x-zip-compressed")

  /** 報告のメール（raw）から、添付（.xml・.xml.gz・.zip、または本文そのものの XML）を探して読む。読めなければ Left(理由)。 */
  def fromMessage(raw: Array[Byte]): Either[String, DmarcAggregate] =
    try
      val root = Mime.parse(raw)
      val candidates = root.leaves().map(_._2).filter(isCandidate)
      if candidates.isEmpty then Left("DMARC レポートの添付が見つかりません")
      else
        // 候補を順に試し、最初に読めたものを返す（すべて駄目なら最初の理由）
        val results = candidates.iterator.map(p => fromBytes(Mime.content(raw, p)))
        var first: Option[String] = None
        var found: Option[DmarcAggregate] = None
        while found.isEmpty && results.hasNext do
          results.next() match
            case Right(a) => found = Some(a)
            case Left(e)  => if first.isEmpty then first = Some(e)
        found.toRight(first.getOrElse("DMARC レポートを読めません"))
    catch case NonFatal(e) => Left(s"メールを読めません: ${e.getMessage}")

  private def isCandidate(p: MimePart): Boolean =
    val name = p.filename.map(_.toLowerCase).getOrElse("")
    name.endsWith(".xml") || name.endsWith(".gz") || name.endsWith(".zip") ||
      XmlTypes(p.mimeType) || GzipTypes(p.mimeType) || ZipTypes(p.mimeType) ||
      // application/octet-stream で送ってくる報告者もいる（中身は先頭のバイトで見分ける）
      (p.mimeType == "application/octet-stream" && name.nonEmpty)

  /** 添付の中身（gzip・zip・XML のどれか。先頭のバイトで見分ける）を読む。 */
  def fromBytes(data: Array[Byte]): Either[String, DmarcAggregate] =
    try
      if data.length >= 2 && (data(0) & 0xff) == 0x1f && (data(1) & 0xff) == 0x8b then
        readLimited(new GZIPInputStream(new ByteArrayInputStream(data))).flatMap(parseXml)
      else if data.length >= 4 && data(0) == 0x50 && data(1) == 0x4b && data(2) == 0x03 && data(3) == 0x04 then
        fromZip(data).flatMap(parseXml)
      else if data.length > MaxBytes then Left("XML が大きすぎます")
      else parseXml(data)
    catch case NonFatal(e) => Left(s"展開できません: ${e.getMessage}")

  /** zip の中の最初の *.xml を取り出す（調べる項目は先頭から MaxZipEntries 個まで）。 */
  private def fromZip(data: Array[Byte]): Either[String, Array[Byte]] =
    val zin = new ZipInputStream(new ByteArrayInputStream(data))
    try
      var seen = 0
      var result: Option[Either[String, Array[Byte]]] = None
      while result.isEmpty && seen < MaxZipEntries do
        val entry = zin.getNextEntry
        if entry == null then result = Some(Left("zip に XML がありません"))
        else
          seen += 1
          if !entry.isDirectory && entry.getName.toLowerCase.endsWith(".xml") then result = Some(readLimited(zin))
      result.getOrElse(Left("zip に XML がありません（項目が多すぎます）"))
    finally zin.close()

  /** 上限まで読む。超えたら途中でやめて Left（全部を読み込んでから大きさを見ることはしない）。 */
  private def readLimited(in: InputStream): Either[String, Array[Byte]] =
    val out = new ByteArrayOutputStream()
    val buf = new Array[Byte](64 * 1024)
    var total = 0L
    var over = false
    var n = in.read(buf)
    while n >= 0 && !over do
      total += n
      if total > MaxBytes then over = true
      else
        out.write(buf, 0, n)
        n = in.read(buf)
    if over then Left(s"展開後の大きさが上限（${MaxBytes / 1024 / 1024} MB）を超えています") else Right(out.toByteArray)

  /** XXE を防いだ DOM パーサー（DOCTYPE そのものを拒む）。 */
  private def newBuilder() =
    val f = DocumentBuilderFactory.newInstance()
    f.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    f.setFeature("http://xml.org/sax/features/external-general-entities", false)
    f.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.setXIncludeAware(false)
    f.setExpandEntityReferences(false)
    f.setNamespaceAware(true)
    val b = f.newDocumentBuilder()
    // 既定のエラー処理は標準エラーに書き出すので、黙って例外にする
    b.setErrorHandler(new ErrorHandler {
      def warning(e: SAXParseException): Unit = ()
      def error(e: SAXParseException): Unit = throw e
      def fatalError(e: SAXParseException): Unit = throw e
    })
    b

  def parseXml(xml: Array[Byte]): Either[String, DmarcAggregate] =
    try
      if xml.length > MaxBytes then Left("XML が大きすぎます")
      else
        val doc = newBuilder().parse(new ByteArrayInputStream(xml))
        val root = doc.getDocumentElement
        if root == null || nameOf(root) != "feedback" then Left("ルート要素が feedback ではありません")
        else fromFeedback(root)
    catch case NonFatal(e) => Left(s"XML を読めません: ${e.getMessage}")

  private def fromFeedback(root: Element): Either[String, DmarcAggregate] =
    val meta = child(root, "report_metadata")
    val published = child(root, "policy_published")
    val records = children(root, "record")
    val range = meta.flatMap(child(_, "date_range"))
    val begin = range.flatMap(text(_, "begin")).flatMap(epoch)
    val end = range.flatMap(text(_, "end")).flatMap(epoch)
    if meta.isEmpty then Left("report_metadata がありません")
    else if published.isEmpty then Left("policy_published がありません")
    else if begin.isEmpty || end.isEmpty then Left("date_range が読めません")
    else if records.lengthCompare(MaxRows) > 0 then Left(s"行が多すぎます（上限 $MaxRows）")
    else
      Right(DmarcAggregate(
        orgName = meta.flatMap(text(_, "org_name")).getOrElse(""),
        email = meta.flatMap(text(_, "email")),
        reportId = meta.flatMap(text(_, "report_id")).getOrElse(""),
        domain = published.flatMap(text(_, "domain")).getOrElse(""),
        policy = published.flatMap(text(_, "p")).getOrElse(""),
        begin = begin.get,
        end = end.get,
        rows = records.map(row)
      ))

  private def row(rec: Element): DmarcRow =
    val r = child(rec, "row")
    val eval = r.flatMap(child(_, "policy_evaluated"))
    val ids = child(rec, "identifiers")
    val auth = child(rec, "auth_results")
    def result(name: String) =
      if eval.flatMap(text(_, name)).exists(_.equalsIgnoreCase("pass")) then "pass" else "fail"
    DmarcRow(
      sourceIp = r.flatMap(text(_, "source_ip")).getOrElse(""),
      count = r.flatMap(text(_, "count")).flatMap(_.toLongOption).map(math.max(0L, _)).getOrElse(0L),
      disposition = eval.flatMap(text(_, "disposition")).map(_.toLowerCase).getOrElse(""),
      dkim = result("dkim"),
      spf = result("spf"),
      headerFrom = ids.flatMap(text(_, "header_from")).getOrElse(""),
      envelopeFrom = ids.flatMap(text(_, "envelope_from")),
      dkimDomains = auth.toList.flatMap(children(_, "dkim")).flatMap(text(_, "domain")),
      spfDomains = auth.toList.flatMap(children(_, "spf")).flatMap(text(_, "domain"))
    )

  /** Unix 秒を Instant に（範囲外や数字でないものは None）。 */
  private def epoch(s: String): Option[Instant] =
    s.toLongOption.filter(v => v >= 0 && v < 253_402_300_800L).map(Instant.ofEpochSecond)

  /** 名前空間つき（DMARCbis の草案）でもそうでなくても、ローカル名で比べる。 */
  private def nameOf(n: Node): String = Option(n.getLocalName).getOrElse(n.getNodeName)

  private def children(e: Element, name: String): List[Element] =
    val nodes = e.getChildNodes
    (0 until nodes.getLength).iterator.map(nodes.item).collect {
      case c: Element if nameOf(c) == name => c
    }.toList

  private def child(e: Element, name: String): Option[Element] = children(e, name).headOption

  /** 子要素の文字列（前後の空白を除き、長さを切り詰める。空なら None）。 */
  private def text(e: Element, name: String): Option[String] =
    child(e, name).map(_.getTextContent).map(s => clip(s.trim)).filter(_.nonEmpty)

  private def clip(s: String): String = if s.length > MaxString then s.take(MaxString) else s
