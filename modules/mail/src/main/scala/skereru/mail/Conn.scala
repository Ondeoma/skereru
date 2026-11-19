package skereru.mail

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.net.{InetAddress, InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets
import javax.net.ssl.{SSLContext, SSLSocket}

/** 接続（1行ずつの読み書き・STARTTLS）。読みかけのデータはタイムアウトしても失われない。 */
final class Conn(private var socket: Socket, val remote: InetAddress, val localPort: Int) {
  private var in: InputStream = socket.getInputStream
  private var out: OutputStream = socket.getOutputStream
  private val buf = new Array[Byte](16 * 1024)
  private var pos = 0
  private var lim = 0
  private val partial = new ByteArrayOutputStream()
  @volatile private var tlsOn = false

  /** PROXY プロトコルで知らされた本当の接続元（ロードバランサーの後ろにいるとき）。 */
  var clientIp: InetAddress = remote

  /**
   * この時刻（ミリ秒）を過ぎたら、次の読み込みで切る。1回の読み込みごとのタイムアウトだけだと、少しずつ送って接続を保てるため。
   * 認証前は短く、認証できたら各セッションが延ばす。
   */
  var deadline: Long = Long.MaxValue

  def isTls: Boolean = tlsOn
  def tlsInfo: String = socket match {
    case s: SSLSocket => s"${s.getSession.getProtocol} ${s.getSession.getCipherSuite}"
    case _ => ""
  }

  def setTimeout(ms: Int): Unit = socket.setSoTimeout(ms)

  private def fill(): Boolean =
    if pos < lim then true
    else {
      if System.currentTimeMillis() > deadline then throw new java.io.EOFException("session deadline")
      val n = in.read(buf)
      if n <= 0 then false
      else {
        pos = 0; lim = n; true
      }
    }

  /** 読み込み済みでまだ使っていないバイトがあるか（STARTTLS の直後に平文が残っていれば攻撃）。 */
  def hasBuffered: Boolean = pos < lim

  /** 1行（CRLF・LF を除く）。max バイトを超えたら LineTooLong。接続が閉じたら None。 */
  def readLine(max: Int = 8192): Option[String] = readLineBytes(max).map(b => new String(b, StandardCharsets.UTF_8))

  def readLineBytes(max: Int = 8192): Option[Array[Byte]] = {
    var done = false
    var eof = false
    while !done && !eof do
      if !fill() then eof = true
      else {
        var i = pos
        while i < lim && buf(i) != '\n' do i += 1
        partial.write(buf, pos, i - pos)
        if i < lim then { pos = i + 1; done = true }
        else pos = lim
        if partial.size() > max then {
          partial.reset()
          // 行の残りを読み捨てる
          var skipping = !done
          while skipping && fill() do {
            var j = pos
            while j < lim && buf(j) != '\n' do j += 1
            if j < lim then { pos = j + 1; skipping = false }
            else pos = lim
          }
          throw LineTooLong()
        }
      }
    if eof && partial.size() == 0 then None
    else {
      val b = partial.toByteArray
      partial.reset()
      Some(if b.nonEmpty && b.last == '\r' then b.dropRight(1) else b)
    }
  }

  /** 改行を含めた1行（SMTP の DATA で、行末が CRLF かを確かめるため）。接続が閉じたら None。max を超えた分は切って返す。 */
  def readRawLine(max: Int): Option[Array[Byte]] = {
    val out = new ByteArrayOutputStream(256)
    var done = false
    while !done do
      if !fill() then
        done = true
      else {
        var i = pos
        while i < lim && buf(i) != '\n' do i += 1
        val end = if i < lim then i + 1 else lim
        if out.size() < max then out.write(buf, pos, math.min(end - pos, max - out.size()))
        pos = end
        if i < lim then done = true
      }
    if out.size() == 0 then None else Some(out.toByteArray)
  }

  /** ちょうど n バイト（IMAP のリテラル）。 */
  def readBytes(n: Int): Option[Array[Byte]] = {
    // 宣言された大きさの配列を先に確保しない（届いていないデータのためにメモリを使わせない）
    val out = new ByteArrayOutputStream(math.min(n, 64 * 1024))
    var ok = true
    while ok && out.size() < n do
      if !fill() then ok = false
      else {
        val k = math.min(n - out.size(), lim - pos)
        out.write(buf, pos, k)
        pos += k
      }
    Option.when(ok)(out.toByteArray)
  }

  def write(s: String): Unit = { out.write(s.getBytes(StandardCharsets.UTF_8)); out.flush() }
  def writeBytes(b: Array[Byte]): Unit = out.write(b)
  def writeRaw(s: String): Unit = out.write(s.getBytes(StandardCharsets.ISO_8859_1))
  def flush(): Unit = out.flush()

  /**
   * 平文の接続を TLS にする。
   * STARTTLS では読み込み済みの平文を捨てる（STARTTLS インジェクションの対策）。
   * 接続の直後から TLS のポート（993 など）では、PROXY ヘッダーと一緒に読み込んだ TLS の最初のデータを引き継ぐ（keepBuffered）。
   */
  def startTls(ctx: SSLContext, keepBuffered: Boolean = false): Unit = {
    val consumed =
      if keepBuffered && pos < lim then java.util.Arrays.copyOfRange(buf, pos, lim) else Array.emptyByteArray
    pos = 0; lim = 0; partial.reset()
    val s = (if consumed.nonEmpty then
               ctx.getSocketFactory.createSocket(socket, new java.io.ByteArrayInputStream(consumed), true)
             else
               ctx.getSocketFactory.createSocket(socket, socket.getInetAddress.getHostAddress, socket.getPort, true)
    ).asInstanceOf[SSLSocket]
    s.setUseClientMode(false)
    Tls.harden(s)
    s.startHandshake()
    socket = s
    in = s.getInputStream
    out = s.getOutputStream
    tlsOn = true
  }

  /** 外へ送るときの STARTTLS（client として）。verify なら証明書とホスト名を確かめる。 */
  def startClientTls(host: String, verify: Boolean): Unit = {
    pos = 0; lim = 0; partial.reset()
    val ctx = if verify then SSLContext.getDefault else Tls.trustAll
    val s = ctx.getSocketFactory.createSocket(socket, host, socket.getPort, true).asInstanceOf[SSLSocket]
    s.setUseClientMode(true)
    val p = s.getSSLParameters
    p.setProtocols(Array("TLSv1.3", "TLSv1.2").filter(s.getSupportedProtocols.contains))
    if verify then p.setEndpointIdentificationAlgorithm("HTTPS")
    s.setSSLParameters(p)
    s.startHandshake()
    socket = s
    in = s.getInputStream
    out = s.getOutputStream
    tlsOn = true
  }

  def close(): Unit = try socket.close()
  catch case _: Throwable => ()

  /** PROXY プロトコル（v1 の文字列・v2 のバイナリ）を読み、本当の接続元を設定する。 */
  def readProxyHeader(): Boolean = {
    if !fill() then return false
    if buf(pos) == 'P' then
      readLine(108).exists { l =>
        l.split(' ').toList match {
          case "PROXY" :: ("TCP4" | "TCP6") :: src :: _ =>
            clientIp = InetAddress.getByName(src); true
          case "PROXY" :: "UNKNOWN" :: _ => true
          case _ => false
        }
      }
    else if buf(pos) == 0x0d then {
      val sig = Array[Byte](0x0d, 0x0a, 0x0d, 0x0a, 0x00, 0x0d, 0x0a, 0x51, 0x55, 0x49, 0x54, 0x0a)
      readBytes(16).filter(h => h.take(12).sameElements(sig) && (h(12) & 0xf0) == 0x20).flatMap { head =>
        val len = ((head(14) & 0xff) << 8) | (head(15) & 0xff)
        readBytes(len).map { body =>
          val cmd = head(12) & 0x0f
          val fam = (head(13) & 0xf0) >> 4
          if cmd == 1 && fam == 1 && body.length >= 12 then clientIp = InetAddress.getByAddress(body.take(4))
          else if cmd == 1 && fam == 2 && body.length >= 36 then clientIp = InetAddress.getByAddress(body.take(16))
          true
        }
      }.getOrElse(false)
    } else false
  }
}

object Conn {

  /** 認証前のやりとりにかけてよい時間。 */
  val PreAuthMs: Long = 3 * 60 * 1000L

  /** 外からの受信（認証しない SMTP）の1接続にかけてよい時間。 */
  val MxSessionMs: Long = 30 * 60 * 1000L

  /** 接続先の Socket（client 側）。 */
  def client(host: String, port: Int, timeoutMs: Int): Conn = connect(new InetSocketAddress(host, port), timeoutMs)

  /** 名前を引き直さずに、この IP へつなぐ。 */
  def client(ip: InetAddress, port: Int, timeoutMs: Int): Conn = connect(new InetSocketAddress(ip, port), timeoutMs)

  private def connect(addr: InetSocketAddress, timeoutMs: Int): Conn = {
    val s = new Socket()
    s.connect(addr, timeoutMs)
    s.setSoTimeout(timeoutMs)
    new Conn(s, s.getInetAddress, s.getLocalPort)
  }
}
