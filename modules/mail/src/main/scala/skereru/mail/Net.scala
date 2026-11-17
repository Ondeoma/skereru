package skereru.mail

import cats.effect.*
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import skereru.observe.{Metrics, OpenSpan, Telemetry}

import java.io.{ByteArrayOutputStream, InputStream, OutputStream}
import java.net.{InetAddress, InetSocketAddress, ServerSocket, Socket, SocketTimeoutException}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap
import javax.net.ssl.{SSLContext, SSLSocket}

/** 仮想スレッドのブロッキングな処理から、IO（DB など）を呼ぶ橋渡し。scope があれば、動かす IO はそのスパンの子になる。 */
final class Bridge(d: Dispatcher[IO], scope: Option[OpenSpan] = None):
  def run[A](io: IO[A]): A = d.unsafeRunSync(scope.fold(io)(_.within(io)))

  /** body（セッション1つなど）をスパンで囲む。body に渡す Bridge で動かした IO は、このスパンの子になる。 */
  def span[A](name: String, kv: (String, String)*)(body: Bridge => A): A =
    val (s, close) = run(Telemetry.open(name, kv*).allocated)
    try body(new Bridge(d, Some(s)))
    catch
      case e: (java.io.IOException | LineTooLong) => throw e // 切断・タイムアウトはふつうの終わり
      case e: Throwable => { run(s.fail(e)); throw e }
    finally run(close)

  /** body の間だけ、メトリクス name（接続数など）を1つ増やす。 */
  def tracking[A](name: String, kv: (String, String)*)(body: => A): A =
    run(Metrics.upDown(name, 1, kv*))
    try body finally run(Metrics.upDown(name, -1, kv*))

/** 行の長さの上限を超えた。 */
final class LineTooLong extends Exception("line too long")

/** 接続（1行ずつの読み書き・STARTTLS）。読みかけのデータはタイムアウトしても失われない。 */
final class Conn(private var socket: Socket, val remote: InetAddress, val localPort: Int):
  private var in: InputStream   = socket.getInputStream
  private var out: OutputStream = socket.getOutputStream
  private val buf               = new Array[Byte](16 * 1024)
  private var pos               = 0
  private var lim               = 0
  private val partial           = new ByteArrayOutputStream()
  @volatile private var tlsOn   = false

  /** PROXY プロトコルで知らされた本当の接続元（ロードバランサーの後ろにいるとき）。 */
  var clientIp: InetAddress = remote
  /** この時刻（ミリ秒）を過ぎたら、次の読み込みで切る。1回の読み込みごとのタイムアウトだけだと、少しずつ送って接続を保てるため。
    * 認証前は短く、認証できたら各セッションが延ばす。 */
  var deadline: Long = Long.MaxValue

  def isTls: Boolean = tlsOn
  def tlsInfo: String = socket match
    case s: SSLSocket => s"${s.getSession.getProtocol} ${s.getSession.getCipherSuite}"
    case _            => ""

  def setTimeout(ms: Int): Unit = socket.setSoTimeout(ms)

  private def fill(): Boolean =
    if pos < lim then true
    else
      if System.currentTimeMillis() > deadline then throw new java.io.EOFException("session deadline")
      val n = in.read(buf)
      if n <= 0 then false
      else
        pos = 0; lim = n; true

  /** 読み込み済みでまだ使っていないバイトがあるか（STARTTLS の直後に平文が残っていれば攻撃）。 */
  def hasBuffered: Boolean = pos < lim

  /** 1行（CRLF・LF を除く）。max バイトを超えたら LineTooLong。接続が閉じたら None。 */
  def readLine(max: Int = 8192): Option[String] = readLineBytes(max).map(b => new String(b, StandardCharsets.UTF_8))

  def readLineBytes(max: Int = 8192): Option[Array[Byte]] =
    var done = false
    var eof  = false
    while !done && !eof do
      if !fill() then eof = true
      else
        var i = pos
        while i < lim && buf(i) != '\n' do i += 1
        partial.write(buf, pos, i - pos)
        if i < lim then { pos = i + 1; done = true }
        else pos = lim
        if partial.size() > max then
          partial.reset()
          // 行の残りを読み捨てる
          var skipping = !done
          while skipping && fill() do
            var j = pos
            while j < lim && buf(j) != '\n' do j += 1
            if j < lim then { pos = j + 1; skipping = false } else pos = lim
          throw LineTooLong()
    if eof && partial.size() == 0 then None
    else
      val b = partial.toByteArray
      partial.reset()
      Some(if b.nonEmpty && b.last == '\r' then b.dropRight(1) else b)

  /** 改行を含めた1行（SMTP の DATA で、行末が CRLF かを確かめるため）。接続が閉じたら None。max を超えた分は切って返す。 */
  def readRawLine(max: Int): Option[Array[Byte]] =
    val out = new ByteArrayOutputStream(256)
    var done = false
    while !done do
      if !fill() then
        done = true
      else
        var i = pos
        while i < lim && buf(i) != '\n' do i += 1
        val end = if i < lim then i + 1 else lim
        if out.size() < max then out.write(buf, pos, math.min(end - pos, max - out.size()))
        pos = end
        if i < lim then done = true
    if out.size() == 0 then None else Some(out.toByteArray)

  /** ちょうど n バイト（IMAP のリテラル）。 */
  def readBytes(n: Int): Option[Array[Byte]] =
    // 宣言された大きさの配列を先に確保しない（届いていないデータのためにメモリを使わせない）
    val out = new ByteArrayOutputStream(math.min(n, 64 * 1024))
    var ok = true
    while ok && out.size() < n do
      if !fill() then ok = false
      else
        val k = math.min(n - out.size(), lim - pos)
        out.write(buf, pos, k)
        pos += k
    Option.when(ok)(out.toByteArray)

  def write(s: String): Unit = { out.write(s.getBytes(StandardCharsets.UTF_8)); out.flush() }
  def writeBytes(b: Array[Byte]): Unit = out.write(b)
  def writeRaw(s: String): Unit = out.write(s.getBytes(StandardCharsets.ISO_8859_1))
  def flush(): Unit = out.flush()

  /** 平文の接続を TLS にする。
    * STARTTLS では読み込み済みの平文を捨てる（STARTTLS インジェクションの対策）。
    * 接続の直後から TLS のポート（993 など）では、PROXY ヘッダーと一緒に読み込んだ TLS の最初のデータを引き継ぐ（keepBuffered）。 */
  def startTls(ctx: SSLContext, keepBuffered: Boolean = false): Unit =
    val consumed = if keepBuffered && pos < lim then java.util.Arrays.copyOfRange(buf, pos, lim) else Array.emptyByteArray
    pos = 0; lim = 0; partial.reset()
    val s = (if consumed.nonEmpty then ctx.getSocketFactory.createSocket(socket, new java.io.ByteArrayInputStream(consumed), true)
             else ctx.getSocketFactory.createSocket(socket, socket.getInetAddress.getHostAddress, socket.getPort, true)).asInstanceOf[SSLSocket]
    s.setUseClientMode(false)
    Tls.harden(s)
    s.startHandshake()
    socket = s
    in = s.getInputStream
    out = s.getOutputStream
    tlsOn = true

  /** 外へ送るときの STARTTLS（client として）。verify なら証明書とホスト名を確かめる。 */
  def startClientTls(host: String, verify: Boolean): Unit =
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

  def close(): Unit = try socket.close() catch case _: Throwable => ()

  /** PROXY プロトコル（v1 の文字列・v2 のバイナリ）を読み、本当の接続元を設定する。 */
  def readProxyHeader(): Boolean =
    if !fill() then return false
    if buf(pos) == 'P' then
      readLine(108).exists { l =>
        l.split(' ').toList match
          case "PROXY" :: ("TCP4" | "TCP6") :: src :: _ =>
            clientIp = InetAddress.getByName(src); true
          case "PROXY" :: "UNKNOWN" :: _ => true
          case _ => false
      }
    else if buf(pos) == 0x0d then
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
    else false

object Conn:
  /** 認証前のやりとりにかけてよい時間。 */
  val PreAuthMs: Long = 3 * 60 * 1000L
  /** 外からの受信（認証しない SMTP）の1接続にかけてよい時間。 */
  val MxSessionMs: Long = 30 * 60 * 1000L

  /** 接続先の Socket（client 側）。 */
  def client(host: String, port: Int, timeoutMs: Int): Conn = connect(new InetSocketAddress(host, port), timeoutMs)

  /** 名前を引き直さずに、この IP へつなぐ。 */
  def client(ip: InetAddress, port: Int, timeoutMs: Int): Conn = connect(new InetSocketAddress(ip, port), timeoutMs)

  private def connect(addr: InetSocketAddress, timeoutMs: Int): Conn =
    val s = new Socket()
    s.connect(addr, timeoutMs)
    s.setSoTimeout(timeoutMs)
    new Conn(s, s.getInetAddress, s.getLocalPort)

/** 外へつなぐときの決まり（ユーザーやメールの内容が接続先を決めるときに、内部のネットワークへつながせない＝SSRF の対策）。 */
object NetPolicy:
  def internal(ip: InetAddress): Boolean =
    ip.isLoopbackAddress || ip.isSiteLocalAddress || ip.isLinkLocalAddress || ip.isAnyLocalAddress || ip.isMulticastAddress ||
      (ip.getAddress()(0) & 0xfe) == 0xfc || Reserved.exists((n, b) => skereru.core.Cidr.contains(n, b, ip))

  /** そのほかの、外の相手ではないアドレス（CGNAT・試験用・予約・IPv4 を埋め込んだ IPv6 など）。 */
  private val Reserved = List("0.0.0.0/8", "100.64.0.0/10", "192.0.0.0/24", "198.18.0.0/15", "240.0.0.0/4",
    "::/96", "64:ff9b::/96", "64:ff9b:1::/48", "2002::/16", "100::/64").flatMap(skereru.core.Cidr.parse)

  /** host の IP のうち、つないでよいもの（allowPrivate なら内部も可）。内部のアドレスが1つでも混ざっていれば Left。 */
  def resolve(dns: JndiDns, host: String, allowPrivate: Boolean): Either[String, InetAddress] =
    val ips = scala.util.Try(InetAddress.getByName(host)).toOption.filter(_ => host.headOption.exists(c => c.isDigit || c == '[')).toList match
      case Nil => dns.addresses(host).toOption.getOrElse(Nil)
      case l   => l
    if ips.isEmpty then Left(s"$host の IP アドレスが見つかりません")
    else if !allowPrivate && ips.exists(internal) then Left(s"$host は内部のアドレスを指しているため、つなげません")
    else Right(ips.head)

/** 待ち受けの設定。 */
final case class Listener(name: String, port: Int, implicitTls: Boolean)

/** IP をまとめる単位（IPv4 はアドレス、IPv6 は /64。IPv6 は1人が /64 を丸ごと持つことが多いため）。 */
object IpKey:
  def of(ip: InetAddress): String = ip match
    case a: java.net.Inet6Address => prefix(a, 8) + "::/64"
    case a                        => a.getHostAddress

  /** より広い単位（IPv4 は /24、IPv6 は /48）。たくさんのアドレスを持つ相手に、IP ごとの上限を回避させないため。 */
  def wide(ip: InetAddress): String = ip match
    case a: java.net.Inet6Address => prefix(a, 6) + "::/48"
    case a                        => a.getAddress.take(3).map(_ & 0xff).mkString(".") + ".0/24"

  private def prefix(a: InetAddress, bytes: Int): String = a.getAddress.take(bytes).map(b => f"${b & 0xff}%02x").mkString

/** 接続数の制限（全体・IP ごと・広い単位ごと）。IP ごとの数は、PROXY プロトコルで知った本当の接続元で数える。 */
final class ConnLimits(maxTotal: Int, maxPerIp: Int):
  private val total = new AtomicInteger(0)
  private val perIp = new ConcurrentHashMap[String, AtomicInteger]()
  private val maxPerNet = maxPerIp * 4

  def acquireTotal(): Boolean =
    if total.incrementAndGet() > maxTotal then { total.decrementAndGet(); false } else true

  def releaseTotal(): Unit = total.decrementAndGet()

  private def acquire(k: String, max: Int): Boolean =
    val c = perIp.computeIfAbsent(k, _ => new AtomicInteger(0))
    if c.incrementAndGet() > max then { c.decrementAndGet(); false } else true

  private def release(k: String): Unit =
    val c = perIp.get(k)
    if c != null && c.decrementAndGet() <= 0 then perIp.remove(k, c)

  def acquireIp(ip: InetAddress): Boolean =
    if !acquire(IpKey.of(ip), maxPerIp) then false
    else if !acquire("net:" + IpKey.wide(ip), maxPerNet) then { release(IpKey.of(ip)); false }
    else true

  def releaseIp(ip: InetAddress): Unit =
    release(IpKey.of(ip)); release("net:" + IpKey.wide(ip))

  def current: Int = total.get()

/** TCP の待ち受け。接続ごとに仮想スレッドで handler を動かす。 */
object TcpServer:

  def resource(l: Listener, bind: String, tls: Option[TlsHolder], limits: ConnLimits, proxyFrom: List[(InetAddress, Int)], log: String => Unit)(
      handler: Conn => Unit): Resource[IO, Unit] =
    Resource.make(IO.blocking {
      val ss = new ServerSocket()
      ss.setReuseAddress(true)
      ss.bind(new InetSocketAddress(bind, l.port), 512)
      val t = Thread.ofVirtual().name(s"accept-${l.name}").start(() => acceptLoop(ss, l, tls, limits, proxyFrom, log, handler))
      (ss, t)
    })((ss, _) => IO.blocking(ss.close())).void

  private def acceptLoop(ss: ServerSocket, l: Listener, tls: Option[TlsHolder], limits: ConnLimits, proxyFrom: List[(InetAddress, Int)],
      log: String => Unit, handler: Conn => Unit): Unit =
    while !ss.isClosed do
      try
        val s = ss.accept()
        Thread.ofVirtual().name(s"${l.name}-${s.getInetAddress.getHostAddress}").start { () =>
          val ip = s.getInetAddress
          var counted: Option[InetAddress] = None
          def refuse(): Unit = try { s.getOutputStream.write("421 too many connections\r\n".getBytes); s.close() } catch case _: Throwable => ()
          if !limits.acquireTotal() then refuse()
          else
            try
              s.setSoTimeout(60_000)
              s.setTcpNoDelay(true)
              val conn = new Conn(s, ip, l.port)
              val viaProxy = proxyFrom.exists((net, bits) => skereru.core.Cidr.contains(net, bits, ip))
              if viaProxy then
                // PROXY ヘッダーは 10 秒以内に来なければ切る
                s.setSoTimeout(10_000)
                if !conn.readProxyHeader() then throw new IllegalStateException("invalid PROXY header")
                s.setSoTimeout(60_000)
              if !limits.acquireIp(conn.clientIp) then refuse()
              else
                counted = Some(conn.clientIp)
                if l.implicitTls then
                  tls match
                    case None => throw new IllegalStateException("TLS is not configured")
                    case Some(t) =>
                      val realIp = conn.clientIp
                      conn.startTls(t.context, keepBuffered = true)
                      conn.clientIp = realIp
                // 認証前のやりとりにかけてよい時間（認証できたら各セッションが延ばす）
                conn.deadline = System.currentTimeMillis() + Conn.PreAuthMs
                handler(conn)
                conn.close()
            catch
              case _: SocketTimeoutException => ()
              case _: LineTooLong             => ()
              case _: java.io.IOException     => ()
              case e: Throwable               => log(s"[${l.name}] ${ip.getHostAddress}: $e")
            finally
              try s.close() catch case _: Throwable => ()
              counted.foreach(limits.releaseIp)
              limits.releaseTotal()
        }
      catch
        case _: java.net.SocketException if ss.isClosed => ()
        case e: Throwable => log(s"[${l.name}] accept failed: $e")
