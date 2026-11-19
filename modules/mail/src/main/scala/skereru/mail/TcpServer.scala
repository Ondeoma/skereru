package skereru.mail

import cats.effect.*
import cats.syntax.all.*

import java.net.{InetAddress, InetSocketAddress, ServerSocket, SocketTimeoutException}

/** TCP の待ち受け。接続ごとに仮想スレッドで handler を動かす。 */
object TcpServer {

  def resource(l: Listener,
               bind: String,
               tls: Option[TlsHolder],
               limits: ConnLimits,
               proxyFrom: List[(InetAddress, Int)],
               log: String => Unit)(handler: Conn => Unit): Resource[IO, Unit] =
    Resource.make(IO.blocking {
      val ss = new ServerSocket()
      ss.setReuseAddress(true)
      ss.bind(new InetSocketAddress(bind, l.port), 512)
      val t = Thread.ofVirtual().name(s"accept-${l.name}").start(() =>
        acceptLoop(ss, l, tls, limits, proxyFrom, log, handler)
      )
      (ss, t)
    })((ss, _) => IO.blocking(ss.close())).void

  private def acceptLoop(ss: ServerSocket,
                         l: Listener,
                         tls: Option[TlsHolder],
                         limits: ConnLimits,
                         proxyFrom: List[(InetAddress, Int)],
                         log: String => Unit,
                         handler: Conn => Unit): Unit =
    while !ss.isClosed do
      try {
        val s = ss.accept()
        Thread.ofVirtual().name(s"${l.name}-${s.getInetAddress.getHostAddress}").start { () =>
          val ip = s.getInetAddress
          var counted: Option[InetAddress] = None
          def refuse(): Unit = try { s.getOutputStream.write("421 too many connections\r\n".getBytes); s.close() }
          catch case _: Throwable => ()
          if !limits.acquireTotal() then refuse()
          else
            try {
              s.setSoTimeout(60_000)
              s.setTcpNoDelay(true)
              val conn = new Conn(s, ip, l.port)
              val viaProxy = proxyFrom.exists((net, bits) => skereru.core.Cidr.contains(net, bits, ip))
              if viaProxy then {
                // PROXY ヘッダーは 10 秒以内に来なければ切る
                s.setSoTimeout(10_000)
                if !conn.readProxyHeader() then throw new IllegalStateException("invalid PROXY header")
                s.setSoTimeout(60_000)
              }
              if !limits.acquireIp(conn.clientIp) then refuse()
              else {
                counted = Some(conn.clientIp)
                if l.implicitTls then
                  tls match {
                    case None => throw new IllegalStateException("TLS is not configured")
                    case Some(t) =>
                      val realIp = conn.clientIp
                      conn.startTls(t.context, keepBuffered = true)
                      conn.clientIp = realIp
                  }
                // 認証前のやりとりにかけてよい時間（認証できたら各セッションが延ばす）
                conn.deadline = System.currentTimeMillis() + Conn.PreAuthMs
                handler(conn)
                conn.close()
              }
            } catch {
              case _: SocketTimeoutException => ()
              case _: LineTooLong => ()
              case _: java.io.IOException => ()
              case e: Throwable => log(s"[${l.name}] ${ip.getHostAddress}: $e")
            } finally {
              try s.close()
              catch case _: Throwable => ()
              counted.foreach(limits.releaseIp)
              limits.releaseTotal()
            }
        }
      } catch {
        case _: java.net.SocketException if ss.isClosed => ()
        case e: Throwable => log(s"[${l.name}] accept failed: $e")
      }
}
