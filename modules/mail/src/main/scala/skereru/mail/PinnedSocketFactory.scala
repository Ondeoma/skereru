package skereru.mail

import java.net.{InetAddress, InetSocketAddress, Socket, SocketAddress}

/** 決まった IP へつなぐソケット（connect に渡された名前の IP は使わない）。 */
final class PinnedSocketFactory(ip: InetAddress) extends javax.net.SocketFactory {
  private def pinned(): Socket = new Socket {
    override def connect(ep: SocketAddress, timeout: Int): Unit = ep match {
      case a: InetSocketAddress => super.connect(new InetSocketAddress(ip, a.getPort), timeout)
      case _ => throw new java.io.IOException("unsupported address")
    }
    override def connect(ep: SocketAddress): Unit = connect(ep, 0)
  }
  override def createSocket(): Socket = pinned()
  override def createSocket(host: String, port: Int): Socket = {
    val s = pinned(); s.connect(new InetSocketAddress(ip, port), 15000); s
  }
  override def createSocket(host: String, port: Int, la: InetAddress, lp: Int): Socket = createSocket(host, port)
  override def createSocket(addr: InetAddress, port: Int): Socket = createSocket("", port)
  override def createSocket(addr: InetAddress, port: Int, la: InetAddress, lp: Int): Socket = createSocket("", port)
}
