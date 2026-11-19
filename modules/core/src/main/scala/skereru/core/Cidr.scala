package skereru.core

import java.net.InetAddress

/** CIDR の判定。 */
object Cidr {
  def contains(net: InetAddress, bits: Int, ip: InetAddress): Boolean = {
    val a = net.getAddress; val b = ip.getAddress
    if a.length != b.length || bits < 0 || bits > a.length * 8 then false
    else {
      val full = bits / 8; val rest = bits % 8
      (0 until full).forall(i => a(i) == b(i)) &&
      (rest == 0 || ((a(full) ^ b(full)) & (0xff << (8 - rest)) & 0xff) == 0)
    }
  }

  /** "192.0.2.0/24" や "2001:db8::/32" や単独のアドレス。 */
  def parse(s: String): Option[(InetAddress, Int)] = {
    val (a, b) = s.trim.split('/') match {
      case Array(x) => (x, None)
      case Array(x, y) => (x, y.toIntOption)
      case _ => ("", None)
    }
    if a.isEmpty || !a.forall(c => c.isLetterOrDigit || c == '.' || c == ':') ||
      a.exists(c => c.isLetter && !"abcdefABCDEF".contains(c))
    then None
    else scala.util.Try(InetAddress.getByName(a)).toOption.map(ip => ip -> b.getOrElse(ip.getAddress.length * 8))
  }
}
