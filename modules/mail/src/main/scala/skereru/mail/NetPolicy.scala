package skereru.mail

import java.net.InetAddress

/** 外へつなぐときの決まり（ユーザーやメールの内容が接続先を決めるときに、内部のネットワークへつながせない＝SSRF の対策）。 */
object NetPolicy {
  def internal(ip: InetAddress): Boolean =
    ip.isLoopbackAddress || ip.isSiteLocalAddress || ip.isLinkLocalAddress || ip.isAnyLocalAddress ||
      ip.isMulticastAddress ||
      (ip.getAddress()(0) & 0xfe) == 0xfc || Reserved.exists((n, b) => skereru.core.Cidr.contains(n, b, ip))

  /** そのほかの、外の相手ではないアドレス（CGNAT・試験用・予約・IPv4 を埋め込んだ IPv6 など）。 */
  private val Reserved = List(
    "0.0.0.0/8",
    "100.64.0.0/10",
    "192.0.0.0/24",
    "198.18.0.0/15",
    "240.0.0.0/4",
    "::/96",
    "64:ff9b::/96",
    "64:ff9b:1::/48",
    "2002::/16",
    "100::/64"
  ).flatMap(skereru.core.Cidr.parse)

  /** host の IP のうち、つないでよいもの（allowPrivate なら内部も可）。内部のアドレスが1つでも混ざっていれば Left。 */
  def resolve(dns: JndiDns, host: String, allowPrivate: Boolean): Either[String, InetAddress] = {
    val ips = scala.util.Try(InetAddress.getByName(host)).toOption.filter(_ =>
      host.headOption.exists(c => c.isDigit || c == '[')
    ).toList match {
      case Nil => dns.addresses(host).toOption.getOrElse(Nil)
      case l => l
    }
    if ips.isEmpty then Left(s"$host の IP アドレスが見つかりません")
    else if !allowPrivate && ips.exists(internal) then Left(s"$host は内部のアドレスを指しているため、つなげません")
    else Right(ips.head)
  }
}
