package skereru.mail

import java.net.InetAddress

/** IP をまとめる単位（IPv4 はアドレス、IPv6 は /64。IPv6 は1人が /64 を丸ごと持つことが多いため）。 */
object IpKey {
  def of(
    ip: InetAddress
  ): String = ip match {
    case a: java.net.Inet6Address => prefix(a, 8) + "::/64"
    case a => a.getHostAddress
  }

  /** より広い単位（IPv4 は /24、IPv6 は /48）。たくさんのアドレスを持つ相手に、IP ごとの上限を回避させないため。 */
  def wide(
    ip: InetAddress
  ): String = ip match {
    case a: java.net.Inet6Address => prefix(a, 6) + "::/48"
    case a => a.getAddress.take(3).map(_ & 0xff).mkString(".") + ".0/24"
  }

  private def prefix(
    a: InetAddress,
    bytes: Int
  ): String = a.getAddress.take(bytes).map(b => f"${b & 0xff}%02x").mkString
}
