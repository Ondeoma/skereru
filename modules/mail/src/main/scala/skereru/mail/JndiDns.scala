package skereru.mail

import skereru.core.Dns

import java.net.InetAddress
import java.util.Hashtable
import java.util.concurrent.ConcurrentHashMap
import javax.naming.directory.InitialDirContext
import javax.naming.{NameNotFoundException, NamingException}

/** JNDI（JDK 標準）の DNS。servers が空なら OS の設定を使う。結果は ttlSeconds の間覚えておく。 */
final class JndiDns(
  servers: List[String],
  timeoutMs: Int = 3000,
  ttlSeconds: Int = 300) extends Dns {

  private val cache = new ConcurrentHashMap[String, (Long, Either[String, List[String]])]()

  private def env: Hashtable[String, String] = {
    val e = new Hashtable[String, String]()
    e.put("java.naming.factory.initial", "com.sun.jndi.dns.DnsContextFactory")
    e.put("java.naming.provider.url", if servers.isEmpty then "dns:" else servers.map(s => s"dns://$s").mkString(" "))
    e.put("com.sun.jndi.dns.timeout.initial", timeoutMs.toString)
    e.put("com.sun.jndi.dns.timeout.retries", "2")
    e
  }

  private def query(
    name: String,
    kind: String
  ): Either[String, List[String]] = {
    val key = s"$kind:$name"
    val now = System.currentTimeMillis()
    Option(cache.get(key)).filter(_._1 > now).map(_._2).getOrElse {
      val r: Either[String, List[String]] =
        try {
          val ctx = new InitialDirContext(env)
          try {
            val attrs = ctx.getAttributes(name.stripSuffix(".") + ".", Array(kind))
            Right(
              Option(attrs.get(kind)).map(a => (0 until a.size()).map(i => a.get(i).toString).toList).getOrElse(Nil)
            )
          } finally ctx.close()
        } catch {
          case _: NameNotFoundException => Right(Nil)
          case e: NamingException
              if Option(e.getMessage).exists(m => m.contains("DNS name not found") || m.contains("NXDOMAIN")) =>
            Right(Nil)
          case e: NamingException => Left(Option(e.getMessage).getOrElse(e.toString))
        }
      if cache.size() > 50_000 then cache.clear()
      // 一時的な失敗は短い間だけ覚える
      cache.put(key, (now + (if r.isRight then ttlSeconds * 1000L else 30_000L), r))
      r
    }
  }

  /** TXT の値（"…" "…" に分かれた文字列は連結する）。 */
  def txt(
    name: String
  ): Either[String, List[String]] =
    query(name, "TXT").map(_.map { raw =>
      val parts = "\"((?:[^\"\\\\]|\\\\.)*)\"".r.findAllMatchIn(raw).map(_.group(1).replace(
        "\\\"",
        "\""
      ).replace("\\\\", "\\")).toList
      if parts.isEmpty then raw else parts.mkString
    })

  def addresses(
    name: String
  ): Either[String, List[InetAddress]] =
    for {
      a <- query(name, "A")
      aaaa <- query(name, "AAAA")
    } yield (a ++ aaaa).flatMap(s => scala.util.Try(InetAddress.getByName(s)).toOption)

  def mx(
    name: String
  ): Either[String, List[(Int, String)]] =
    query(name, "MX").map(_.flatMap { r =>
      r.trim.split("\\s+") match {
        case Array(p, h) => p.toIntOption.map(_ -> h.stripSuffix(".").toLowerCase)
        case _ => None
      }
    }.sortBy(_._1))

  def ptr(
    ip: InetAddress
  ): Either[String, List[String]] = {
    val name = ip match {
      case a: java.net.Inet4Address => a.getAddress.reverse.map(b => (b & 0xff).toString).mkString(".") +
          ".in-addr.arpa"
      case a =>
        a.getAddress.reverse.flatMap(b => List(b & 0xf, (b >> 4) & 0xf)).map(Integer.toHexString).mkString(".") +
          ".ip6.arpa"
    }
    query(name, "PTR").map(_.map(_.stripSuffix(".").toLowerCase))
  }

  /** 逆引きの名前で、正引きすると同じ IP に戻るもの（FCrDNS）。 */
  def verifiedReverse(
    ip: InetAddress
  ): Option[String] =
    ptr(ip).toOption.toList.flatten.take(5).find(n => addresses(n).toOption.exists(_.contains(ip)))

  /** DNSBL に載っているか（zones のうち載っているもの）。 */
  def dnsbl(
    ip: InetAddress,
    zones: List[String]
  ): List[String] = ip match {
    case a: java.net.Inet4Address =>
      val rev = a.getAddress.reverse.map(b => (b & 0xff).toString).mkString(".")
      zones.filter { z =>
        query(s"$rev.$z", "A").toOption.exists(_.exists(r =>
          r.startsWith("127.") && r != "127.255.255.254" && r != "127.255.255.255"
        ))
      }
    case _ => Nil
  }
}
