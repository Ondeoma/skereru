package skereru.api

import cats.effect.*
import org.http4s.*
import org.typelevel.ci.CIString

object ReqInfo {
  private val Key = org.typelevel.vault.Key.newKey[SyncIO, String].unsafeRunSync()

  /** 接続元の IP（middleware が決めた値。なければ直接の接続元）。 */
  def clientIp(req: Request[IO]): String =
    req.attributes.lookup(Key).orElse(req.remoteAddr.map(_.toString)).getOrElse("-")

  /** 直接の接続元が trusted（ロードバランサー）のときだけ、X-Forwarded-For の最後の値（ロードバランサーが追記した値）を信じる。 */
  def middleware(trusted: List[(java.net.InetAddress, Int)])(app: HttpApp[IO]): HttpApp[IO] =
    cats.data.Kleisli { req =>
      val direct = req.remoteAddr.map(_.toString)
      val fromProxy = req.remoteAddr.exists(a =>
        scala.util.Try(java.net.InetAddress.getByName(a.toString)).toOption
          .exists(ip =>
            trusted.exists((net, bits) => skereru.core.Cidr.contains(net, bits, ip))
          )
      )
      val ip =
        (if fromProxy then
           req.headers.get(CIString("X-Forwarded-For")).map(_.head.value.split(',').last.trim).filter(_.nonEmpty)
         else None)
          .orElse(direct).getOrElse("-")
      app(req.withAttribute(Key, ip))
    }
}
