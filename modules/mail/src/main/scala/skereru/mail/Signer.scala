package skereru.mail

import cats.effect.*
import skereru.core.*
import skereru.db.*

import java.nio.charset.StandardCharsets
import java.time.Instant

/** DKIM の署名（From のドメインの鍵で）。 */
final class Signer(
  domains: DomainStore,
  box: skereru.auth.SecretBox,
  dns: Option[Dns] = None) {

  private def key(
    domain: String
  ): IO[Option[(Domain, java.security.PrivateKey)]] =
    domains.domainByName(domain).map(_.flatMap { dom =>
      box.decrypt(dom.dkimPrivate).flatMap(k =>
        scala.util.Try(Dkim.privateKey(java.util.Base64.getDecoder.decode(k))).toOption
      ).map(dom -> _)
    })

  /**
   * 転送するメールに ARC の封印を付ける（転送先で、このサーバーが受け取ったときの認証の結果を引き継いでもらう。
   * 転送すると SPF・DKIM が通らなくなりやすく、DMARC で拒否されるのを防ぐ）。domain は転送するアドレスのドメイン。
   * authHeader は受信時に付けた Authentication-Results のヘッダー全体。
   */
  def arcSeal(
    raw: Array[Byte],
    domain: String,
    authHeader: Option[String],
    hostname: String
  ): IO[Array[Byte]] =
    key(domain).flatMap {
      case None => IO.pure(raw)
      case Some((dom, pk)) =>
        IO.blocking {
          val chain = dns.fold(ArcResult("none", 0, ""))(d => new Arc(d).verify(raw, Instant.now()))
          // "Authentication-Results: host;\r\n\tspf=…" から、host; の後ろを1行にして渡す
          val results = authHeader.map(h => h.drop(h.indexOf(';') + 1).replaceAll("\r?\n[ \t]*", " ").trim).filter(
            _.nonEmpty
          ).getOrElse("none")
          val seal = Arc.seal(raw, dom.name, dom.dkimSelector, pk, hostname, results, chain, Instant.now())
          seal.getBytes(StandardCharsets.ISO_8859_1) ++ raw
        }
    }

  def sign(
    raw: Array[Byte]
  ): IO[Array[Byte]] = {
    val root = Mime.parse(raw)
    root.header("From").flatMap(Address.parseOne).map(_.domain) match {
      case None => IO.pure(raw)
      case Some(d) =>
        domains.domainByName(d).map {
          case None => raw
          case Some(dom) =>
            box.decrypt(dom.dkimPrivate).flatMap(k =>
              scala.util.Try(java.util.Base64.getDecoder.decode(k)).toOption
            ) match {
              case None => raw
              case Some(pk) =>
                Dkim.sign(
                  raw,
                  dom.name,
                  dom.dkimSelector,
                  Dkim.privateKey(pk),
                  Instant.now()
                ).getBytes(StandardCharsets.ISO_8859_1) ++ raw
            }
        }
    }
  }
}
