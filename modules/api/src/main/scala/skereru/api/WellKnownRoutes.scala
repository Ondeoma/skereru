package skereru.api

import skereru.core.ErrorCode

import sbirds.http.Errors

import cats.effect.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.`Content-Type`
import skereru.db.*
import skereru.mail.MailConfig

/** MTA-STS のポリシーと、メールソフトの自動設定（Thunderbird の autoconfig）。 */
final class WellKnownRoutes(
  domains: DomainStore,
  mcfg: MailConfig,
  stsMode: String) {
  import org.http4s.dsl.io.*

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // https://mta-sts.example.com/.well-known/mta-sts.txt
    case req @ GET -> Root / ".well-known" / "mta-sts.txt" => getWellKnownMtaStsTxt(req)
    case req @ GET ->
        (Root / ".well-known" / "autoconfig" / "mail" / "config-v1.1.xml" | Root / "mail" / "config-v1.1.xml") =>
      getMailAutoconfig(req)
  }

  private def getWellKnownMtaStsTxt(
    req: Request[IO]
  ): IO[Response[IO]] = {
    val h = host(req)
    if !h.startsWith("mta-sts.") then Errors.respond(ErrorCode.MailConfigNotFound)
    else
      domains.domainByName(h.stripPrefix("mta-sts.")).flatMap {
        case None => Errors.respond(ErrorCode.MailConfigNotFound)
        case Some(_) =>
          Ok(
            s"version: STSv1\r\nmode: $stsMode\r\nmx: ${mcfg.hostname}\r\nmax_age: 604800\r\n"
          ).map(_.withContentType(`Content-Type`(MediaType.text.plain)))
      }
  }

  private def getMailAutoconfig(
    req: Request[IO]
  ): IO[Response[IO]] = {
    val d = req.params.get("emailaddress").map(skereru.core.Address.domainOf).filter(_.nonEmpty).getOrElse(host(
      req
    ).stripPrefix("autoconfig."))
    domains.domainByName(d).flatMap {
      case None => Errors.respond(ErrorCode.MailConfigNotFound)
      case Some(x) => Ok(autoconfig(x.name)).map(_.withContentType(`Content-Type`(MediaType.application.xml)))
    }
  }

  private def host(
    req: Request[IO]
  ): String = req.headers.get[org.http4s.headers.Host].map(_.host.toLowerCase).getOrElse("")

  private def xmlEscape(
    s: String
  ) = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

  private def autoconfig(
    domain: String
  ): String = {
    val h = xmlEscape(mcfg.hostname)
    val d = xmlEscape(domain)
    val incoming = List(
      mcfg.imapsPort.map(p =>
        s"""    <incomingServer type="imap"><hostname>$h</hostname><port>$p</port><socketType>SSL</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></incomingServer>"""
      ),
      mcfg.imapPort.map(p =>
        s"""    <incomingServer type="imap"><hostname>$h</hostname><port>$p</port><socketType>STARTTLS</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></incomingServer>"""
      ),
      mcfg.pop3sPort.map(p =>
        s"""    <incomingServer type="pop3"><hostname>$h</hostname><port>$p</port><socketType>SSL</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></incomingServer>"""
      )
    ).flatten
    val outgoing = List(
      mcfg.smtpsPort.map(p =>
        s"""    <outgoingServer type="smtp"><hostname>$h</hostname><port>$p</port><socketType>SSL</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></outgoingServer>"""
      ),
      mcfg.submissionPort.map(p =>
        s"""    <outgoingServer type="smtp"><hostname>$h</hostname><port>$p</port><socketType>STARTTLS</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></outgoingServer>"""
      )
    ).flatten
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<clientConfig version="1.1">
       |  <emailProvider id="$d">
       |    <domain>$d</domain>
       |    <displayName>$d</displayName>
       |${(incoming ++ outgoing).mkString("\n")}
       |  </emailProvider>
       |</clientConfig>
       |""".stripMargin
  }

}
