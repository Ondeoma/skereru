package skereru.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.`Content-Type`
import skereru.db.*
import skereru.mail.{Delivery, MailConfig}
import skereru.shared.*

/** 管理（ドメイン・DNS・ユーザー・アドレス・送信待ち・監査ログ・統計）。すべて監査ログに残す。 */
final class AdminRoutes(svc: AdminService, users: UserStore, domains: DomainStore, mail: MailStore, queue: QueueStore, delivery: Delivery, dmarc: DmarcStore,
    val sessions: SessionAuth) extends RouteSupport:

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // DMARC の集計レポート（dmarc-reports@<ドメイン> に届いたもの）
    case req @ GET -> Root / "api" / "admin" / "dmarc" => admin(req)(_ => dmarc.list(param(req, "domain")).flatMap(Ok(_)))
    case req @ GET -> Root / "api" / "admin" / "dmarc" / UuidVar(id) / "rows" =>
      admin(req)(_ => dmarc.rows(id).flatMap(_.fold(err(Status.NotFound, "レポートが見つかりません"))(Ok(_))))

    case req @ GET -> Root / "api" / "admin" / "stats" => admin(req)(_ => mail.stats.flatMap(Ok(_)))

    // ---------------------------------------------------------------- ドメイン

    case req @ GET -> Root / "api" / "admin" / "domains" => admin(req)(_ => domains.domainDtos.flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "domains" =>
      admin(req)(s => body[CreateDomainReq](req)(r => respond(svc.createDomain(r))(id =>
        audit(users, s, req, "domain.create", Some(r.name)) *> Created(Json.obj("id" -> id.asJson)))))

    case req @ PATCH -> Root / "api" / "admin" / "domains" / UuidVar(id) =>
      admin(req)(s => body[UpdateDomainReq](req)(r =>
        domains.updateDomain(id, r.enabled, r.autoProvision, r.catchAll).flatMap(n =>
          if n == 0 then err(Status.NotFound, "ドメインが見つかりません")
          else delivery.refreshDomains *> audit(users, s, req, "domain.update", Some(id.toString), r.asJson) *> NoContent())))

    case req @ DELETE -> Root / "api" / "admin" / "domains" / UuidVar(id) =>
      admin(req)(s => domains.domain(id).flatMap {
        case None => err(Status.NotFound, "ドメインが見つかりません")
        case Some(d) =>
          domains.addresses(Some(id), None).flatMap { as =>
            if as.nonEmpty then err(Status.Conflict, s"ドメインにアドレスが ${as.size} 件あります。先にアドレスを削除してください")
            else domains.deleteDomain(id) *> delivery.refreshDomains *> audit(users, s, req, "domain.delete", Some(d.name)) *> NoContent()
          }
      })

    case req @ GET -> Root / "api" / "admin" / "domains" / UuidVar(id) / "dns" => admin(req)(_ => okJson(svc.dnsRecords(id)))

    case req @ POST -> Root / "api" / "admin" / "domains" / UuidVar(id) / "rotate-dkim" =>
      admin(req)(s => svc.rotateDkim(id).flatMap(n =>
        if n == 0 then err(Status.NotFound, "ドメインが見つかりません") else audit(users, s, req, "domain.rotate_dkim", Some(id.toString)) *> NoContent()))

    // ---------------------------------------------------------------- アドレス

    case req @ GET -> Root / "api" / "admin" / "addresses" =>
      admin(req)(_ => domains.addresses(param(req, "domainId").flatMap(x => scala.util.Try(java.util.UUID.fromString(x)).toOption), None)
        .map(_.map(a => AddressDto(a.id, a.address, a.domainId, a.userId, a.userName, a.forwardTo, a.isPrimary, a.canSend))).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "addresses" =>
      admin(req)(s => body[AddressReq](req)(r => respond(svc.createAddress(r))(id =>
        audit(users, s, req, "address.create", Some(r.address), r.asJson) *> Created(Json.obj("id" -> id.asJson)))))

    case req @ PUT -> Root / "api" / "admin" / "addresses" / UuidVar(id) =>
      admin(req)(s => body[AddressReq](req) { r =>
        val fwd = r.forwardTo.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct
        if fwd.exists(f => !skereru.core.Address.valid(f)) then err(Status.BadRequest, "転送先のアドレスが正しくありません")
        else if r.userId.isEmpty && fwd.isEmpty then err(Status.BadRequest, "配送先のユーザーか転送先を指定してください")
        else domains.updateAddress(id, r.userId, fwd, r.canSend && r.userId.isDefined).flatMap(n =>
          if n == 0 then err(Status.NotFound, "アドレスが見つかりません") else audit(users, s, req, "address.update", Some(r.address), r.asJson) *> NoContent())
      })

    case req @ DELETE -> Root / "api" / "admin" / "addresses" / UuidVar(id) =>
      admin(req)(s => domains.addressById(id).flatMap {
        case None => err(Status.NotFound, "アドレスが見つかりません")
        case Some(a) => domains.deleteAddress(id) *> audit(users, s, req, "address.delete", Some(a.address)) *> NoContent()
      })

    // ---------------------------------------------------------------- ユーザー

    case req @ GET -> Root / "api" / "admin" / "users" => admin(req)(_ => svc.adminUsers(param(req, "q")).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "users" =>
      admin(req)(s => body[CreateUserReq](req)(r => respond(svc.createUser(r))(id =>
        audit(users, s, req, "user.create", Some(r.address), Json.obj("name" -> r.name.asJson)) *> Created(Json.obj("id" -> id.asJson)))))

    case req @ PATCH -> Root / "api" / "admin" / "users" / UuidVar(id) =>
      admin(req)(s => body[UpdateUserReq](req) { r =>
        if r.isAdmin.contains(false) && id == s.user.id then err(Status.BadRequest, "自分の管理者の権限は外せません")
        else if r.disabled.contains(true) && id == s.user.id then err(Status.BadRequest, "自分を無効にはできません")
        else if r.quotaMb.exists(q => q < 1 || q > 10_000_000) then err(Status.BadRequest, "容量は 1MB 以上にしてください")
        else users.update(id, r.name.map(_.trim).filter(_.nonEmpty), r.isAdmin, r.disabled, r.quotaMb.map(q => Some(q.toLong * 1024 * 1024))).flatMap(n =>
          if n == 0 then err(Status.NotFound, "ユーザーが見つかりません") else audit(users, s, req, "user.update", Some(id.toString), r.asJson) *> NoContent())
      })

    case req @ DELETE -> Root / "api" / "admin" / "users" / UuidVar(id) =>
      admin(req)(s =>
        if id == s.user.id then err(Status.BadRequest, "自分は削除できません")
        else users.find(id).flatMap {
          case None => err(Status.NotFound, "ユーザーが見つかりません")
          case Some(u) => users.delete(id) *> audit(users, s, req, "user.delete", Some(u.name)) *> NoContent()
        })

    // ---------------------------------------------------------------- 送信待ち・監査ログ

    case req @ GET -> Root / "api" / "admin" / "queue" =>
      admin(req)(_ => queue.list(param(req, "status"), intParam(req, "limit", 100, 500)).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "admin" / "queue" / UuidVar(id) / "retry" =>
      admin(req)(s => queue.retryNow(id) *> audit(users, s, req, "queue.retry", Some(id.toString)) *> NoContent())

    case req @ POST -> Root / "api" / "admin" / "queue" / UuidVar(id) / "cancel" =>
      admin(req)(s => queue.cancel(id) *> audit(users, s, req, "queue.cancel", Some(id.toString)) *> NoContent())

    case req @ GET -> Root / "api" / "admin" / "audit" =>
      admin(req)(_ => users.auditLog(intParam(req, "limit", 100, 500), param(req, "before").flatMap(_.toLongOption), param(req, "action")).flatMap(Ok(_)))
  }

/** MTA-STS のポリシーと、メールソフトの自動設定（Thunderbird の autoconfig）。 */
final class WellKnownRoutes(domains: DomainStore, mcfg: MailConfig, stsMode: String):
  import org.http4s.dsl.io.*

  private def host(req: Request[IO]): String = req.headers.get[org.http4s.headers.Host].map(_.host.toLowerCase).getOrElse("")

  private def xmlEscape(s: String) = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

  private def autoconfig(domain: String): String =
    val h = xmlEscape(mcfg.hostname)
    val d = xmlEscape(domain)
    val incoming = List(
      mcfg.imapsPort.map(p => s"""    <incomingServer type="imap"><hostname>$h</hostname><port>$p</port><socketType>SSL</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></incomingServer>"""),
      mcfg.imapPort.map(p => s"""    <incomingServer type="imap"><hostname>$h</hostname><port>$p</port><socketType>STARTTLS</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></incomingServer>"""),
      mcfg.pop3sPort.map(p => s"""    <incomingServer type="pop3"><hostname>$h</hostname><port>$p</port><socketType>SSL</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></incomingServer>""")
    ).flatten
    val outgoing = List(
      mcfg.smtpsPort.map(p => s"""    <outgoingServer type="smtp"><hostname>$h</hostname><port>$p</port><socketType>SSL</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></outgoingServer>"""),
      mcfg.submissionPort.map(p => s"""    <outgoingServer type="smtp"><hostname>$h</hostname><port>$p</port><socketType>STARTTLS</socketType><username>%EMAILADDRESS%</username><authentication>password-cleartext</authentication></outgoingServer>""")
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

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // https://mta-sts.example.com/.well-known/mta-sts.txt
    case req @ GET -> Root / ".well-known" / "mta-sts.txt" =>
      val h = host(req)
      if !h.startsWith("mta-sts.") then NotFound()
      else domains.domainByName(h.stripPrefix("mta-sts.")).flatMap {
        case None => NotFound()
        case Some(_) =>
          Ok(s"version: STSv1\r\nmode: $stsMode\r\nmx: ${mcfg.hostname}\r\nmax_age: 604800\r\n").map(_.withContentType(`Content-Type`(MediaType.text.plain)))
      }

    case req @ GET -> (Root / ".well-known" / "autoconfig" / "mail" / "config-v1.1.xml" | Root / "mail" / "config-v1.1.xml") =>
      val d = req.params.get("emailaddress").map(skereru.core.Address.domainOf).filter(_.nonEmpty).getOrElse(host(req).stripPrefix("autoconfig."))
      domains.domainByName(d).flatMap {
        case None    => NotFound()
        case Some(x) => Ok(autoconfig(x.name)).map(_.withContentType(`Content-Type`(MediaType.application.xml)))
      }
  }
