package skereru.api

import java.util.UUID

import skereru.core.ErrorCode

import sbirds.shared.AppError

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skereru.db.*
import skereru.mail.Delivery
import skereru.shared.*

/** 管理（ドメイン・DNS・ユーザー・アドレス・送信待ち・監査ログ・統計）。すべて監査ログに残す。 */
final class AdminRoutes(svc: AdminService,
                        users: UserStore,
                        domains: DomainStore,
                        mail: MailStore,
                        queue: QueueStore,
                        delivery: Delivery,
                        dmarc: DmarcStore,
                        val sessions: SessionAuth) extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    // DMARC の集計レポート（dmarc-reports@<ドメイン> に届いたもの）
    case req @ GET -> Root / "api" / "admin" / "dmarc" => getAdminDmarc(req)
    case req @ GET -> Root / "api" / "admin" / "dmarc" / UuidVar(id) / "rows" => getAdminDmarcRows(req, id)
    case req @ GET -> Root / "api" / "admin" / "stats" => getAdminStats(req)
    case req @ GET -> Root / "api" / "admin" / "domains" => getAdminDomains(req)
    case req @ POST -> Root / "api" / "admin" / "domains" => postAdminDomains(req)
    case req @ PATCH -> Root / "api" / "admin" / "domains" / UuidVar(id) => patchAdminDomain(req, id)
    case req @ DELETE -> Root / "api" / "admin" / "domains" / UuidVar(id) => deleteAdminDomain(req, id)
    case req @ GET -> Root / "api" / "admin" / "domains" / UuidVar(id) / "dns" => getAdminDomainDns(req, id)
    case req @ POST -> Root / "api" / "admin" / "domains" / UuidVar(id) / "rotate-dkim" =>
      postAdminDomainRotateDkim(req, id)
    case req @ GET -> Root / "api" / "admin" / "addresses" => getAdminAddresses(req)
    case req @ POST -> Root / "api" / "admin" / "addresses" => postAdminAddresses(req)
    case req @ PUT -> Root / "api" / "admin" / "addresses" / UuidVar(id) => putAdminAddress(req, id)
    case req @ DELETE -> Root / "api" / "admin" / "addresses" / UuidVar(id) => deleteAdminAddress(req, id)
    case req @ GET -> Root / "api" / "admin" / "users" => getAdminUsers(req)
    case req @ POST -> Root / "api" / "admin" / "users" => postAdminUsers(req)
    case req @ PATCH -> Root / "api" / "admin" / "users" / UuidVar(id) => patchAdminUser(req, id)
    case req @ DELETE -> Root / "api" / "admin" / "users" / UuidVar(id) => deleteAdminUser(req, id)
    case req @ GET -> Root / "api" / "admin" / "agents" => getAdminAgents(req)
    case req @ POST -> Root / "api" / "admin" / "agents" => postAdminAgents(req)
    case req @ PATCH -> Root / "api" / "admin" / "agents" / UuidVar(id) => patchAdminAgent(req, id)
    case req @ POST -> Root / "api" / "admin" / "agents" / UuidVar(id) / "keys" => postAdminAgentKeys(req, id)
    case req @ DELETE -> Root / "api" / "admin" / "agents" / UuidVar(id) / "keys" / UuidVar(keyId) =>
      deleteAdminAgentKey(req, id, keyId)
    case req @ GET -> Root / "api" / "admin" / "queue" => getAdminQueue(req)
    case req @ POST -> Root / "api" / "admin" / "queue" / UuidVar(id) / "retry" => postAdminQueueRetry(req, id)
    case req @ POST -> Root / "api" / "admin" / "queue" / UuidVar(id) / "cancel" => postAdminQueueCancel(req, id)
    case req @ GET -> Root / "api" / "admin" / "audit" => getAdminAudit(req)
  }

  private def getAdminDmarc(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ => dmarc.list(param(req, "domain")).flatMap(Ok(_)))

  private def getAdminDmarcRows(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(_ => dmarc.rows(id).flatMap(_.fold(err(ErrorCode.DmarcReportNotFound))(Ok(_))))

  private def getAdminStats(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ => mail.stats.flatMap(Ok(_)))

  private def getAdminDomains(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ => domains.domainDtos.flatMap(Ok(_)))

  private def postAdminDomains(req: Request[IO]): IO[Response[IO]] =
    admin(req)(s =>
      body[CreateDomainReq](req)(r =>
        respond(svc.createDomain(r))(id =>
          audit(users, s, req, "domain.create", Some(r.name)) *> Created(Json.obj("id" -> id.asJson))
        )
      )
    )

  private def patchAdminDomain(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      body[UpdateDomainReq](req)(r =>
        domains.updateDomain(id, r.enabled, r.autoProvision, r.catchAll).flatMap(n =>
          if n == 0 then err(ErrorCode.DomainNotFound)
          else
            delivery.refreshDomains *> audit(users, s, req, "domain.update", Some(id.toString), r.asJson) *>
              NoContent()
        )
      )
    )

  private def deleteAdminDomain(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      domains.domain(id).flatMap {
        case None => err(ErrorCode.DomainNotFound)
        case Some(d) =>
          domains.addresses(Some(id), None).flatMap { as =>
            if as.nonEmpty then err(ErrorCode.DomainHasAddresses, s"ドメインにアドレスが ${as.size} 件あります。先にアドレスを削除してください")
            else
              domains.deleteDomain(id) *> delivery.refreshDomains *>
                audit(users, s, req, "domain.delete", Some(d.name)) *> NoContent()
          }
      }
    )

  private def getAdminDomainDns(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(_ => okJson(svc.dnsRecords(id)))

  private def postAdminDomainRotateDkim(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      svc.rotateDkim(id).flatMap(n =>
        if n == 0 then err(ErrorCode.DomainNotFound)
        else audit(users, s, req, "domain.rotate_dkim", Some(id.toString)) *> NoContent()
      )
    )

  private def getAdminAddresses(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ =>
      domains.addresses(
        param(req, "domainId").flatMap(x => scala.util.Try(java.util.UUID.fromString(x)).toOption),
        None
      )
        .map(_.map(a =>
          AddressDto(a.id, a.address, a.domainId, a.userId, a.userName, a.forwardTo, a.isPrimary, a.canSend)
        )).flatMap(Ok(_))
    )

  private def postAdminAddresses(req: Request[IO]): IO[Response[IO]] =
    admin(req)(s =>
      body[AddressReq](req)(r =>
        respond(svc.createAddress(r))(id =>
          audit(users, s, req, "address.create", Some(r.address), r.asJson) *> Created(Json.obj("id" -> id.asJson))
        )
      )
    )

  private def putAdminAddress(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      body[AddressReq](req) { r =>
        val fwd = r.forwardTo.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct
        if fwd.exists(f => !skereru.core.Address.valid(f)) then err(ErrorCode.ForwardAddressInvalid)
        else if r.userId.isEmpty && fwd.isEmpty then err(ErrorCode.AddressTargetRequired)
        else
          domains.updateAddress(id, r.userId, fwd, r.canSend && r.userId.isDefined).flatMap(n =>
            if n == 0 then err(ErrorCode.AddressNotFound)
            else audit(users, s, req, "address.update", Some(r.address), r.asJson) *> NoContent()
          )
      }
    )

  private def deleteAdminAddress(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      domains.addressById(id).flatMap {
        case None => err(ErrorCode.AddressNotFound)
        case Some(a) => domains.deleteAddress(id) *> audit(users, s, req, "address.delete", Some(a.address)) *>
            NoContent()
      }
    )

  private def getAdminUsers(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ => svc.adminUsers(param(req, "q")).flatMap(Ok(_)))

  private def postAdminUsers(req: Request[IO]): IO[Response[IO]] =
    admin(req)(s =>
      body[CreateUserReq](req)(r =>
        respond(svc.createUser(r))(id =>
          audit(users, s, req, "user.create", Some(r.address), Json.obj("name" -> r.name.asJson)) *>
            Created(Json.obj("id" -> id.asJson))
        )
      )
    )

  private def patchAdminUser(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      body[UpdateUserReq](req) { r =>
        if r.isAdmin.contains(false) && id == s.user.id then err(ErrorCode.CannotRemoveOwnAdmin)
        else if r.disabled.contains(true) && id == s.user.id then err(ErrorCode.CannotDisableSelf)
        else if r.quotaMb.exists(q => q < 1 || q > 10_000_000) then err(ErrorCode.QuotaTooSmall)
        else
          users.update(
            id,
            r.name.map(_.trim).filter(_.nonEmpty),
            r.isAdmin,
            r.disabled,
            r.quotaMb.map(q => Some(q.toLong * 1024 * 1024))
          ).flatMap(n =>
            if n == 0 then err(ErrorCode.UserNotFound)
            else audit(users, s, req, "user.update", Some(id.toString), r.asJson) *> NoContent()
          )
      }
    )

  private def deleteAdminUser(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s =>
      if id == s.user.id then err(ErrorCode.CannotDeleteSelf)
      else
        users.find(id).flatMap {
          case None => err(ErrorCode.UserNotFound)
          case Some(u) => users.delete(id) *> audit(users, s, req, "user.delete", Some(u.name)) *> NoContent()
        }
    )

  private def getAdminAgents(req: Request[IO]): IO[Response[IO]] =
    adminSessionOnly(req)(_ => svc.agents.flatMap(Ok(_)))

  private def postAdminAgents(req: Request[IO]): IO[Response[IO]] =
    adminSessionOnly(req)(s =>
      body[CreateAgentReq](req)(r =>
        respond(svc.createAgent(r))(id =>
          audit(
            users,
            s,
            req,
            "agent.create",
            Some(id.toString),
            Json.obj("name" -> r.name.trim.asJson, "address" -> r.address.asJson)
          ) *>
            svc.agent(id).flatMap(a => Created(a))
        )
      )
    )

  private def patchAdminAgent(req: Request[IO], id: UUID): IO[Response[IO]] =
    adminSessionOnly(req)(s =>
      body[UpdateAgentReq](req)(r =>
        withAgent(id) { _ =>
          r.name.traverse(AgentAccounts.validName) match {
            case Left(e) => err(ErrorCode.AgentNameInvalid, e)
            case Right(name) =>
              users.update(id, name, None, r.disabled, None) *>
                audit(users, s, req, "agent.update", Some(id.toString), r.asJson) *>
                svc.agent(id).flatMap(a => Ok(a))
          }
        }
      )
    )

  private def postAdminAgentKeys(req: Request[IO], id: UUID): IO[Response[IO]] =
    adminSessionOnly(req)(s =>
      body[CreateApiKeyReq](req)(r =>
        withAgent(id) { _ =>
          respond(ApiKeyIssuer.issue(users, id, r, Some(AppError(ErrorCode.AgentApiKeyAdminScope))))(c =>
            audit(
              users,
              s,
              req,
              "agent.api_key.create",
              Some(c.info.id.toString),
              Json.obj("agent" -> id.asJson, "scopes" -> c.info.scopes.asJson)
            ) *>
              Created(c)
          )
        }
      )
    )

  private def deleteAdminAgentKey(req: Request[IO], id: UUID, keyId: UUID): IO[Response[IO]] =
    adminSessionOnly(req)(s =>
      withAgent(id)(_ =>
        users.deleteApiKey(id, keyId).flatMap(n =>
          if n == 0 then err(ErrorCode.ApiKeyNotFound)
          else
            audit(users, s, req, "agent.api_key.delete", Some(keyId.toString), Json.obj("agent" -> id.asJson)) *>
              NoContent()
        )
      )
    )

  private def getAdminQueue(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ => queue.list(param(req, "status"), intParam(req, "limit", 100, 500)).flatMap(Ok(_)))

  private def postAdminQueueRetry(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s => queue.retryNow(id) *> audit(users, s, req, "queue.retry", Some(id.toString)) *> NoContent())

  private def postAdminQueueCancel(req: Request[IO], id: UUID): IO[Response[IO]] =
    admin(req)(s => queue.cancel(id) *> audit(users, s, req, "queue.cancel", Some(id.toString)) *> NoContent())

  private def getAdminAudit(req: Request[IO]): IO[Response[IO]] =
    admin(req)(_ =>
      users.auditLog(
        intParam(req, "limit", 100, 500),
        param(req, "before").flatMap(_.toLongOption),
        param(req, "action")
      ).flatMap(Ok(_))
    )

  /** エージェント専用アカウントだけを対象にする（ふつうのユーザーなら 404）。 */
  private def withAgent(id: java.util.UUID)(f: User => IO[Response[IO]]): IO[Response[IO]] =
    users.findAgent(id).flatMap(_.fold(err(ErrorCode.AgentNotFound))(f))

}
