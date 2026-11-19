package skereru.api

import java.util.UUID

import skereru.core.ErrorCode

import sbirds.shared.AppError

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skereru.auth.*
import skereru.core.Rules
import skereru.db.*
import skereru.mail.MailConfig
import skereru.shared.*

/** 自分の情報・設定・アプリパスワード・APIキー・宛先の候補・振り分けの規則。 */
final class MeRoutes(
  users: UserStore,
  domains: DomainStore,
  mail: MailStore,
  queue: QueueStore,
  val sessions: SessionAuth,
  mcfg: MailConfig)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "me" => getMe(req)
    case req @ PATCH -> Root / "api" / "me" => patchMe(req)
    case req @ GET -> Root / "api" / "me" / "settings" => getMeSettings(req)
    case req @ PUT -> Root / "api" / "me" / "settings" => putMeSettings(req)
    case req @ PUT -> Root / "api" / "me" / "primary-address" / UuidVar(id) => putMePrimaryAddress(req, id)
    case req @ GET -> Root / "api" / "address-book" => getAddressBook(req)
    case req @ POST -> Root / "api" / "address-book" => postAddressBook(req)
    case req @ DELETE -> Root / "api" / "address-book" => deleteAddressBook(req)
    case req @ GET -> Root / "api" / "contacts" => getContacts(req)
    case req @ GET -> Root / "api" / "me" / "app-passwords" => getMeAppPasswords(req)
    case req @ POST -> Root / "api" / "me" / "app-passwords" => postMeAppPasswords(req)
    case req @ DELETE -> Root / "api" / "me" / "app-passwords" / UuidVar(id) => deleteMeAppPassword(req, id)
    case req @ GET -> Root / "api" / "me" / "api-keys" => getMeApiKeys(req)
    case req @ POST -> Root / "api" / "me" / "api-keys" => postMeApiKeys(req)
    case req @ DELETE -> Root / "api" / "me" / "api-keys" / UuidVar(id) => deleteMeApiKey(req, id)
    case req @ GET -> Root / "api" / "filters" => getFilters(req)
    case req @ POST -> Root / "api" / "filters" => postFilters(req)
    case req @ PUT -> Root / "api" / "filters" / "order" => putFiltersOrder(req)
    case req @ PUT -> Root / "api" / "filters" / UuidVar(id) => putFilter(req, id)
    case req @ DELETE -> Root / "api" / "filters" / UuidVar(id) => deleteFilter(req, id)
  }

  private def getMe(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => me(s).flatMap(m => sessions.appNames(req).flatMap(n => Ok(m.copy(appNames = n)))))

  private def patchMe(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[UpdateMeReq](req)(r =>
        if r.timezone.exists(tz => scala.util.Try(java.time.ZoneId.of(tz)).isFailure) then
          err(ErrorCode.TimezoneInvalid)
        else users.updateMe(s.user.id, r.name, r.timezone) *> NoContent()
      )
    )

  private def getMeSettings(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => users.settings(s.user.id).flatMap(Ok(_)))

  private def putMeSettings(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[MailSettingsDto](req)(x =>
        if x.vacation.enabled && !s.can(ApiScope.Send) then err(ErrorCode.VacationNeedsSend)
        else
          validSettings(x) match {
            case Left(e) => err(ErrorCode.SettingsInvalid, e)
            case Right(v) => users.saveSettings(s.user.id, v) *> Ok(v)
          }
      )
    )

  private def putMePrimaryAddress(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      domains.setPrimary(s.user.id, id).flatMap(n =>
        if n == 0 then err(ErrorCode.AddressNotFound) else NoContent()
      )
    )

  private def getAddressBook(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      users.addressBook(
        s.user.id,
        param(req, "q").getOrElse(""),
        !param(req, "all").contains("true"),
        intParam(req, "limit", 500, 2000)
      ).flatMap(Ok(_))
    )

  private def postAddressBook(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[SaveContactReq](req) { r =>
        if !skereru.core.Address.valid(r.address.trim) then err(ErrorCode.AddressInvalid)
        else users.saveContact(s.user.id, r) *> Ok(UpdatedDto(1))
      }
    )

  private def deleteAddressBook(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      param(req, "address").fold(err(ErrorCode.AddressRequired))(a =>
        users.deleteContact(s.user.id, a).flatMap(n => Ok(UpdatedDto(n)))
      )
    )

  private def getContacts(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      users.contacts(s.user.id, param(req, "q").getOrElse(""), intParam(req, "limit", 10, 50)).flatMap(Ok(_))
    )

  private def getMeAppPasswords(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s => users.appPasswords(s.user.id).flatMap(Ok(_)))

  private def postMeAppPasswords(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      body[CreateAppPasswordReq](req) { r =>
        if r.name.trim.isEmpty then err(ErrorCode.AppPasswordNameRequired)
        else
          for {
            n <- users.appPasswords(s.user.id).map(_.size)
            res <- if n >= 50 then err(ErrorCode.AppPasswordTooMany)
            else {
              val (shown, prefix) = AppPasswords.generate()
              for {
                info <- users.createAppPassword(s.user.id, r.name.trim.take(100), AppPasswords.hash(shown), prefix)
                login <- domains.primaryAddress(s.user.id)
                _ <- audit(users, s, req, "app_password.create", Some(info.id.toString))
                resp <- Created(CreatedAppPasswordDto(shown, login.getOrElse(""), info))
              } yield resp
            }
          } yield res
      }
    )

  private def deleteMeAppPassword(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      users.deleteAppPassword(s.user.id, id).flatMap(n =>
        if n == 0 then err(ErrorCode.AppPasswordNotFound)
        else audit(users, s, req, "app_password.delete", Some(id.toString)) *> NoContent()
      )
    )

  private def getMeApiKeys(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s => users.apiKeys(s.user.id).flatMap(Ok(_)))

  private def postMeApiKeys(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      body[CreateApiKeyReq](req)(r =>
        respond(ApiKeyIssuer.issue(
          users,
          s.user.id,
          r,
          Option.unless(s.isAdmin)(AppError(ErrorCode.ApiKeyAdminScopeAdminOnly))
        ))(c =>
          audit(
            users,
            s,
            req,
            "api_key.create",
            Some(c.info.id.toString),
            Json.obj("scopes" -> c.info.scopes.asJson)
          ) *> Created(c)
        )
      )
    )

  private def deleteMeApiKey(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    sessionOnly(req)(s =>
      users.deleteApiKey(s.user.id, id).flatMap(n =>
        if n == 0 then err(ErrorCode.ApiKeyNotFound)
        else audit(users, s, req, "api_key.delete", Some(id.toString)) *> NoContent()
      )
    )

  private def getFilters(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => queue.rules(s.user.id).flatMap(Ok(_)))

  private def postFilters(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[FilterRuleReq](req)(r =>
        if forwards(r) && !s.can(ApiScope.Send) then needSend
        else
          Rules.validate(r) match {
            case Left(e) => err(ErrorCode.RuleInvalid, e)
            case Right(v) => queue.rules(s.user.id).flatMap(rs =>
                if rs.size >= 200 then err(ErrorCode.RuleTooMany)
                else queue.createRule(s.user.id, v).flatMap(Created(_))
              )
          }
      )
    )

  private def putFiltersOrder(
    req: Request[IO]
  ): IO[Response[IO]] =
    authed(req)(s => body[ReorderReq](req)(r => queue.reorderRules(s.user.id, r.ids) *> NoContent()))

  private def putFilter(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      body[FilterRuleReq](req)(r =>
        if forwards(r) && !s.can(ApiScope.Send) then needSend
        else
          Rules.validate(r) match {
            case Left(e) => err(ErrorCode.RuleInvalid, e)
            case Right(v) => queue.updateRule(s.user.id, id, v).flatMap(n =>
                if n == 0 then err(ErrorCode.RuleNotFound) else NoContent()
              )
          }
      )
    )

  private def deleteFilter(
    req: Request[IO],
    id: UUID
  ): IO[Response[IO]] =
    authed(req)(s =>
      queue.deleteRule(s.user.id, id).flatMap(n => if n == 0 then err(ErrorCode.RuleNotFound) else NoContent())
    )

  def me(
    s: AuthSession
  ): IO[MeDto] =
    for {
      addrs <- domains.addresses(None, Some(s.user.id))
      q <- mail.quota(s.user.id, mcfg.defaultQuotaBytes)
    } yield MeDto(
      s.user.id,
      s.user.name,
      s.user.email,
      s.user.avatarUrl,
      s.isAdmin,
      s.user.timezone,
      addrs.map(a =>
        AddressDto(a.id, a.address, a.domainId, a.userId, a.userName, a.forwardTo, a.isPrimary, a.canSend)
      ),
      q._1,
      q._2,
      s.viaString,
      s.scopes.toList.sorted
    )

  private def validSettings(
    x: MailSettingsDto
  ): Either[String, MailSettingsDto] = {
    val dateOk = (d: Option[String]) => d.forall(v => scala.util.Try(java.time.LocalDate.parse(v)).isSuccess)
    if x.signature.length > 10_000 || x.vacation.body.length > 10_000 then Left("署名・自動返信の本文が長すぎます")
    else if !dateOk(x.vacation.startDate) || !dateOk(x.vacation.endDate) then Left("日付は yyyy-MM-dd で指定してください")
    else if x.allowSenders.size > 1000 || x.blockSenders.size > 1000 then Left("許可・拒否の差出人が多すぎます")
    else
      Right(x.copy(
        pageSize = math.max(10, math.min(200, x.pageSize)),
        allowSenders = x.allowSenders.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct,
        blockSenders = x.blockSenders.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct
      ))
  }

  private def forwards(
    r: FilterRuleReq
  ): Boolean = r.actions.exists(_.kind == RuleAction.Forward)
  private def needSend = err(ErrorCode.RuleForwardNeedsSend)

}
