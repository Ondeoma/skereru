package skereru.api

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
final class MeRoutes(users: UserStore, domains: DomainStore, mail: MailStore, queue: QueueStore, val sessions: SessionAuth, mcfg: MailConfig)
    extends RouteSupport:

  def me(s: AuthSession): IO[MeDto] =
    for
      addrs <- domains.addresses(None, Some(s.user.id))
      q <- mail.quota(s.user.id, mcfg.defaultQuotaBytes)
    yield MeDto(s.user.id, s.user.name, s.user.email, s.user.avatarUrl, s.isAdmin, s.user.timezone,
      addrs.map(a => AddressDto(a.id, a.address, a.domainId, a.userId, a.userName, a.forwardTo, a.isPrimary, a.canSend)), q._1, q._2, s.viaString,
      s.scopes.toList.sorted)

  private def validSettings(x: MailSettingsDto): Either[String, MailSettingsDto] =
    val dateOk = (d: Option[String]) => d.forall(v => scala.util.Try(java.time.LocalDate.parse(v)).isSuccess)
    if x.signature.length > 10_000 || x.vacation.body.length > 10_000 then Left("署名・自動返信の本文が長すぎます")
    else if !dateOk(x.vacation.startDate) || !dateOk(x.vacation.endDate) then Left("日付は yyyy-MM-dd で指定してください")
    else if x.allowSenders.size > 1000 || x.blockSenders.size > 1000 then Left("許可・拒否の差出人が多すぎます")
    else Right(x.copy(pageSize = math.max(10, math.min(200, x.pageSize)),
      allowSenders = x.allowSenders.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct,
      blockSenders = x.blockSenders.map(_.trim.toLowerCase).filter(_.nonEmpty).distinct))

  private def forwards(r: FilterRuleReq): Boolean = r.actions.exists(_.kind == RuleAction.Forward)
  private def needSend = err(Status.Forbidden, "転送を含む規則には send のスコープが必要です")

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "api" / "me" => authed(req)(s => me(s).flatMap(Ok(_)))

    case req @ PATCH -> Root / "api" / "me" =>
      authed(req)(s => body[UpdateMeReq](req)(r =>
        if r.timezone.exists(tz => scala.util.Try(java.time.ZoneId.of(tz)).isFailure) then err(Status.BadRequest, "タイムゾーンが正しくありません")
        else users.updateMe(s.user.id, r.name, r.timezone) *> NoContent()))

    case req @ GET -> Root / "api" / "me" / "settings" => authed(req)(s => users.settings(s.user.id).flatMap(Ok(_)))

    // 自動返信は外へメールを送るので send スコープが必要（write だけの APIキー・MCP から外へ送らせない）
    case req @ PUT -> Root / "api" / "me" / "settings" =>
      authed(req)(s => body[MailSettingsDto](req)(x => if x.vacation.enabled && !s.can(ApiScope.Send) then err(Status.Forbidden, "自動返信の設定には send のスコープが必要です") else validSettings(x) match
        case Left(e) => err(Status.BadRequest, e)
        case Right(v) => users.saveSettings(s.user.id, v) *> Ok(v)))

    case req @ PUT -> Root / "api" / "me" / "primary-address" / UuidVar(id) =>
      authed(req)(s => domains.setPrimary(s.user.id, id).flatMap(n => if n == 0 then err(Status.NotFound, "アドレスが見つかりません") else NoContent()))

    // アドレス帳
    case req @ GET -> Root / "api" / "address-book" =>
      authed(req)(s => users.addressBook(s.user.id, param(req, "q").getOrElse(""), !param(req, "all").contains("true"), intParam(req, "limit", 500, 2000)).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "address-book" =>
      authed(req)(s => body[SaveContactReq](req) { r =>
        if !skereru.core.Address.valid(r.address.trim) then err(Status.BadRequest, "メールアドレスが正しくありません")
        else users.saveContact(s.user.id, r) *> Ok(UpdatedDto(1))
      })

    case req @ DELETE -> Root / "api" / "address-book" =>
      authed(req)(s => param(req, "address").fold(err(Status.BadRequest, "address が必要です"))(a => users.deleteContact(s.user.id, a).flatMap(n => Ok(UpdatedDto(n)))))

    case req @ GET -> Root / "api" / "contacts" =>
      authed(req)(s => users.contacts(s.user.id, param(req, "q").getOrElse(""), intParam(req, "limit", 10, 50)).flatMap(Ok(_)))

    // ---------------------------------------------------------------- アプリパスワード（画面からのみ）

    case req @ GET -> Root / "api" / "me" / "app-passwords" => sessionOnly(req)(s => users.appPasswords(s.user.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "me" / "app-passwords" =>
      sessionOnly(req)(s => body[CreateAppPasswordReq](req) { r =>
        if r.name.trim.isEmpty then err(Status.BadRequest, "名前を入力してください（例: 会社の PC の Thunderbird）")
        else
          for
            n <- users.appPasswords(s.user.id).map(_.size)
            res <- if n >= 50 then err(Status.BadRequest, "アプリパスワードが多すぎます。使っていないものを削除してください")
                   else
                     val (shown, prefix) = AppPasswords.generate()
                     for
                       info <- users.createAppPassword(s.user.id, r.name.trim.take(100), AppPasswords.hash(shown), prefix)
                       login <- domains.primaryAddress(s.user.id)
                       _ <- audit(users, s, req, "app_password.create", Some(info.id.toString))
                       resp <- Created(CreatedAppPasswordDto(shown, login.getOrElse(""), info))
                     yield resp
          yield res
      })

    case req @ DELETE -> Root / "api" / "me" / "app-passwords" / UuidVar(id) =>
      sessionOnly(req)(s => users.deleteAppPassword(s.user.id, id).flatMap(n =>
        if n == 0 then err(Status.NotFound, "見つかりません") else audit(users, s, req, "app_password.delete", Some(id.toString)) *> NoContent()))

    // ---------------------------------------------------------------- APIキー（画面からのみ）

    case req @ GET -> Root / "api" / "me" / "api-keys" => sessionOnly(req)(s => users.apiKeys(s.user.id).flatMap(Ok(_)))

    case req @ POST -> Root / "api" / "me" / "api-keys" =>
      sessionOnly(req)(s => body[CreateApiKeyReq](req)(r =>
        respond(ApiKeyIssuer.issue(users, s.user.id, r, Option.unless(s.isAdmin)(Fail.Forbidden("admin スコープは管理者だけが使えます"))))(c =>
          audit(users, s, req, "api_key.create", Some(c.info.id.toString), Json.obj("scopes" -> c.info.scopes.asJson)) *> Created(c))))

    case req @ DELETE -> Root / "api" / "me" / "api-keys" / UuidVar(id) =>
      sessionOnly(req)(s => users.deleteApiKey(s.user.id, id).flatMap(n =>
        if n == 0 then err(Status.NotFound, "見つかりません") else audit(users, s, req, "api_key.delete", Some(id.toString)) *> NoContent()))

    // ---------------------------------------------------------------- 振り分けの規則

    case req @ GET -> Root / "api" / "filters" => authed(req)(s => queue.rules(s.user.id).flatMap(Ok(_)))

    // 転送を含む規則は外へメールを送るので send スコープが必要（メールの持ち出しを防ぐ）
    case req @ POST -> Root / "api" / "filters" =>
      authed(req)(s => body[FilterRuleReq](req)(r => if forwards(r) && !s.can(ApiScope.Send) then needSend else Rules.validate(r) match
        case Left(e) => err(Status.BadRequest, e)
        case Right(v) => queue.rules(s.user.id).flatMap(rs =>
          if rs.size >= 200 then err(Status.BadRequest, "規則が多すぎます") else queue.createRule(s.user.id, v).flatMap(Created(_)))))

    case req @ PUT -> Root / "api" / "filters" / "order" =>
      authed(req)(s => body[ReorderReq](req)(r => queue.reorderRules(s.user.id, r.ids) *> NoContent()))

    case req @ PUT -> Root / "api" / "filters" / UuidVar(id) =>
      authed(req)(s => body[FilterRuleReq](req)(r => if forwards(r) && !s.can(ApiScope.Send) then needSend else Rules.validate(r) match
        case Left(e) => err(Status.BadRequest, e)
        case Right(v) => queue.updateRule(s.user.id, id, v).flatMap(n => if n == 0 then err(Status.NotFound, "規則が見つかりません") else NoContent())))

    case req @ DELETE -> Root / "api" / "filters" / UuidVar(id) =>
      authed(req)(s => queue.deleteRule(s.user.id, id).flatMap(n => if n == 0 then err(Status.NotFound, "規則が見つかりません") else NoContent()))
  }
