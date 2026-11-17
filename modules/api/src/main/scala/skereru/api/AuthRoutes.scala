package skereru.api

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import skereru.auth.*
import skereru.db.*
import skereru.mail.MailConfig
import skereru.shared.*

import java.time.Instant

/** ログイン（OIDC の Authorization Code + PKCE・開発用のログイン）とログアウト。 */
final class AuthRoutes(oidc: Option[OidcClient], users: UserStore, adminSvc: AdminService, val sessions: SessionAuth, cfg: ApiConfig, mcfg: MailConfig)
    extends RouteSupport:

  /** ログイン後に戻る先（同じサイトのパスだけ）。 */
  private def safeReturn(s: Option[String]): String =
    s.filter(SafePath.isLocal).getOrElse("/")

  private def redirect(to: String, cookie: Option[ResponseCookie] = None): IO[Response[IO]] =
    val r = Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to)))
    IO.pure(cookie.fold(r)(r.addCookie))

  private def loginCookie(state: String, maxAge: Long): ResponseCookie =
    ResponseCookie(cfg.loginCookieName, state, maxAge = Some(maxAge), path = Some("/"), httpOnly = true, secure = cfg.cookieSecure,
      sameSite = Some(SameSite.Lax))

  /** callback に来た state が、このブラウザで始めたログインのものか（ほかの人のログインの続きを踏ませる「ログイン CSRF」を防ぐ）。 */
  private def sameBrowser(req: Request[IO], state: String): Boolean =
    req.cookies.find(_.name == cfg.loginCookieName).exists(c =>
      java.security.MessageDigest.isEqual(c.content.getBytes("UTF-8"), state.getBytes("UTF-8")))

  private def loggedIn(u: User, req: Request[IO]): IO[ResponseCookie] =
    sessions.issue(u, req).flatTap(_ => users.audit(Some(u.id), "web", "auth.login", None, Some(ReqInfo.clientIp(req))))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" =>
      val back = safeReturn(param(req, "returnTo"))
      oidc match
        case Some(o) =>
          val state = Crypto.randomToken(); val verifier = Crypto.randomToken(48); val nonce = Crypto.randomToken()
          users.saveLoginState(state, verifier, nonce, back) *>
            o.authorizationUri(state, verifier, nonce).flatMap(u => redirect(u.renderString, Some(loginCookie(state, 600))))
        case None if cfg.devLogin => redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
        case None                 => err(Status.ServiceUnavailable, "OIDC が設定されていません")

    case req @ GET -> Root / "auth" / "callback" =>
      (oidc, param(req, "code"), param(req, "state")) match
        case (Some(_), Some(_), Some(state)) if !sameBrowser(req, state) =>
          err(Status.BadRequest, "ログインを始めたブラウザと違います。もう一度ログインしてください")
        case (Some(o), Some(code), Some(state)) =>
          users.takeLoginState(state).flatMap {
            case None => err(Status.BadRequest, "ログインの有効期限が切れました。もう一度ログインしてください")
            case Some((verifier, nonce, back)) =>
              o.authenticate(code, verifier, nonce, Instant.now()).attempt.flatMap {
                case Left(e) =>
                  users.audit(None, "web", "auth.denied", None, Some(ReqInfo.clientIp(req)), Json.obj("error" -> e.getMessage.take(300).asJson)) *>
                    err(Status.Unauthorized, "ログインできませんでした")
                case Right(id) =>
                  // 確かめられていないメールアドレスでは、ほかのアカウントへの結び付けやメールボックスの自動作成をしない
                  users.login(id.sub, id.name, id.email.filter(_ => id.emailVerified), id.picture, cfg.autoCreateUsers).flatMap {
                    case None => err(Status.Forbidden, "このサーバーのユーザーとして登録されていません。管理者に連絡してください")
                    case Some(u) if u.disabled => err(Status.Forbidden, "このユーザーは無効にされています")
                    case Some(u) =>
                      users.syncGroups(u.id, id.groups) *> adminSvc.autoProvision(u) *> loggedIn(u, req)
                        .flatMap(c => redirect(back, Some(c)).map(_.addCookie(loginCookie("", 0))))
                  }
              }
          }
        case (None, _, _) => err(Status.ServiceUnavailable, "OIDC が設定されていません")
        case _            => err(Status.BadRequest, "code と state が必要です")

    // ほかのサイトからログアウトさせられないよう、ほかの更新と同じく X-Skereru ヘッダーを求める
    case req @ POST -> Root / "auth" / "logout" if !req.headers.get(org.typelevel.ci.CIString("X-Skereru")).isDefined =>
      err(Status.Forbidden, "X-Skereru ヘッダーが必要です")
    case req @ POST -> Root / "auth" / "logout" =>
      sessions.revoke(req).flatMap(c => Ok(Json.obj("redirect" -> "/".asJson)).map(_.addCookie(c)))

    // 開発用: 名前とアドレスだけでログインする（DEV_LOGIN=true で OIDC がないときだけ）
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty =>
      body[Json](req) { j =>
        val c = j.hcursor
        val name = c.get[String]("name").getOrElse("").trim
        val email = c.get[String]("email").toOption.map(_.trim.toLowerCase).filter(_.contains("@"))
        val admin = c.get[Boolean]("admin").getOrElse(false)
        if name.isEmpty then err(Status.BadRequest, "名前を入力してください")
        else
          for
            u0 <- users.devUser(name, email, admin)
            u = u0.copy(email = email.orElse(u0.email))
            _ <- adminSvc.autoProvision(u)
            ck <- loggedIn(u, req)
            r <- Ok(Json.obj("redirect" -> safeReturn(c.get[String]("returnTo").toOption).asJson)).map(_.addCookie(ck))
          yield r
      }

    case GET -> Root / "api" / "client-config" =>
      Ok(ClientConfigDto(cfg.devLogin && oidc.isEmpty, cfg.version, mcfg.hostname, mcfg.imapPort, mcfg.imapsPort, mcfg.pop3Port, mcfg.pop3sPort,
        mcfg.submissionPort, mcfg.smtpsPort, (mcfg.maxMessageBytes / 1024 / 1024).toInt))
  }
