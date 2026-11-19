package skereru.api

import skereru.core.ErrorCode

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Location
import skereru.auth.*
import sbirds.auth.Crypto
import skereru.db.*
import skereru.mail.MailConfig
import skereru.shared.*

import java.time.Instant

/** ログイン（OIDC の Authorization Code + PKCE・開発用のログイン）とログアウト。 */
final class AuthRoutes(
  oidc: Option[OidcClient],
  users: UserStore,
  adminSvc: AdminService,
  val sessions: SessionAuth,
  cfg: ApiConfig,
  mcfg: MailConfig)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" => getAuthLogin(req)
    case req @ GET -> Root / "auth" / "callback" => getAuthCallback(req)
    case req @ POST -> Root / "auth" / "logout" if !req.headers.get(org.typelevel.ci.CIString("X-Skereru")).isDefined =>
      rejectLogoutWithoutCsrf(req)
    case req @ POST -> Root / "auth" / "logout" => postAuthLogout(req)
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty => postAuthDevLogin(req)
    case GET -> Root / "api" / "client-config" => getClientConfig()
  }

  private def getAuthLogin(
    req: Request[IO]
  ): IO[Response[IO]] = {
    val back = safeReturn(param(req, "returnTo"))
    oidc match {
      case Some(o) =>
        val state = Crypto.randomToken(); val verifier = Crypto.randomToken(48); val nonce = Crypto.randomToken()
        users.saveLoginState(state, verifier, nonce, back) *>
          o.authorizationUri(state, verifier, nonce).flatMap(u =>
            redirect(u.renderString, Some(loginCookie(state, 600)))
          )
      case None if cfg.devLogin => redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
      case None => err(ErrorCode.OidcNotConfigured)
    }
  }

  private def getAuthCallback(
    req: Request[IO]
  ): IO[Response[IO]] =
    (oidc, param(req, "code"), param(req, "state")) match {
      case (Some(_), Some(_), Some(state)) if !sameBrowser(req, state) =>
        err(ErrorCode.LoginBrowserMismatch)
      case (Some(o), Some(code), Some(state)) =>
        users.takeLoginState(state).flatMap {
          case None => err(ErrorCode.LoginExpired)
          case Some((verifier, nonce, back)) =>
            o.authenticate(code, verifier, nonce, Instant.now()).attempt.flatMap {
              case Left(e) =>
                users.audit(
                  None,
                  "web",
                  "auth.denied",
                  None,
                  Some(ReqInfo.clientIp(req)),
                  Json.obj("error" -> e.getMessage.take(300).asJson)
                ) *>
                  err(ErrorCode.LoginFailed)
              case Right(id) =>
                // 確かめられていないメールアドレスでは、ほかのアカウントへの結び付けやメールボックスの自動作成をしない
                users.login(
                  id.sub,
                  id.name,
                  id.email.filter(_ => id.emailVerified),
                  id.picture,
                  cfg.autoCreateUsers
                ).flatMap {
                  case None => err(ErrorCode.LoginUserNotRegistered)
                  case Some(u) if u.disabled => err(ErrorCode.UserDisabled)
                  case Some(u) =>
                    users.syncGroups(u.id, id.groups) *> adminSvc.autoProvision(u) *> loggedIn(u, req, id.idToken)
                      .flatMap(c => redirect(back, Some(c)).map(_.addCookie(loginCookie("", 0))))
                }
            }
        }
      case (None, _, _) => err(ErrorCode.OidcNotConfigured)
      case _ => err(ErrorCode.LoginCodeStateRequired)
    }

  private def rejectLogoutWithoutCsrf(
    req: Request[IO]
  ): IO[Response[IO]] =
    err(ErrorCode.CsrfHeaderMissing, "X-Skereru ヘッダーが必要です")

  private def postAuthLogout(
    req: Request[IO]
  ): IO[Response[IO]] =
    sessions.revoke(req).flatMap {
      (
        idToken,
        c
      ) =>
        oidc.flatTraverse(_.logoutUri(idToken, s"${cfg.publicUrl.stripSuffix("/")}/")).flatMap { idp =>
          Ok(Json.obj("redirect" -> idp.fold("/")(_.renderString).asJson)).map(_.addCookie(c))
        }
    }

  private def postAuthDevLogin(
    req: Request[IO]
  ): IO[Response[IO]] =
    body[Json](req) { j =>
      val c = j.hcursor
      val name = c.get[String]("name").getOrElse("").trim
      val email = c.get[String]("email").toOption.map(_.trim.toLowerCase).filter(_.contains("@"))
      val admin = c.get[Boolean]("admin").getOrElse(false)
      if name.isEmpty then err(ErrorCode.NameRequired)
      else
        for {
          u0 <- users.devUser(name, email, admin)
          u = u0.copy(email = email.orElse(u0.email))
          _ <- adminSvc.autoProvision(u)
          ck <- loggedIn(u, req)
          r <-
            Ok(Json.obj("redirect" -> safeReturn(c.get[String]("returnTo").toOption).asJson)).map(_.addCookie(ck))
        } yield r
    }

  private def getClientConfig(): IO[Response[IO]] =
    Ok(ClientConfigDto(
      cfg.devLogin && oidc.isEmpty,
      cfg.version,
      mcfg.hostname,
      mcfg.imapPort,
      mcfg.imapsPort,
      mcfg.pop3Port,
      mcfg.pop3sPort,
      mcfg.submissionPort,
      mcfg.smtpsPort,
      (mcfg.maxMessageBytes / 1024 / 1024).toInt
    ))

  /** ログイン後に戻る先（同じサイトのパスだけ）。 */
  private def safeReturn(
    s: Option[String]
  ): String =
    s.filter(SafePath.isLocal).getOrElse("/")

  private def redirect(
    to: String,
    cookie: Option[ResponseCookie] = None
  ): IO[Response[IO]] = {
    val r = Response[IO](Status.Found).putHeaders(Location(Uri.unsafeFromString(to)))
    IO.pure(cookie.fold(r)(r.addCookie))
  }

  private def loginCookie(
    state: String,
    maxAge: Long
  ): ResponseCookie =
    ResponseCookie(
      cfg.loginCookieName,
      state,
      maxAge = Some(maxAge),
      path = Some("/"),
      httpOnly = true,
      secure = cfg.cookieSecure,
      sameSite = Some(SameSite.Lax)
    )

  /** callback に来た state が、このブラウザで始めたログインのものか（ほかの人のログインの続きを踏ませる「ログイン CSRF」を防ぐ）。 */
  private def sameBrowser(
    req: Request[IO],
    state: String
  ): Boolean =
    req.cookies.find(_.name == cfg.loginCookieName).exists(c =>
      java.security.MessageDigest.isEqual(c.content.getBytes("UTF-8"), state.getBytes("UTF-8"))
    )

  private def loggedIn(
    u: User,
    req: Request[IO],
    idToken: Option[String] = None
  ): IO[ResponseCookie] =
    sessions.issue(u, req, idToken).flatTap(_ =>
      users.audit(Some(u.id), "web", "auth.login", None, Some(ReqInfo.clientIp(req))) *>
        sbirds.observe.Metrics.count("skereru.logins", "via" -> "web")
    )

}
