package skereru.api

import skereru.core.ErrorCode

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import skereru.auth.*
import sbirds.http.{Csrf, Errors, OidcLoginFlow}
import skereru.db.*
import skereru.mail.MailConfig
import skereru.shared.*

import java.time.Instant

/** ログイン（OIDC の Authorization Code + PKCE・開発用のログイン）とログアウト。 */
final class AuthRoutes(oidc: Option[OidcClient],
                       users: UserStore,
                       adminSvc: AdminService,
                       val sessions: SessionAuth,
                       cfg: ApiConfig,
                       mcfg: MailConfig)
  extends RouteSupport {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / "auth" / "login" => getAuthLogin(req)
    case req @ GET -> Root / "auth" / "callback" => getAuthCallback(req)
    case req @ POST -> Root / "auth" / "logout" if !Csrf.fromOwnScreen(req, csrf) => rejectLogoutWithoutCsrf(req)
    case req @ POST -> Root / "auth" / "logout" => postAuthLogout(req)
    case req @ POST -> Root / "auth" / "dev-login" if cfg.devLogin && oidc.isEmpty => postAuthDevLogin(req)
    case GET -> Root / "api" / "client-config" => getClientConfig()
  }

  private def getAuthLogin(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.login(flow)(req)

  private def getAuthCallback(req: Request[IO]): IO[Response[IO]] = OidcLoginFlow.callback(flow)(req)

  /** OIDC のログインの流れ（sbirds.http.OidcLoginFlow）に差し込む、skereru の部分。 */
  private val flow = new OidcLoginFlow.Host[Identity] {
    val idp = oidc.map(o =>
      // skereru は自前の OidcClient（確かめたメールアドレスかどうかを見る）
      new OidcLoginFlow.Idp[Identity] {
        def authorizationUri(state: String, verifier: String, nonce: String): IO[Uri] =
          o.authorizationUri(state, verifier, nonce)
        def authenticate(code: String, verifier: String, nonce: String, now: Instant): IO[Identity] =
          o.authenticate(code, verifier, nonce, now)
      }
    )
    // state をこのブラウザの Cookie にも入れ、戻ってきたときに照合する（ほかの人のログインの続きを踏ませる「ログイン CSRF」を防ぐ）
    val cookie = OidcLoginFlow.StateCookie(cfg.loginCookieName, cfg.cookieSecure, path = "/")

    def returnTo(req: Request[IO]): String = safeReturn(param(req, "returnTo"))
    def saveState(state: String, verifier: String, nonce: String, returnTo: String): IO[Unit] =
      users.saveLoginState(state, verifier, nonce, returnTo)
    def takeState(state: String): IO[Option[(String, String, String)]] = users.takeLoginState(state)
    def redirect(to: String): IO[Response[IO]] = AuthRoutes.this.redirect(to)

    def withoutIdp(req: Request[IO], back: String): IO[Response[IO]] =
      if cfg.devLogin then AuthRoutes.this.redirect(s"/login?returnTo=${java.net.URLEncoder.encode(back, "UTF-8")}")
      else err(ErrorCode.OidcNotConfigured)

    def failed(req: Request[IO], f: OidcLoginFlow.Failure): IO[Response[IO]] = {
      import OidcLoginFlow.Failure.*
      // 詳しい理由は監査にだけ残し、画面には出さない
      def denied(e: Throwable) = users.audit(
        None,
        "web",
        "auth.denied",
        None,
        Some(ReqInfo.clientIp(req)),
        Json.obj("error" -> e.getMessage.take(300).asJson)
      ) *> err(ErrorCode.LoginFailed)
      f match {
        case NotConfigured => err(ErrorCode.OidcNotConfigured)
        case ParamsMissing => err(ErrorCode.LoginCodeStateRequired)
        case BrowserMismatch => err(ErrorCode.LoginBrowserMismatch)
        case Expired => err(ErrorCode.LoginExpired)
        case Denied(e) => denied(e)
        case Failed(e) => denied(e)
        case IdpError(e, _) => Errors.respondLogged(ErrorCode.LoginFailed, e.take(100), Some(req))
      }
    }

    def loggedIn(req: Request[IO], id: Identity, back: String): IO[Response[IO]] =
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
          users.syncGroups(u.id, id.groups) *> adminSvc.autoProvision(u) *>
            AuthRoutes.this.loggedIn(u, req, id.idToken).flatMap(c => AuthRoutes.this.redirect(back, Some(c)))
      }
  }

  private def rejectLogoutWithoutCsrf(req: Request[IO]): IO[Response[IO]] =
    err(ErrorCode.CsrfHeaderMissing, "X-Skereru ヘッダーが必要です")

  private def postAuthLogout(req: Request[IO]): IO[Response[IO]] =
    sessions.revoke(req).flatMap {
      (idToken, c) =>
        oidc.flatTraverse(_.logoutUri(idToken, s"${cfg.publicUrl.stripSuffix("/")}/")).flatMap { idp =>
          Ok(Json.obj("redirect" -> idp.fold("/")(_.renderString).asJson)).map(_.addCookie(c))
        }
    }

  private def postAuthDevLogin(req: Request[IO]): IO[Response[IO]] =
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
  private def safeReturn(s: Option[String]): String =
    sbirds.shared.SafePath.orElse(s)

  private def loggedIn(u: User, req: Request[IO], idToken: Option[String] = None): IO[ResponseCookie] =
    sessions.issue(u, req, idToken).flatTap(_ =>
      users.audit(Some(u.id), "web", "auth.login", None, Some(ReqInfo.clientIp(req))) *>
        sbirds.observe.Metrics.count("skereru.logins", "via" -> "web")
    )

}
